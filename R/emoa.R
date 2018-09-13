#' @title Evolutionary Multi-Objective Algorithm for dynamic routing of a vehicle.
#'
#' @param fitness.fun [\code{function(ind, instance, ...)}]\cr
#'   Fitness function depends on individuals (see docs for initIndividual),
#'   the problem instnace of type \code{\link[salesperson]{Network}} and
#'   optional further parameter (not used at the moment).
#' @param instance [\code{\link[salesperson]{Network}}]\cr
#'   Network object with exactly two depots and optional release dates.
#' @param time.resolution [\code{numeric(1)}]\cr
#'   Width of time windows.
#' @param n.timeslots [\code{integer(1)} | \code{NULL}]\cr
#'   Number of time steps.
#'   If \code{NULL} (default), the value is computed via (max. request time
#'   of dynamic customers of \code{instance} / time.resolution.
#' @param decision.fun [\code{function(fitness, ...)}]\cr
#'   Function used to make decision after each time slot.
#'   May be used for interactive decision or decision maker simulation (e.g.,
#'   always decide for lexicographic optimum regarding one of the objective functions).
#'   Default is \code{decideRandom}, i.e., random choice of solution.
#' @param do.pause [\code{logical(1)}]\cr
#'   Pause execution after each time slot?
#'   Default is \code{FALSE}.
#' @param init.keep [\code{logical(1)}]\cr
#'   Should individuals in eras \eqn{> 1} carry over information from last population?
#'   Default is \code{TRUE}.
#' @param local.search.method [\code{character(1)}]\cr
#'   Local search algorithm. Default is \code{NULL}, i.e., no
#'   local search at all.
#' @param local.search.gens [\code{numeric}]\cr
#'   Generations where local search should be applied.
#' @param stop.conds [\code{list[ecr_terminator]}]\cr
#'   List of stopping conditions for each internal EMOA run.
#'   Default is to stop after 100 generations.
#' @param mu [\code{integer(1)}]\cr
#'   Population size.
#'   Defaults to 50.
#' @param lambda [\code{integer(1)}]\cr
#'   Number of offspring.
#'   Default is \code{mu}.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [list] List of \code{ecr_result} objects.
#' @export
dynamicVRPEMOA = function(fitness.fun,
  instance,
  time.resolution = 100L,
  n.timeslots = NULL,
  decision.fun = dynvrp::decideRandom,
  do.pause = FALSE,
  init.keep = TRUE,
  local.search.method = NULL,
  local.search.gens = 100L,
  stop.conds = list(ecr::stopOnIters(100L)),
  mu = 50L,
  local.search.args = list(),
  lambda = mu,
  ...) {

  assertFunction(fitness.fun)
  assertFunction(decision.fun)
  assertNumber(time.resolution)
  assertList(stop.conds)
  assertChoice(local.search.method, choices = c("eax", "lkh"), null.ok = TRUE)

  mu = asInt(mu, lower = 5L)
  lambda = asInt(lambda, lower = 5L)


  # preprocessing
  max.time = max(instance$arrival.times)
  n.dynamic = sum(instance$arrival.times > 0)
  n.mandatory = sum(instance$arrival.times == 0)

  if (is.null(n.timeslots))
    n.timeslots = ceiling(max.time / time.resolution) + 1L
  n.timeslots = asInt(n.timeslots, lower = 1L)
  current.time = 0

  # init control object
  control = ecr::initECRControl(fitness.fun, n.objectives = 2L, minimize = c(TRUE, TRUE))
  control = ecr::registerECROperator(control, slot = "mutate", mutVRP)
  control = ecr::registerECROperator(control, slot = "selectForMating", ecr::selSimple)
  control = ecr::registerECROperator(control, slot = "selectForSurvival", ecr::selNondom)

  init.tour = integer()
  era.results = vector(mode = "list", length = n.timeslots)
  stop.object = NA

  for (era in seq_len(n.timeslots)) {
    catf("Starting era %i. Current time: %.2f", era, current.time)

    # init EMOA
    population = if (!init.keep | current.time == 0) {
      ecr::gen(initIndividual(instance, init.tour = init.tour, current.time = current.time), mu)
    } else {
      lapply(population, function(template.ind) {
        initIndividual(instance, init.tour = init.tour, current.time = current.time, template.ind = template.ind)
      })
    }
    fitness = ecr::evaluateFitness(control, population, instance = instance)

    log = ecr::initLogger(control,
      log.pop = TRUE)
    ecr::updateLogger(log, population, fitness = fitness, n.evals = mu)

    gen = 0L
    # EMOA loop
    while (TRUE) {

      # no recombination at the moment -> mutate with probability 1
      offspring = ecr::generateOffspring(control, population, fitness, lambda = lambda, p.mut = 1L)

      if (!is.null(local.search.method) & (gen %in% local.search.gens)) {
        BBmisc::catf("Applying local search ...")
        offspring = lapply(offspring, applyLocalSearch, instance = instance, more.args = local.search.args)
      }

      #offspring = ecr::mutate(control, population, p.mut = 1L)
      fitness.o = ecr::evaluateFitness(control, offspring, instance = instance)

      # (mu + lambda) strategy
      sel = ecr::replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
      population = sel$population
      fitness = sel$fitness

      ecr::updateLogger(log, population, fitness, n.evals = lambda)
      gen = gen + 1L

      stop.object = ecr:::doTerminate(log, stop.conds)
      if (length(stop.object) > 0L) {
        catf("Finished EMOA run: %s", stop.object$message)
        break
      }
    }

    # log statistics
    era.stats = ecr::getStatistics(log)
    era.stats$era = era

    era.results[[era]]$stats = era.stats

    # final LS polishing
    if (!is.null(local.search.method)) {
      BBmisc::catf("Polishing solutions.")
      population = lapply(population, applyLocalSearch, instance = instance, more.args = local.search.args)
      fitness = ecr::evaluateFitness(control, population, instance = instance)
    }

    front.approx = as.data.frame(t(fitness))
    colnames(front.approx) = c("f1", "f2")

    # here we "shift" the second objective considering ALL dynamic customers
    # This is needed for visualization within scatterplots with the ILP and a posteriori stuff.
    n.dynamic.available = sum(instance$arrival.times > 0 & instance$arrival.times <= current.time)
    idx.dynamic = which(instance$arrival.times > 0)
    n.dynamic.in.init.tour = length(which(init.tour %in% idx.dynamic))
    front.approx$f2shifted = n.dynamic - (n.dynamic.available - front.approx$f2)
    front.approx$era = era
    front.approx$t   = current.time
    front.approx$n.dynamic.available = n.dynamic.available
    front.approx$n.dyn = n.dynamic
    front.approx$n.mandatory = n.mandatory
    front.approx$n.dynamic.in.init.tour = n.dynamic.in.init.tour

    # log results
    era.results[[era]]$front = front.approx
    era.results[[era]]$init.tour = init.tour
    era.results[[era]]$result = ecr:::makeECRResult(control, log, population, fitness, stop.object)
    era.results[[era]]$current.time = current.time
    era.results[[era]]$n.dynamic.available = n.dynamic.available
    era.results[[era]]$n.dynamic = n.dynamic
    era.results[[era]]$n.mandatory = n.mandatory
    era.results[[era]]$n.dynamic.in.init.tour = n.dynamic.in.init.tour
    era.results[[era]]$time.passed = log$env$time.passed

    # update time
    current.time = current.time + time.resolution

    # decision maker (get index of selected solution)
    dm.choice = decision.fun(fitness)

    # get solution and fix initial tour
    dm.ind = population[[dm.choice]]
    #FIXME: run TSP solver on DM choice
    init.tour = findFixedTour(dm.ind, instance, time.bound = current.time)

    era.results[[era]]$dm.choice.idx = dm.choice
    era.results[[era]]$dm.choice.ind = dm.ind

    #debug <<- population

    catf("DM choice is: %i", dm.choice)
    catf("Decided for tour: %s", BBmisc::collapse(init.tour, sep = ", "))
    catf("Length of init tour: %i", length(init.tour))
    catf("Serving %i mandatory and %i dynamic requests.", dm.ind$n.mandatory, dm.ind$n.dynamic.active)

    # if (do.pause)
    #   BBmisc::pause()


  } # end era loop

  return(list(
    era.results = era.results
  ))
}
