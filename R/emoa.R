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
#' @param local.search.method [\code{character(1)}]\cr
#'   Local search algorithm. Default is \code{NULL}, i.e., no
#'   local search at all.
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
dynamicVRPEMOA = function(fitness.fun,
  instance,
  time.resolution = 100L,
  n.timeslots = NULL,
  decision.fun = decideRandom,
  do.pause = FALSE,
  local.search.method = NULL,
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
  if (is.null(n.timeslots))
    n.timeslots = ceiling(max.time / time.resolution)
  n.timeslots = asInt(n.timeslots, lower = 1L)
  current.time = 0

  # init control object
  control = ecr::initECRControl(fitness.fun, n.objectives = 2L, minimize = c(TRUE, TRUE))
  control = ecr::registerECROperator(control, slot = "mutate", mutVRP)
  control = ecr::registerECROperator(control, slot = "selectForMating", ecr::selSimple)
  control = ecr::registerECROperator(control, slot = "selectForSurvival", ecr::selNondom)

  init.tour = integer()
  era.results = vector(mode = "list", length = n.timeslots)
  era.fronts  = vector(mode = "list", length = n.timeslots)
  era.decisions = integer(n.timeslots)
  era.logs = vector(mode = "list", length = n.timeslots)
  stop.object = NA

  for (era in seq_len(n.timeslots)) {
    catf("Starting era %i. Current time: %.2f", era, current.time)

    # init EMOA
    population = ecr::gen(initIndividual(instance, init.tour = init.tour, current.time = current.time), mu)
    fitness = ecr::evaluateFitness(control, population, instance = instance)

    log = ecr::initLogger(control,
      log.stats = list(fitness = list("HV" = list(
        fun = ecr::computeHV,
        pars = list(ref.point = c(10000, 10000))))))
    ecr::updateLogger(log, population, fitness = fitness, n.evals = mu)

    gen = 1L
    # EMOA loop
    while (TRUE) {

      # no recombination at the moment -> mutate with probability 1
      offspring = ecr::generateOffspring(control, population, fitness, lambda = lambda, p.mut = 1L)

      if (!is.null(local.search.method) & (gen %% 25 == 0)) {
        BBmisc::catf("Applying local search")
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
    stats = ecr::getStatistics(log)
    stats$era = era
    era.logs[[era]] = stats

    front.approx = as.data.frame(t(fitness))
    colnames(front.approx) = c("f1", "f2")
    front.approx$era = era

    era.fronts[[era]] = front.approx

    era.results[[era]]$front = front.approx
    era.results[[era]]$init.tour = init.tour
    era.results[[era]]$result = ecr:::makeECRResult(control, log, population, fitness, stop.object)

    current.time = current.time + time.resolution

    # decision maker (get index of selected solution)
    dm.choice = decision.fun(fitness)
    era.decisions[era] = dm.choice

    # get solution and fix initial tour
    ind = population[[dm.choice]]
    #FIXME: run TSP solver on DM choice
    init.tour = findFixedTour(ind, instance, time.bound = current.time)

    #debug <<- population

    catf("DM choice is: %i", dm.choice)
    catf("Decided for tour: %s", BBmisc::collapse(init.tour, sep = ", "))
    catf("Length of init tour: %i", length(init.tour))
    catf("Serving %i mandatory and %i dynamic requests.", ind$n.mandatory, ind$n.dynamic.active)

    # if (do.pause)
    #   BBmisc::pause()


  } # end era loop

  return(list(
    era.results = era.results,
    era.decisions = era.decisions,
    era.logs = do.call(rbind, era.logs)
  ))
}
