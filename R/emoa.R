#' @title Evolutionary Multi-Objective Algorithm for dynamic routing.
#'
#' @description EMOA designed to tackle a challenging multi-objective vehicle
#' routing problem, where the goal is to minimize tour length(s) of one or
#' multiple vehicles. Simultaneously, the number of unvisited customers which
#' request for service as time passes by is to be minimized.
#'
#' @param fitness.fun [\code{function(ind, instance, ...)}]\cr
#'   Fitness function depends on individuals (see docs for initIndividual),
#'   the problem instance of type \code{Network} and
#'   optional further parameter (not used at the moment).
#' @param instance [\code{Network}]\cr
#'   Network object with exactly two depots and optional release dates.
#' @param time.resolution [\code{numeric(1)}]\cr
#'   Width of time windows.
#'   Default is 100.
#' @param n.vehicles [\code{integer(1)}]\cr
#'   The number of vehicles.
#'   Default is 1.
#' @param decision.fun [\code{function(fitness, ...)}]\cr
#'   Function used to make decision after each time slot.
#'   May be used for interactive decision or decision maker simulation (e.g.,
#'   always decide for lexicographic optimum regarding one of the objective functions).
#'   Default is \code{decideRandom}, i.e., random choice of solution.
#'   If a vector of functions is passed the i-th function is used to make a choice
#'   in the i-th era.
#' @param decision.params [\code{list | NULL}]\cr
#'   List of named lists of parameter choices for \code{decision.fun}.
#'   The i-the list is passed down to the i-th decision.fun in the i-th era.
#' @param do.pause [\code{logical(1)}]\cr
#'   Pause execution after each time slot?
#'   Default is \code{FALSE}.
#' @param p.swap [\code{numeric(1)}]\cr
#'   Probability for swap mutation.
#'   Default is 1.
#' @param init.keep [\code{logical(1)}]\cr
#'   Should individuals in eras \eqn{> 1} carry over information from last population?
#'   Default is \code{TRUE}.
#' @param local.search.method [\code{character(1)}]\cr
#'   Local search algorithm. Default is \code{NULL}, i.e., no
#'   local search at all.
#' @param local.search.gens [\code{numeric}]\cr
#'   Generations where local search should be applied.
#' @param local.search.args [\code{list}]\cr
#'   List of arguments for local search algorithm.
#'   Defaults to empty list.
#' @param init.distribution [\code{character(1)}]\cr
#'   How shall available dynamic customers be sampled?
#'   Option \dQuote{binomial}: each dynamic available customer is active with probability \eqn{0.5}
#'   independently.
#'   Option \dQuote{uniform}: if there are \eqn{n_d} available dynamic customers,
#'   we have \eqn{P(X = i) = \frac{1}{n_d}} for \eqn{i \in \{1, \ldots, n_d\}}. In a second step
#'   \eqn{i} positions are sampled at random.
#' @param stop.conds [\code{list[ecr_terminator]}]\cr
#'   List of stopping conditions for each internal EMOA run.
#'   Default is to stop after 100 generations.
#' @param mu [\code{integer(1)}]\cr
#'   Population size.
#'   Defaults to 50.
#' @param lambda [\code{integer(1)}]\cr
#'   Number of offspring.
#'   Default is \code{mu}.
#' @param seed [\code{integer} | \code{NULL}]\cr
#'   Optional seed for run.
#'   Set outside the algorithm by default.
#' @param aposteriori [\code{logical(1)}]\cr
#'   Treat the problem from an a posteriori perspective, i.e., perform just a single run of the EMOA,
#'   but give him oracle knowledge on the request times of dynamic requests.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{list}] List with the following components:
#' \describe{
#'  \item{era.results}{Fine grained results. Use \code{str} to get insights.}
#'  \item{pareto.front}{Data frame with columns
#'   \describe{
#'     \item{\dQuote{f1} [\code{numeric}]}{Tour length.}
#'     \item{\dQuote{f2} [\code{integer}]}{Normalized number of unvisited customers.}
#'     \item{\dQuote{f2shifted} [\code{integer}]}{Unnormalized number of unvisited customers.},
#'     \item{\dQuote{era} [\code{integer}]}{Corresponding era.}
#'     \item{\dQuote{selected} [\code{logical}]}{Indicates whether the solution was selected by the decision maker.}
#'   }}
#' \item{populations}{Data frame with objective vectors of all populations over all generations. Contains components
#'   \describe{
#'     \item{\dQuote{f1} [\code{numeric}]}{tour length}
#'     \item{\dQuote{f2} [\code{integer}]}{(normalized tour length)}
#'     \item{\dQuote{f2shifted} [\code{integer}]}{unnormalized number of unvisited customers.},
#'     \item{\dQuote{era} [\code{integer}]}{Corresponding era.}
#'     \item{\dQuote{gen} [\code{integer}]}{Generation.}
#'   }}
#' \item{meta}{Data frame with additional meta information per era, namely
#'   \describe{
#'     \item{\dQuote{era} [\code{integer}]}{Corresponding era.}
#'     \item{\dQuote{current.time} [\code{numeric}]}{Time passed so far.}
#'     \item{\dQuote{n.mandatory} [\code{integer}]}{Number of mandatory customers.}
#'     \item{\dQuote{n.dynamic} [\code{integer}]}{Total number of dynamic customers.}
#'     \item{\dQuote{n.dynamic.available} [\code{integer}]}{Number of available dynamic customers, i.e., those who already requested for service.}
#'     \item{\dQuote{n.dynamic.upper.bound} [\code{integer}]}{Upper bound for the number of unserved dynamic customers (note, that a partial tour is already traveled.) in normalized space, i.e., the space used in the last era.}
#'     \item{\dQuote{n.dynamic.lower.bound} [\code{integer}]}{Lower bound for the number of unserved dynamic customers (depends on the time and the number of already arrived dynamic requests.}
#'     \item{\dQuote{n.dynamic.in.init.tour} [\code{integer}]}{Number of dynamic customers already visited.}
#'     \item{\dQuote{time.passed} [\code{numeric}]}{Time passed during the start of the optimization process.}
#'     \item{\dQuote{init.tour} [\code{character}]}{Comma-separated partial, already traveled tour.}
#'     \item{\dQuote{dm.tour} [\code{character}]}{Comma-separated sequence of customers in tour selected by decision maker in the corresponding era.}
#'   }}
#' }
#' @export
dynamicVRPEMOA = function(fitness.fun,
  instance,
  time.resolution = 100L,
  n.timeslots = NULL,
  n.vehicles = 1L,
  decision.fun = dynvrp::decideRandom,
  decision.params = list(),
  do.pause = FALSE,
  p.swap = 1,
  init.keep = TRUE,
  local.search.method = NULL,
  local.search.gens = 100L,
  init.distribution = "uniform",
  stop.conds = list(ecr::stopOnIters(100L)),
  mu = 50L,
  local.search.args = list(on.ls.failure = "warn"),
  lambda = mu,
  seed = NULL,
  aposteriori = FALSE,
  ...) {

  if (!is.null(seed))
    set.seed(seed)

  assertFunction(fitness.fun)
  assertList(stop.conds)
  assertNumber(p.swap, lower = 0,, upper = 1)
  assertChoice(local.search.method, choices = c("eax", "lkh"), null.ok = TRUE)
  assertChoice(init.distribution, choices = c("binomial", "uniform"), null.ok = FALSE)
  assertFlag(aposteriori)

  # if (init.keep & n.vehicles > 1L)
  #   BBmisc::stopf("[dynamicVRPEMOA] Currently init.keep is not supported if >1 vehicles are available.")

  if (!is.null(n.timeslots) & !is.null(time.resolution))
    BBmisc::stopf("[dynamicVRPEMOA] Both n.timeslots and time.resolution passed.")

  n.vehicles = asInt(n.vehicles, lower = 1L)
  mu = asInt(mu, lower = 5L)
  lambda = asInt(lambda, lower = 5L)

  # preprocessing
  max.time = max(instance$arrival.times)
  n.dynamic = sum(instance$arrival.times > 0)
  n.mandatory = sum(instance$arrival.times == 0)

  if (!is.null(time.resolution)) {
    n.timeslots = ceiling(max.time / time.resolution) + 1L
  } else {
    time.resolution = ceiling(max.time / (n.timeslots - 2))
  }

  BBmisc::messagef("[dynamicVRPEMOA] Running for %i eras with time resolution: %.3f.", n.timeslots, time.resolution)

  assertNumber(time.resolution)

  if (is.list(decision.fun)) {
    if (length(decision.fun) != n.timeslots) {
      BBmisc::messagef("[dynamicVRPEMOA] Running for %i timeslots, but only %i decision functions passed.", n.timeslots, length(decision.fun))
    }
    #checkmate::assertList(decision.fun, types = "function")

    if (!is.list(decision.params)) {
      BBmisc::messagef("[dynamicVRPEMOA] decision.params need to be a list of length n.timeslots.", n.timeslots)
    }
  }

  current.time = 0

  # init control object
  control = ecr::initECRControl(fitness.fun, n.objectives = 2L, minimize = c(TRUE, TRUE))
  control = ecr::registerECROperator(control, slot = "mutate", mutVRP, p.swap = p.swap)
  control = ecr::registerECROperator(control, slot = "selectForMating", ecr::selSimple)
  control = ecr::registerECROperator(control, slot = "selectForSurvival", ecr::selNondom)

  # initial tours empty at the beginning
  init.tours = lapply(seq_len(n.vehicles), function(i) integer())

  # allocate vector for individual results per era
  era.results = vector(mode = "list", length = n.timeslots)
  stop.object = NA

  # ERA/EPOCH loop
  for (era in seq_len(n.timeslots)) {
    catf("Starting era %i. Current time: %.2f", era, current.time)

    # init EMOA
    population = if (!init.keep | current.time == 0) {
      # in a posteriori approach we pass a dummy time larger than the latest request time. This way
      # all dyanmic customers are available to the algorithm.
      current.time2 = if (aposteriori) max.time + 1 else current.time
      ecr::gen(initIndividual(instance, init.tours = init.tours, current.time = current.time2, init.distribution = init.distribution, n.vehicles = n.vehicles), mu)
    } else {
      lapply(population, function(template.ind) {
        initIndividual(instance, init.tours = init.tours, current.time = current.time, init.distribution = init.distribution, n.vehicles = n.vehicles, template.ind = template.ind)
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
    } # END OF EMOA LOOP
    cat(".\n")

    # final LS polishing
    if (!is.null(local.search.method)) {
      BBmisc::catf("Polishing solutions.")
      population = lapply(population, applyLocalSearch, instance = instance, more.args = local.search.args)
      fitness = ecr::evaluateFitness(control, population, instance = instance)
    }

    # filter non-dominated points
    idx.nondom = ecr::which.nondominated(fitness)
    nondom.fitness = fitness[, idx.nondom, drop = FALSE]
    nondom.population = population[idx.nondom]

    front.approx = as.data.frame(t(nondom.fitness))
    colnames(front.approx) = c("f1", "f2")

    # here we "shift" the second objective considering ALL dynamic customers
    # This is needed for visualization within scatterplots with the ILP and a posteriori stuff.
    n.dynamic.available = sum(instance$arrival.times > 0 & instance$arrival.times <= current.time)
    idx.dynamic = which(instance$arrival.times > 0)

    # go through all initial tours and count the number of dynamic
    #FIXME: this can be done more efficiently!
    n.dynamic.in.init.tour = sum(sapply(seq_len(n.vehicles), function(v) {
      length(which(init.tours[[v]] %in% idx.dynamic))
    }))
    front.approx$f2shifted = n.dynamic - (n.dynamic.available - front.approx$f2)
    front.approx$era = era

    # log results
    era.results[[era]]$front = front.approx
    era.results[[era]]$init.tours = lapply(init.tours, function(it) c(1, it + 2L))
    era.results[[era]]$result = ecr:::makeECRResult(control, log, population, fitness, stop.object)

    # quit here after "first and single era" if a posteriori approach is used
    if (aposteriori) {
      # correction
      front.approx$f2shifted = front.approx$f2
      front.approx$f2shifted = front.approx$era = NULL
      return(front.approx)
    }

    # update time
    current.time = current.time + time.resolution

    # select decision fun
    dm.fun = decision.fun
    dm.params = decision.params
    if (is.list(decision.fun)) {
      dm.fun = decision.fun[[era]]
      dm.params = decision.params[[era]]
    }

    # decision maker (get index of selected solution)
    dm.params$fitness = nondom.fitness
    dm.choice = do.call(dm.fun, args = dm.params)

    # get solution and fix initial tour
    dm.ind = nondom.population[[dm.choice]]
    #FIXME: run TSP solver on DM choice
    init.tours = findFixedTour(dm.ind, instance, time.bound = current.time)

    era.results[[era]]$dm.choice.idx = dm.choice
    era.results[[era]]$dm.choice.ind = dm.ind

    front.approx$selected = FALSE
    front.approx$selected[dm.choice] = TRUE
    era.results[[era]]$front = front.approx

    #print(init.tours)

    #print(getToursFromIndividual(dm.ind, append.depots = TRUE))

    era.results[[era]]$meta = data.frame(
      era = era,
      time.resolution = time.resolution,
      n.vehicles = n.vehicles,
      current.time = current.time,
      n.mandatory = n.mandatory,
      n.dynamic = n.dynamic,
      n.dynamic.available = n.dynamic.available,
      n.dynamic.upper.bound = n.dynamic - n.dynamic.in.init.tour,
      n.dynamic.lower.bound = n.dynamic - n.dynamic.available,
      n.dynamic.in.init.tour = n.dynamic.in.init.tour,
      time.passed = log$env$time.passed,
      init.tour = toursToString(getInitToursFromIndividual(dm.ind, append.depot = TRUE)),
      # init.tour = BBmisc::collapse(c(1, getInitToursFromIndividual(dm.ind, append.depots = FALSE)[[1L]])),
      dm.tour = toursToString(getToursFromIndividual(dm.ind, append.depots = TRUE)),
      stringsAsFactors = FALSE
    )
    #debug <<- population

    # population2 = ecr::getPopulations(log)
    # gg = length(population2)
    # population2 = lapply(seq_len(gg), function(i) {
    #   ff = as.data.frame(t(population2[[i]]$fitness))
    #   colnames(ff) = c("f1", "f2")
    #   ff$f2shifted = n.dynamic - (n.dynamic.available - ff$f2)
    #   ff$era = era
    #   ff$gen = i
    #   return(ff)
    # })
    # era.results[[era]]$population = do.call(rbind, population2)

    catf("DM choice is: %i", dm.choice)
    #catf("Decided for tour: %s", BBmisc::collapse(init.tour, sep = ", "))
    catf("Length of init tour: %i", length(init.tours))
    catf("Serving %i mandatory and %i dynamic requests.", dm.ind$n.mandatory, dm.ind$n.dynamic.active)

    if (do.pause)
      BBmisc::pause()
  } # end era loop

  pareto.front = do.call(rbind, lapply(era.results, function(er) er$front))
  meta = do.call(rbind, lapply(era.results, function(er) er$meta))
  #populations = do.call(rbind, lapply(era.results, function(er) er$population))

  return(list(
    era.results = era.results,
    pareto.front = pareto.front,
    meta = meta
    #populations = populations
  ))
}

