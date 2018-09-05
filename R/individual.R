#' @title Initialize individual.
#'
#' @param instance [\code{\link[salesperson]{Network}}]\cr
#'   Network instance.
#' @param current.time [\code{numeric(1)}]\cr
#'   Current point in time.
#'   Default is 0.
#' @param init.tour [\code{integer}]\cr
#'   Fixed prefix tour, i.e., part of tour which is already fixed,
#'   because time passed and vehicle already visited some customers.
#' @return [\code{VRPIndividual}] List with following components:
#' \describe{
#'   \item[\code{b}]{Binary vector of length |V| - 2. b[i] is 1, if customer i is active, i.e., in tour.}
#'   \item[\code{t}]{Permutation vector.}
#'   \item[\code{p}]{Vector of mutation probablities. I.e., p[i] is the probability to flip b[i]. p[i] is zero if customer i is already fixed or not yet released.}
#'   \item[\code{it}]{Binary vector of length |V| - 2. it[i] is 1 if and only if customer i is in initial tour.}
#'   \item[n.mandatory \code{integer(1)}]{Number of mandatory customers.}
#'   \item[idx.dynamic.available \code{integer}]{IDs/positions of dynamic customers which already requested serving.}
#'   \item[n.dynamic.active \code{integer(1)}]{Number of active dynamic customers i (i.e., b[i] = 1)}
#'   \item[n.dynamic.inactive \code{integer(1)}]{Number of available, but not active dynamic customers i (i.e., b[i] = 0)}
#'   \item[init.tour \code{integer}]{Fixed tour part, i.e., sequence of nodes already visited.}
#' }
initIndividual = function(instance, current.time = 0, init.tour = integer()) {
  n = salesperson::getNumberOfNodes(instance)

  # mandatory customers: always b = 1 and p = 0, i.e., they are
  # active and cannot be "flipped away"
  n.mandatory = sum(instance$arrival.times == 0)
  idx.mandatory = which(instance$arrival.times == 0)

  # get all available dynamic customers (i.e., those that requested for service already)
  idx.dynamic.available = which(instance$arrival.times > 0 & instance$arrival.times <= current.time)
  n.dynamic.available = length(idx.dynamic.available)

  # which customers (both mandatory and available dynamic) are not yet visited
  not.yet.visited = setdiff(1:n, init.tour)

  # init individual
  ind = list(
    b = rep(0, n), # active?
    #FIXME: shuffle instead of sample
    t = c(init.tour, sample(not.yet.visited)), # tour permutation
    p = rep(0, n), # can be "flipped"?
    it = rep(0, n), # part of initial tour?
    # more meta info
    n.mandatory = n.mandatory,
    n.dynamic.available = n.dynamic.available,
    idx.dynamic.available = idx.dynamic.available,
    init.tour = init.tour
  )

  # all mandatory customers are active
  ind$b[idx.mandatory] = 1L

  #FIXME: might be zero
  #FIXME: is this smart way to set this one?
  p.denominator = n - n.mandatory - n.dynamic.available
  if (p.denominator == 0)
    p.denominator = 0.5

  #FIXME: maybe set p = 1 / n.dynamic.available?
  if (n.dynamic.available > 0L) {
    #ind$b[idx.dynamic.available] = sample(c(0, 1), size = n.dynamic.available, replace = TRUE, prob = c(0.02, 0.98))

    #FIXME: probs need to be 0.5
    ind$b[idx.dynamic.available] = sample(c(0, 1), size = n.dynamic.available, replace = TRUE, prob = c(0.5, 0.5))
    ind$p[idx.dynamic.available] = 1/n.dynamic.available
    #FIXME: here we assign 1.0 / 0 = Inf in some cases
    # ind$b[idx.dynamic.available] = sample(c(0, 1), size = n.dynamic.available, replace = TRUE, prob = c(0.5, 0.5))
    # ind$p[idx.dynamic.available] = 1.0 / (n - n.mandatory - n.dynamic.available)
  }

  # adapt individual if some customers are already visited
  # In this case we cannot flip those (even if they are dynamic)
  # and they need to be active
  if (length(init.tour) > 0L) {
    ind$b[init.tour] = 1L
    ind$p[init.tour] = 0L
    ind$it[init.tour] = 1L
  }

  ind$n.dynamic.active = sum(ind$b[idx.dynamic.available])
  ind$n.dynamic.inactive = ind$n.dynamic.available - ind$n.dynamic.active

  #DEBUG
  stopifnot(sum(ind$b) <= n.mandatory + n.dynamic.available)
  stopifnot(sum(ind$b) >= n.mandatory)
  stopifnot(sum(ind$it) == length(init.tour))

  class(ind) = "VRPIndividual"
  return(ind)
}

#' Simple printer for individuals.
#' @export
print.VRPIndividual = function(x, ...) {
  catf("#Active customers:         %i", sum(x$b))
  catf("#Active dynamic customers: %i", sum(x$b) - x$n.mandatory)
}

#' Construct tour from individual.
#' @export
getTourFromIndividual = function(ind, ...) {
  # get active customers
  idx.active = which(ind$b == 1L)
  idx.tour = sort(which(ind$t %in% idx.active))

  # +2 since we are 1-based in encoding and 1 and 2 are the depots
  tour = ind$t[idx.tour] + 2L
  return(tour)
}
