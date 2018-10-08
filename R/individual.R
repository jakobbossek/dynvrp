#' @title Initialize individual.
#'
#' @param instance [\code{\link[salesperson]{Network}}]\cr
#'   Network instance.
#' @param current.time [\code{numeric(1)}]\cr
#'   Current point in time.
#'   Default is 0.
#' @param init.tours [\code{integer}]\cr
#'   List of fixed prefix tours, i.e., part of a vehicles tour which is already fixed,
#'   because time passed and vehicle already visited some customers.
#' @param n.vehicles [\code{integer(1)}]\cr
#'   Number of vehicles.
#'   Defaults to 1.
#' @param template.ind [\code{integer}]\cr
#'   Tour used as a \dQuote{template} for a newly generated individual. Here,
#'   we aim to pass as much information from \code{template.tour} as possible.
#' @param init.distribution [\code{character(1)}]\cr
#'   How shall available dynamic customers be sampled?
#'   Option \dQuote{binomial}: each dynamic available customer is active with probability \eqn{0.5}
#'   independently.
#'   Option \dQuote{uniform}: if there are \eqn{n_d} available dynamic customers,
#'   we have \eqn{P(X = i) = \frac{1}{n_d}} for \eqn{i \in \{1, \ldots, n_d\}}. In a second step
#'   \eqn{i} positions are sampled at random.
#' @return [\code{VRPIndividual}] List with following components:
#' \describe{
#'   \item{\code{b}}{Binary vector of length |V| - 2. b[i] is 1, if customer i is active, i.e., in tour.}
#'   \item{\code{t}}{Permutation vector.}
#'   \item{\code{p}}{Vector of mutation probablities. I.e., p[i] is the probability to flip b[i]. p[i] is zero if customer i is already fixed or not yet released.}
#'   \item{\code{it}}{Binary vector of length |V| - 2. it[i] is 1 if and only if customer i is in initial tour.}
#'   \item{n.mandatory \code{integer(1)}}{Number of mandatory customers.}
#'   \item{idx.dynamic.available \code{integer}}{IDs/positions of dynamic customers which already requested serving.}
#'   \item{n.dynamic.active \code{integer(1)}}{Number of active dynamic customers i (i.e., b[i] = 1)}
#'   \item{n.dynamic.inactive \code{integer(1)}}{Number of available, but not active dynamic customers i (i.e., b[i] = 0)}
#'   \item{init.tours \code{integer}}{Fixed tour part, i.e., sequence of nodes already visited.}
#' }
initIndividual = function(instance, current.time = 0, init.tours = integer(), n.vehicles = 1L, template.ind = NULL, init.distribution = "binomial") {
  checkmate::assertChoice(init.distribution, choices = c("binomial", "uniform"))
  n = salesperson::getNumberOfNodes(instance)

  # mandatory customers: always b = 1 and p = 0, i.e., they are
  # active and cannot be "flipped away"
  n.mandatory = sum(instance$arrival.times == 0)
  idx.mandatory = which(instance$arrival.times == 0)

  # get all available dynamic customers (i.e., those that requested for service already)
  idx.dynamic.available = which(instance$arrival.times > 0 & instance$arrival.times <= current.time)
  n.dynamic.available = length(idx.dynamic.available)

  # which customers (both mandatory and available dynamic) are not yet visited
  yet.visited = unlist(init.tours)
  not.yet.visited = setdiff(1:n, yet.visited)

  # init individual
  ind = list(
    b = rep(0, n), # active?
    v = rep(1, n), # all served by first car
    #FIXME: shuffle instead of sample
    t = c(yet.visited, sample(not.yet.visited)), # tour permutation
    p = rep(0, n), # can be "flipped"?
    it = rep(0, n), # part of initial tour?
    # more meta info
    n.mandatory = n.mandatory,
    n.dynamic.available = n.dynamic.available,
    idx.dynamic.available = idx.dynamic.available,
    init.tours = init.tours,
    n.vehicles = n.vehicles
  )

  # all mandatory customers are active
  ind$b[idx.mandatory] = 1L
  ind$v[idx.mandatory] = sample(seq_len(n.vehicles), size = n.mandatory, replace = TRUE)

  #FIXME: maybe set p = 1 / n.dynamic.available?
  if (n.dynamic.available > 0L) {
    #FIXME: probs need to be 0.5
    if (init.distribution == "binomial") {
      ind$b[idx.dynamic.available] = sample(c(0, 1), size = n.dynamic.available, replace = TRUE, prob = c(0.5, 0.5))
    } else if (init.distribution == "uniform") {
      # how many dynamic customers shall be active?
      n.to.activate = sample(c(0, seq_len(n.dynamic.available)), size = 1L)
      if (n.to.activate > 0L) {
        idx.to.activate = sample(idx.dynamic.available, size = n.to.activate)
        ind$b[idx.to.activate] = 1L
      }
    }
    ind$v[idx.dynamic.available] = sample(seq_len(n.vehicles), size = n.dynamic.available, replace = TRUE)
    ind$p[idx.dynamic.available] = 1/n.dynamic.available
  }

  # adapt individual if some customers are already visited
  # In this case we cannot flip those (even if they are dynamic)
  # and they need to be active
  if (length(yet.visited) > 0L) {
    ind$b[yet.visited] = 1L
    ind$p[yet.visited] = 0L
    ind$it[yet.visited] = 1L
    for (v in seq_len(n.vehicles)) {
      ind$v[init.tours[[v]]] = v
    }
  }

  #FIXME: use only nondominated solutions?
  # now adapt tour if template is passed
  if (!is.null(template.ind)) {
    # carry over all active nodes of template
    idx.template.active = which(template.ind$b == 1L)
    ind$b[idx.template.active] = 1L

    # assure that active nodes are in the order they occured in template
    active.nodes = which(template.ind$b == 1 & ind$it != 1)
    idx.tour = which(ind$t %in% active.nodes)
    ind$t[idx.tour] = active.nodes
  }

  ind$n.dynamic.active = sum(ind$b[idx.dynamic.available])
  ind$n.dynamic.inactive = ind$n.dynamic.available - ind$n.dynamic.active

  #DEBUG
  stopifnot(sum(ind$b) <= n.mandatory + n.dynamic.available)
  stopifnot(sum(ind$b) >= n.mandatory)
  stopifnot(all(ind$v >= 1 & ind$v <= n.vehicles))
  stopifnot(sum(ind$it) == length(yet.visited))

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
getToursFromIndividual = function(ind, append.depots = FALSE, ...) {
  # get active customers
  lapply(seq_len(ind$n.vehicles), function(v) {
    idx.active = which(ind$b == 1L & ind$v == v)
    idx.tour = sort(which(ind$t %in% idx.active))
    tour = ind$t[idx.tour]

    # we need to reorder permutation if some parts are already visited
    if (length(ind$init.tours[[v]]) > 0L) {
      non.fixed = tour[!(tour %in% ind$init.tours[[v]])]
      tour = c(ind$init.tours[[v]], non.fixed)
    }

    # +2 since we are 1-based in encoding and 1 and 2 are the depots
    tour = tour + 2L

    if (append.depots) {
      tour = c(1L, tour, 2L)
    }
    return(tour)
  })
}

getInitToursFromIndividual = function(ind, append.depot = FALSE, ...) {
  lapply(ind$init.tours, function(it) {
    it = it + 2L
    if (append.depot) {
      it = c(1L, it)
    }
    return(it)
  })
}
