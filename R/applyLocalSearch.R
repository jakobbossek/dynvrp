#' Given an individual and an instance, this function performs
#' local search (focused on tour length minimization) accounting
#' for partial tours which are already driven.
#'
#' @param ind [\code{VRPIndividual}]\cr
#'   Individual.
#' @param instance [\code{Network}]\cr
#'   Netgen network.
#' @param more.args [\code{list}]\cr
#'   Further parameter passed down to local search procedure.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return Modified individual.
applyLocalSearch = function(ind, instance, more.args, ...) {
  active.nodes = which(ind$b == 1L & ind$it != 1)
  idx.tour = sort(which(ind$t %in% active.nodes))

  # we need to consider the initially driven tour,
  # i.e., search for Hamiltionian path from last customer of
  # initial tour to end depot
  n.init = length(ind$init.tour)
  start.id = if (n.init == 0) 1L else ind$init.tour[n.init]

  ls.res = findHamiltonianPath(
    instance,
    active.nodes = sort(active.nodes) + 2L,
    start.id = start.id, dest.id = 2L)

  idx.mandatory = which(instance$arrival.times == 0)

  # nodes 1 and 2 are depots above. Hence, we need to recode to 1-based representation.
  ind$t[idx.tour] = ls.res - 2L

  return(ind)
}
