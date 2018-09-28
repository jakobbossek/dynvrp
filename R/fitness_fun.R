#' @title Fitness function for our dynamic VRP approach.
#'
#' @description We aim to minimize the tour length and the number of
#' unvisited customers with later requests in time. Note: tour
#' length depends not only on the edge costs, i.e., driving from
#' customer i to j, but on the release dates of j too. So given a tour
#' t1,t2,...,tk we end up with the following recursive formulation
#' f(t1,tj) = max(f(t1,t(j-1)), r[j]) + d[i, j].
#'
#' @param ind [\code{VRPIndividual}]\cr
#'   Encoding of individual.
#' @param instance [\code{\link[salesperson]{Network}}]
#'   Network instance.
#' @return [\code{numeric(2)}] Vector of tour length and number of
#' unvisited dynamic customers.
fitness.fun = function(ind, instance) {
  return(c(computeTourLength(ind, instance), ind$n.dynamic.inactive))
}

# Minimize the number of unserved customers and the maximum tour length of the vehicles.
fitness.fun2 = function(ind, instance) {
  tour.length = sapply(seq_len(ind$n.vehicles), function(v) {
    computeTourLength(ind, instance, vehicle = v)
  })
  return(c(max(tour.length), ind$n.dynamic.inactive))
}
