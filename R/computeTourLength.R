#' @title Helper for tour length computation.
#'
#' @param ind [\code{VRPIndividual}]\cr
#'   Instance of type \code{VRPIndividual}.
#' @param instance [\code{Network}\cr
#'   Netgen network with slots \dQuote{coords} and \dQuote{dmat}, i.e., coordinates
#'   and distance matrix with start and end depot!
#' @return [\code{numeric(1)}]
#' @seealso fitness.fun
computeTourLength = function(ind, instance) {
  assertClass(ind, "VRPIndividual")
  assertClass(instance, "Network")

  n = salesperson::getNumberOfNodes(instance)
  tour.length = 0

  # get permutation without depots (only active customers)
  idx.tour = which(ind$b == 1L)
  idx.tour = sort(which(ind$t %in% idx.tour))
  permutation = ind$t[idx.tour]
  #permutation = ind$t[ind$b == 1L]

  # we need to reorder permutation if some parts are already visited
  if (length(ind$init.tour) > 0L) {
    non.fixed = permutation[!(permutation %in% ind$init.tour)]
    permutation = c(ind$init.tour, non.fixed)
  }

  # shift customer IDs/positions (depots are encoded as customoers 1 and 2 in instance)
  permutation = permutation + 2L

  # append depots
  permutation = c(1L, permutation, 2L)

  # append release dates for depots
  release.dates = c(0, 0, instance$arrival.times)

  # now access release dates and distances
  k = length(permutation)
  for (i in 1:(k - 1L)) {
    start = permutation[i]
    end = permutation[i + 1L]
    tour.length = max(tour.length, release.dates[end]) + instance$dmat[start, end]
  }
  return(tour.length)
}
