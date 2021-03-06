getMandatoryCustomers = function(network) {
  assertClass(network, "Network")

  if (is.null(network$arrival.times))
    BBmisc::stopf("Network has no arrival times.")

  ids = which(network$arrival.times == 0) + 2L
  return(ids)
}

toVRPInstance = function(instance) {
  instance$coords = rbind(instance$depot.coordinates, instance$coordinates)
  instance$dmat   = as.matrix(dist(instance$coords), method = "euclidean")
  return(instance)
}

#' Find tour prefix of already visited customers.
#'
#' @param ind [\code{VRPIndividual}]\cr
#'   Individual.
#' @param instance [\code{Network}]\cr
#'   Network object.
#' @param time.bound [\code{numeric(1)}]\cr
#'   Current time. This is relevant since the prefix tour is composed
#'   of all customers which already are served at a given point in time.
#' @return [\code{integer}]
findFixedTour = function(ind, instance, time.bound) {
  assertClass(ind, "VRPIndividual")
  assertClass(instance, "Network")
  assertNumber(time.bound, lower = 0)

  n = salesperson::getNumberOfNodes(instance)
  tour.length = 0

  fixed.tours = lapply(seq_len(ind$n.vehicles), function(v) {
    # get permutation without depots (only active customers)
    idx.tour = which(ind$b == 1L & ind$v == v)
    idx.tour = sort(which(ind$t %in% idx.tour))
    permutation = ind$t[idx.tour]

    # BBmisc::catf("BEFORE")
    # print(permutation)
    # # we need to reorder permutation if some parts are already visited
    if (length(ind$init.tours[[v]]) > 0L) {
      non.fixed = permutation[!(permutation %in% ind$init.tours[[v]])]
      permutation = c(ind$init.tours[[v]], non.fixed)
    }
    # BBmisc::catf("AFTER")
    # print(permutation)


    # shift customer IDs/positions (depots are encoded as customoers 1 and 2 in instance)
    permutation = permutation + 2L

    # append depots
    permutation = c(1L, permutation, 2L)

    # append release dates for depots
    release.dates = c(0, 0, instance$arrival.times)

    # now access release dates and distances
    k = length(permutation)
    i = 1L
    while (i < k & tour.length < time.bound) {
      start = permutation[i]
      end = permutation[i + 1L]
      tour.length = max(tour.length, release.dates[end]) + instance$dmat[start, end]
      i = i + 1L
    }

    fixed.tour = permutation[1:(i-1)]
    # drop start depot
    fixed.tour = fixed.tour[-1L]

    n.fixed.tour = length(fixed.tour)
    if (n.fixed.tour > 0L) {
      # drop end depot
      if (fixed.tour[n.fixed.tour] == 2L) {
        fixed.tour = fixed.tour[-n.fixed.tour]
      }

      # shift customer positions
      fixed.tour = fixed.tour - 2L
    }

    return(fixed.tour)
  })
  return(fixed.tours)
}
