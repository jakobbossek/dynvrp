#' @title Mutation operator.
#'
#' @description Performs two consecutive mutation steps:
#' 1) Mutate the binary string ind$b. I.e., dynamic, available, but not yet fixed customers
#' may be added or removed from customer list.
#' 2) Swap mutation on ind$b, i.e., reorder the customer permutation.
#'
#' @param ind [\code{VRPIndividual}]\cr
#'   Individual.
#' @param p.swap [\code{numeric(1)}]\cr
#'   Swap probability. Defaults to 1.
#' @return [\code{VRPIndividual}]
mutVRP = ecr::makeMutator(
  mutator = function(ind, p.swap = 1) {
    n = length(ind$b)

    # flip mutation (only dynamic customers may flip)
    do.flip = which(runif(n) < ind$p)
    if (length(do.flip) > 0L) {
      ind$b[do.flip] = 1 - ind$b[do.flip]
    }

    # now "swap" vehicles
    if (ind$n.vehicles > 1L & ind$n.not.visited > 0L) {
      # which are not visited yet
      idx.notvisited = which(ind$it != 1L)
      do.change.car = which(runif(ind$n.not.visited) < (1 / ind$n.not.visited))
      if (length(do.change.car) > 0L) {
        idx.to.change = idx.notvisited[do.change.car]
        ind$v[idx.to.change] = sample(seq_len(ind$n.vehicles), size = length(idx.to.change), replace = TRUE)
      }
    }

    # swap mutation (only active customers which are not yet fixed!)
    #idx.active.tour = condenseTour(ind, only.active = TRUE)
    idx.active.tour = which(ind$b == 1L & ind$it != 1L)
    if (length(idx.active.tour) >= 2L) {
      # mapping to tour vector
      idx.active.tour = sort(which(ind$t %in% idx.active.tour))
    }

    # otherwise swapping is senseless
    do.swap = runif(1L) < p.swap
    if (do.swap & (length(idx.active.tour) >= 2L)) {

      #FIXME: magic number
      for (i in 1:5) {
        swap.positions = sample(idx.active.tour, 2L, replace = FALSE)
        tmp = ind$t[swap.positions[1L]]
        ind$t[swap.positions[1L]] = ind$t[swap.positions[2L]]
        ind$t[swap.positions[2L]] = tmp
      }
    }

    # update individual meta-data
    ind$n.dynamic.active = sum(ind$b[ind$idx.dynamic.available])
    ind$n.dynamic.inactive = sum(ind$b[ind$idx.dynamic.available] == 0)

    return(ind)
  },
  supported = "custom"
)
