#' @title Find Hamiltonian path in network
#'
#' @description Given a network instance, a set of nodes to consider
#' and designated start and end nodes the function heuristically determines
#' a shortest Hamiltonian path from start node to end node considering
#' all active nodes.
#'
#' @param instance [Network]\cr
#'   A netgen network.
#' @param active.nodes [\code{integer}]\cr
#'   Node numbers of nodes to be considered.
#' @param start.id [\code{integer(1)}]\cr
#'   Node number of start node.
#' @param dest.id [\code{integer(1)}]\cr
#'   Node number of destination node.
#' @return [\code{integer}]
#' @export
findHamiltonianPath = function(
  instance,
  active.nodes = seq_len(salesperson::getNumberOfNodes(instance) + 2L),
  start.id = 1L,
  dest.id = 2L) {

  # get information from source instance
  dist.mat = instance$dmat
  coords = instance$coords

  # active.nodes contains non-depot cities only.
  # Append start and end nodes.
  active.nodes2 = c(start.id, dest.id, active.nodes)

  # reduce distance matrix to considered nodes only
  dist.mat = dist.mat[active.nodes2, active.nodes2, drop = FALSE]

  # we later remove start and end depot (first two rows and columns respectively,
  # but we need the distances)
  dist.mat2 = dist.mat

  # reduce coordinates to active nodes only
  coords = coords[active.nodes2, , drop = FALSE]

  # depots are always stored as the first two cities/nodes in reduced distance matrix!
  # remove this nodes from the distance matrix and generate ATSP
  atsp = TSP::ATSP(dist.mat[-c(1L, 2L), -c(1L, 2L)])

  # insert dummy city with label sd (sd for start/dest) ...
  atsp = TSP::insert_dummy(atsp, label = "sd")
  sd.id = which(labels(atsp) == "sd")

  # ... and set outgoing distances from the dummy node to the
  # outgoing distances of the start node and the incoming distances of the
  # dummy node to the distances of the destination node
  atsp[sd.id, ] = c(dist.mat2[-c(1L, 2L), 1L], 0)
  atsp[, sd.id] = c(dist.mat2[2L, -c(1L, 2L)], 0)

  # reformulate stuff as an STSP problem, i.e., double the number of nodes
  stsp = TSP::reformulate_ATSP_as_TSP(atsp)

  # now prepare for TSP solver
  # FIXME: more documentation
  dmat2 = as.matrix(stsp)
  dmat3 = dmat2
  # EAX cannot handle infinite values. Thus, replace with biiig integer values
  dmat2[dmat2 > 1e8] = 10000
  dmat2[dmat2 < -1e8] = -10000
  dmat2 = floor(dmat2)
  n = nrow(dmat2)
  dummy.coords = matrix(runif(2 * n), ncol = 2)
  instance2 = netgen::makeNetwork(coordinates = dummy.coords, distance.matrix = dmat2, name = "dummy")

  # actually run solver
  res = salesperson::runSolver("eax", instance = instance2, solver.pars = list(full.matrix = TRUE, cutoff.time = 1L))
  #print(str(res))
  #instance2$distance.matrix = dmat3
  #print(salesperson::computeTourLength(instance2, res$tour, close.tour = TRUE))

  # extract ATSP tour from STSP SOLUTION
  atsp.tour = TSP::as.TOUR(res$tour[res$tour <= TSP::n_of_cities(atsp)])
  #print(str(atsp.tour))
  #print(TSP::tour_length(atsp.tour, atsp))

  # Add start and end tour
  # NOTE! NOTE! NOTE!
  # If the dummy nodes appear before and not after the original nodes
  # in the solution of the STSP, we need to reverse the tour! This is done
  # here simply be taking the shorter tour (reversed vs original).
  # (See paper about R package TSP by Hahsler and Hornik)
  cut.tour = TSP::cut_tour(atsp.tour, sd.id)
  print(sort(as.integer(cut.tour)))
  print(dim(dist.mat))
  h.path1 = c(2L, cut.tour + 2L, 1L)
  h.path2 = c(1L, cut.tour + 2L, 2L)

  # print(h.path1)
  # print(h.path2)

  instance2$distance.matrix = dist.mat
  h.path1.length = salesperson::computeTourLength(instance2, h.path1, close.tour = FALSE)
  h.path2.length = salesperson::computeTourLength(instance2, h.path2, close.tour = FALSE)
  # print(h.path1.length)
  # print(h.path2.length)

  instance3 = netgen::makeNetwork(coordinates = coords, name = "dummy2")

  # an = if (h.path1.length < h.path2.length) {
  #   active.nodes[rev(cut.tour)]
  # } else {
  #   active.nodes[cut.tour]
  # }

  print(start.id)
  p1 = autoplot(instance, path = c(start.id, active.nodes[rev(cut.tour)], dest.id), close.path = FALSE)
  p2 = autoplot(instance, path = c(start.id, active.nodes[cut.tour], dest.id), close.path = FALSE)
  gridExtra::grid.arrange(p1, p2, nrow = 1L)

  # stop("WORKS!")

  if (h.path1.length < h.path2.length) {
    return(active.nodes[rev(cut.tour)])
  }
  return(active.nodes[cut.tour])
}
