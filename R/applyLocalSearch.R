applyLocalSearch = function(ind, instance, more.args, ...) {
  active.nodes = which(ind$b == 1L)
  idx.tour = sort(which(ind$t %in% active.nodes))
#  print(idx.tour)

  print("------------------------")
  before = sort(ind$t)

  if (length(ind$init.tour) > 0L) {
    active.nodes = active.nodes[!(active.nodes %in% ind$init.tour)]
  }

  sort(active.nodes)

  # we need to consider the initially driven tour,
  # i.e., search for Hamiltionian path from last customer of
  # initial tour to end depot
  n.init = length(ind$init.tour)
  start.id = if (n.init == 0) 1L else ind$init.tour[n.init]

  ls.res = findHamiltonianPath(
    instance,
    active.nodes = active.nodes,
    start.id = start.id, dest.id = 2L)

  if (n.init > 0) {
    pl1 = autoplot(instance, path = c(1L, ind$init.tour))
    pl2 = autoplot(instance, path = c(ind$init.tour[n.init], ls.res, 2L))
    gridExtra::grid.arrange(pl1, pl2, nrow = 1L)
  } else {
    pl1 = autoplot(instance, path = c(1L, ls.res, 2L))
    print(pl1)
  }
  #BBmisc::pause()
  BBmisc::catf("#active: %i", sum(ind$b))
  BBmisc::catf("#init: %i", n.init)

  # if (n.init > 0) {
  #   print(str(ind))
  #   print(sum(ind$b))
  #   print(ind$n.mandatory)
  #   stop("LS finished")
  # }
  ind$t[idx.tour] = ls.res
  after = sort(ind$t)

  print(sort(ls.res))
  print(sort(active.nodes))
  #stop("djsadh")

  BBmisc::catf("Check: %i", all(before == after))
  BBmisc::catf("Before distinct: %i", length(unique(before)))
  BBmisc::catf("After distinct: %i", length(unique(after)))

  #print(ind$t)
  print("------------------------")

  #stop("DEBUG")
  return(ind)
}
