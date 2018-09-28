applyLocalSearch = function(ind, instance, more.args, ...) {
  for (v in seq_len(ind$n.vehicles)) {
    active.nodes = which(ind$b == 1L & ind$it != 1 & ind$v == v)
    idx.tour = sort(which(ind$t %in% active.nodes))

    # we need to consider the initially driven tour,
    # i.e., search for Hamiltionian path from last customer of
    # initial tour to end depot
    n.init = length(ind$init.tours[[v]])
    start.id = if (n.init == 0) 1L else (ind$init.tours[[v]][n.init] + 2L) # FIXME: do we need to add 2 to the latter!?

    ls.res = findHamiltonianPath(
      instance,
      active.nodes = sort(active.nodes) + 2L,
      start.id = start.id, dest.id = 2L)

    #idx.mandatory = which(instance$arrival.times == 0)
    # if (n.init > 0) {
    #   pl1 = autoplot(instance, path = c(1L, ind$init.tour))
    #   pl1 = pl1 + geom_point(data = as.data.frame(instance$coordinates[idx.mandatory, , drop = FALSE]), colour = "tomato")
    #   pl2 = autoplot(instance, path = c(ind$init.tour[n.init], ls.res, 2L))
    #   pl2 = pl2 + geom_point(data = as.data.frame(instance$coordinates[idx.mandatory, , drop = FALSE]), colour = "tomato")
    #   gridExtra::grid.arrange(pl1, pl2, nrow = 1L)
    # } else {
    #   pl1 = autoplot(instance, path = c(1L, ls.res, 2L))
    #   pl1 = pl1 + geom_point(data = as.data.frame(instance$coordinates[idx.mandatory, , drop = FALSE]), colour = "tomato")
    #   print(pl1)
    # }
    # BBmisc::pause()

    # nodes 1 and 2 are depots above. Hence, we need to recode to 1-based representation.
    ind$t[idx.tour] = ls.res - 2L
  }
  return(ind)
}
