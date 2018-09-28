context("EMOA")

test_that("EMOA produces reasonable and feasible results", {
  instance = getTestInstance()

  n.vehicles = 3L

  res = dynamicVRPEMOA(
    fitness.fun2,
    decision.fun = decideRank(1L, 0),
    instance = instance,
    mu = 10L, lambda = 5L,
    n.vehicles = n.vehicles,
    p.swap = 0.6,
    local.search.method = NULL,
    local.search.gens = c(100),
    init.keep = FALSE,
    stop.conds = list(ecr::stopOnIters(50L)),
    n.timeslots = 3L
  )

  n.eras = length(res$era.results)

  n.nodes = salesperson::getNumberOfNodes(instance)

  #for (i in seq_len(n.eras)) {
  # take final era
  the.res = res$era.results[[n.eras]]

  # check that all nodes in init tour are valid nodes
  expect_true(all(unlist(the.res$init.tours) %in% seq_len(n.nodes + 2L)))

  # check that init.tours are prefix of dm tours
  init.tours = the.res$init.tours
  dm.tours = getToursFromIndividual(the.res$dm.choice.ind, append.depots = TRUE)
  for (v in seq_len(n.vehicles)) {
    it = init.tours[[v]]
    nit = length(it)
    dm.tour = dm.tours[[v]]
    expect_true(all(it == dm.tour[1:nit]))
  }

  # check that dm.tours are all disjoint
  # first cut of leading 1 (first depot)
  dm.tours2 = lapply(dm.tours, function(tour) tour[-1L])
  # now check  if they are disoint
  k = 1L
  inters = logical()
  for (i in seq_len(n.vehicles)) {
    for (j in seq_len(n.vehicles)) {
      if (i == j) next
      inters = c(inters, length(intersect(dm.tours2[[i]], dm.tours2[[j]])) == 0)
    }
  }
  expect_true(!any(inters))

})
