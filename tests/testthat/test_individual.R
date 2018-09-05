context("Working with individuals")

test_that("Initialization produces valid individuals", {
  instance = getTestInstance()

  idx.mandatory = getMandatoryCustomers(instance) - 2L
  n.mandatory = length(idx.mandatory)

  # only mandatory customers
  ind = initIndividual(instance)
  expect_equal(n.mandatory, sum(ind$b))
  expect_equal(ind$n.mandatory, n.mandatory)

  # now with different fixed initial tours
  for (i in 1:100) {
    # sample an initial tour
    n.fixed = sample(1:floor(n.mandatory / 2), size = 1L)
    init.tour = sample(idx.mandatory, size = n.fixed, replace = FALSE)

    ind = initIndividual(instance, init.tour = init.tour)
    ind.tour = getTourFromIndividual(ind)
    expect_true(all((init.tour + 2L) == ind.tour[seq_len(n.fixed)]))
  }

  # now for the trickier part: check with dynamic customers
  current.time = 400
  idx.customers = which(instance$arrival.times <= current.time)
  n.customers = length(idx.customers)

  for (i in 1:100) {
    # sample an initial tour
    n.fixed = sample(1:floor(n.customers / 2), size = 1L)
    init.tour = sample(idx.customers, size = n.fixed, replace = FALSE)

    ind = initIndividual(instance, current.time = current.time, init.tour = init.tour)
    ind.tour = getTourFromIndividual(ind)
    expect_true(all((init.tour + 2L) == ind.tour[seq_len(n.fixed)]))
  }

})
