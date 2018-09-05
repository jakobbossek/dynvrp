context("Working with individuals")

test_that("Initialization produces valid individuals", {
  instance = getTestInstance()

  idx.mandatory = getMandatoryCustomers(instance) - 2L
  n.mandatory = length(idx.mandatory)

  # sample an initial tour
  n.fixed = sample(1:floor(n.mandatory / 2), size = 1L)
  init.tour = sample(idx.mandatory, size = n.fixed, replace = FALSE)

  # only mandatory customers
  ind = initIndividual(instance, init.tour = init.tour)
  expect_equal(n.mandatory, sum(ind$b))
  expect_equal(ind$n.mandatory, n.mandatory)

  # now with different fixed initial tours
  for (i in 1:100) {
    ind.mut = mutVRP(ind)
    mut.tour = getTourFromIndividual(ind.mut)
    # assert that the "initial part" of the tour is untouched!
    #FIXME: I hate this +/-2 stuff
    expect_true(all((init.tour + 2L) == mut.tour[seq_len(n.fixed)]))
  }

})
