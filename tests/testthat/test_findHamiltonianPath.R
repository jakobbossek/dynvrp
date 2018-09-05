context("findHamiltonianPath")

test_that("computation of Hamiltonian Path in netgen network works", {
  instance = getTestInstance()

  idx.mandatory = getMandatoryCustomers(instance)

  res = dynvrp::findHamiltonianPath(
    instance,
    active.nodes = idx.mandatory,
    start.id = 1L,
    dest.id  = 2L
  )

  expect_true(setequal(idx.mandatory, res))
})
