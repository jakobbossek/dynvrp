#set.seed(1)

SOLVER.PATH = c("eax" = file.path("/Users/jboss/repositories/paper/tsp_publications/TSPAS/solvers/EAX/main"))
salesperson::solverPaths(as.list(SOLVER.PATH))

getTestInstance = function() {
  testdata = system.file("testdata", package = "dynvrp")
  #FIXME: leading blank in filenames
  fn = paste0(testdata, "/ vrp_morphed_n100_mo25_dyn75_r1.csv")
  instance = salesperson::importFromFile(fn)
  toVRPInstance(instance)
}
