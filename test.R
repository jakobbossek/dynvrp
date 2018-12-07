library(devtools)
library(ggplot2)
library(ecr)

load_all(".")

# Fitness function:
# - minimize maximal tour length traveled by a vehicle
# - minimize number of unserved requests
fitness.fun2 = function(ind, instance) {
  tour.length = sapply(seq_len(ind$n.vehicles), function(v) {
    computeTourLength(ind, instance, vehicle = v)
  })
  return(c(max(tour.length), ind$n.dynamic.inactive))
}

PATH.TO.INSTANCES = normalizePath("/Users/jboss/repositories/paper/orienteering_publications/PaperGecco2015/instances/publication/")

SOLVER.PATH = c("eax" = file.path("/Users/jboss/repositories/paper/tsp_publications/TSPAS/solvers/EAX/main"))
salesperson::solverPaths(as.list(SOLVER.PATH))

aposteriori = read.table("inst/results/aposteriori.csv", header = TRUE)

# get test instance
instance.path = file.path(PATH.TO.INSTANCES, " vrp_morphed_n100_mo25_dyn75_r1.csv")
inst = " vrp_morphed_n100_mo25_dyn75_r1.csv"

# EMOA test environment
debug = NA

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
instance = toVRPInstance(instance)

instance.apost = aposteriori

st = proc.time()
emoa.res2 = dynamicVRPEMOA(
  fitness.fun2,
  decision.fun = list(decideRank, decideRank, decideRank),
  decision.params = list(list(q = 0.1), list(q = 0.5), list(q = 0.8)),
  instance = instance,
  mu = 5L, lambda = 5L,
  n.vehicles = 1L,
  p.swap = 0.8,
  local.search.method = "eax",
  local.search.gens = c(10),
  init.keep = FALSE,
  time.resolution = 350L,
  stop.conds = list(ecr::stopOnIters(10L))
)
time.passed = proc.time() - st

# #stop("YAY!")
# current.era = length(fronts)

# the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
# pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

fronts = emoa.res2$pareto.front
tours = getListOfToursByEras(emoa.res2, eras = c(1, max(fronts$era)))

source("R/visualization.R")
pl = plotNetworkFancy(instance, time.resolution = 350L,
  tours = tours$dm.tours,
  init.tours = tours$init.tours)
print(pl)

stop("Done :)")
