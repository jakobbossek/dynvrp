library(devtools)
library(ggplot2)
library(ecr)

load_all(".")

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

instance.path = file.path(PATH.TO.INSTANCES, " vrp_morphed_n100_mo25_dyn75_r1.csv")
inst = " vrp_morphed_n100_mo25_dyn75_r1.csv"

# EMOA test environment
debug = NA

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
#FIXME: handle in salesperson package
instance$coords = rbind(instance$depot.coordinates, instance$coordinates)
instance$dmat   = as.matrix(dist(instance$coords), method = "euclidean")

instance.apost = aposteriori

st = proc.time()
emoa.res2 = dynamicVRPEMOA(
  fitness.fun2, decision.fun = decideRank(1L, 0), instance = instance,
  mu = 10L, lambda = 5L,
  n.vehicles = 3L,
  p.swap = 0.8,
  local.search.method = "eax",
  local.search.gens = c(100),
  init.keep = FALSE,
  stop.conds = list(ecr::stopOnIters(100)),
  n.timeslots = 3L
  )
time.passed = proc.time() - st

fronts = emoa.res2$pareto.front

# #stop("YAY!")
# current.era = length(fronts)

# the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
# pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

max.era = max(fronts$era)

dm.ind = emoa.res2$era.results[[max.era]]$dm.choice.ind
dm.ind.tours = getToursFromIndividual(dm.ind, append.depots = TRUE)
dm.init.tours = getInitToursFromIndividual(dm.ind, append.depot = TRUE)
names(dm.ind.tours) = paste0("vehicle", 1:dm.ind$n.vehicles)

pl.instance = autoplot(instance, path = dm.ind.tours)
idx.man = which(instance$arrival.times == 0)
pl.instance = pl.instance + geom_point(data = as.data.frame(instance$coordinates[idx.man, ]), colour = "red")

print(pl.instance)
#gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)

source("R/visualization.R")
pl = plotNetworkFancy(instance, time.resolution = 250L,
  tours = list("1" = dm.ind.tours, "3" = dm.ind.tours),
  init.tours = list("1" = dm.init.tours, "3" = dm.init.tours))


stop("Done :)")
