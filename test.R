library(devtools)
library(ggplot2)
library(ecr)

load_all(".")

PATH.TO.INSTANCES = normalizePath("/Users/jboss/repositories/paper/orienteering_publications/PaperGecco2015/instances/publication/")

SOLVER.PATH = c("eax" = file.path("/Users/jboss/repositories/paper/tsp_publications/TSPAS/solvers/EAX/main"))
salesperson::solverPaths(as.list(SOLVER.PATH))

aposteriori = read.table("inst/results/aposteriori.csv", header = TRUE)

instance.path = file.path(PATH.TO.INSTANCES, " vrp_morphed_n100_mo25_dyn75_r1.csv")
inst = " vrp_morphed_n100_mo25_dyn75_r1.csv"

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
#FIXME: handle in salesperson package
instance$coords = rbind(instance$depot.coordinates, instance$coordinates)
instance$dmat   = as.matrix(dist(instance$coords), method = "euclidean")

# EMOA test environment
debug = NA

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
#FIXME: handle in salesperson package
instance$coords = rbind(instance$depot.coordinates, instance$coordinates)
instance$dmat   = as.matrix(dist(instance$coords), method = "euclidean")

st = proc.time()
emoa.res = dynamicVRPEMOA(
  fitness.fun, decision.fun = decideRank(1L, 0), instance = instance,
  mu = 10L, lambda = 5L,
  local.search.method = "eax",
  local.search.gens = c(100),
  init.keep = TRUE,
  stop.conds = list(ecr::stopOnIters(100)),
  #n.timeslots = 8L
  )
time.passed = proc.time() - st

fronts = lapply(emoa.res$era.results, function(x) x$front)
fronts = lapply(fronts, function(x) {
  x$f2 = x$f2shifted
  return(x)
})
current.era = length(fronts)

the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

max.era = length(fronts)

dm.ind = emoa.res$era.results[[max.era]]$dm.choice.ind
dm.ind.tour = getTourFromIndividual(dm.ind)

pl.instance = autoplot(instance, path = c(1L, dm.ind.tour, 2L))
idx.man = which(instance$arrival.times == 0)
pl.instance = pl.instance + geom_point(data = as.data.frame(instance$coordinates[idx.man, ]), colour = "red")

pl.instance
gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)


stop("Done :)")
