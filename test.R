library(devtools)
library(ggplot2)

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
  init.keep = TRUE,
  stop.conds = list(ecr::stopOnIters(100)),
  n.timeslots = 7L)
time.passed = proc.time() - st

fronts = lapply(emoa.res$era.results, function(x) x$front)
current.era = length(fronts)

the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

max.era = length(fronts)

ind = emoa.res$era.results[[max.era]]$result$pareto.set[[1]]
ind.tour = getTourFromIndividual(ind)

pl.instance = autoplot(instance, path = c(1L, ind.tour, 2L))
idx.man = which(instance$arrival.times == 0)
pl.instance = pl.instance + geom_point(data = as.data.frame(instance$coordinates[idx.man, ]), colour = "red")

pl.instance
gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)


stop("määä")

# TEST Hamilton path computation
idx.man = which(instance$arrival.times == 0) + 2L

res = findHamiltonianPath(instance, active.nodes = idx.man, start.id = 1, dest.id = 2)
pl = autoplot(instance, path = c(1, res, 2))
pl = pl + geom_point(data = as.data.frame(instance$coords[idx.man,]), colour = "red")
print(pl)

# TEST initialization of individual
init.tour = c(7, 9, 15)
ind = initIndividual(instance, current.time = 0, init.tour = init.tour)
tour = getTourFromIndividual(ind)
pl = autoplot(instance, path = c(1, tour, 2))
pl = pl + geom_point(data = as.data.frame(instance$coords[idx.man, ]), colour = "red")
print(pl)

# TEST mutation

for (i in 1:100) {
  ind2 = mutVRP(ind)
  tour = getTourFromIndividual(ind2)
  BBmisc::catf("Init tour wrong: %i", all(tour[1:length(init.tour)] == (init.tour + 2)))
}
pl2 = autoplot(instance, path = c(1, tour, 2))
pl2 = pl2 + geom_point(data = as.data.frame(instance$coords[idx.man, ]), colour = "red")
print(pl2)



stop("määä")



fronts = lapply(emoa.res$era.results, function(x) x$front)
current.era = length(fronts)

the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L)

max.era = length(fronts)

tt = emoa.res$era.results[[1]]$result$pareto.set[[1]]$t
bb = emoa.res$era.results[[1]]$result$pareto.set[[1]]$b

idx = which(bb == 1)
idx = sort(which(tt %in% idx))

pl.instance = autoplot(instance, path = c(1L, tt[idx] + 2L))
pl.instance
gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)

#stop("DONE")

instances = list.files(PATH.TO.INSTANCES, recursive = TRUE, pattern = "n100_cl5_dyn75", full.names = TRUE)
instances = instances[1:5]
aposteriori = read.table("results/aposteriori.csv", header = TRUE)
print(head(instances))
print(head(aposteriori))

for (inst in instances[1L]) {
  catf("Importing instance %s", instance.path)
  instance = salesperson::importFromFile(instance.path)
  #FIXME: handle in salesperson package
  instance$coords = rbind(instance$depot.coordinates, instance$coordinates)
  instance$dmat   = as.matrix(dist(instance$coords), method = "euclidean")

  emoa.res = dynamicVRPEMOA(
    fitness.fun, decision.fun = decideRank(1L, 0), instance = instance,
    mu = 10L, lambda = 5L,
    stop.conds = list(ecr::stopOnIters(100L)),
    n.timeslots = 10L)

  fronts = lapply(emoa.res, function(x) x$front)
  current.era = length(fronts)

  #FIXME: gsub because of leading whitespace in filename
  the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]

  pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

  max.era = length(fronts)
  path.to.instance = list.files("../../PaperGECCO2015/instances/publication/", pattern = basename(inst), full.names = TRUE, all.files = TRUE)
  instance = salesperson::importFromFile(path.to.instance)

  pl.instance = autoplot(instance, path = c(1L, emoa.res[[max.era]]$init.tour))

  gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)

  plot.file = sprintf("images/%s-%s.pdf", basename(inst), "EAX")
  ggsave(plot.file, plot = pl.fronts)
}


fronts = lapply(emoa.res$era.results, function(x) x$front)
current.era = length(fronts)

  #FIXME: gsub because of leading whitespace in filename
  the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]

  pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

  max.era = length(fronts)
  path.to.instance = list.files("../../PaperGECCO2015/instances/publication/", pattern = basename(inst), full.names = TRUE, all.files = TRUE)
  instance = salesperson::importFromFile(path.to.instance)

  pl.instance = autoplot(instance, path = c(1L, emoa.res$era.results[[max.era]]$init.tour + 2, 2))

  gridExtra::grid.arrange(pl.instance, pl.fronts, nrow = 1L)

  plot.file = sprintf("images/%s-%s.pdf", basename(inst), "EAX")
  ggsave(plot.file, plot = pl.fronts)
