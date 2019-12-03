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

PATH.TO.INSTANCES = normalizePath("/Users/bossek/repos/research/vrp/PaperGecco2015/instances/publication/")
SOLVER.PATH = c("eax" = file.path("/Users/bossek/.config/salesperson/solvers/EAX/main"))
salesperson::solverPaths(as.list(SOLVER.PATH))

#aposteriori = read.table("inst/results/aposteriori.csv", header = TRUE)

# get test instance
instance.path = file.path(PATH.TO.INSTANCES, " vrp_morphed_n100_mo25_dyn75_r1.csv")
inst = " vrp_morphed_n100_mo25_dyn75_r1.csv"

# EMOA test environment
debug = NA

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
instance = toVRPInstance(instance)

#instance.apost = aposteriori

st = proc.time()
emoa.res2 = dynamicVRPEMOA(
  fitness.fun2,
  decision.fun = list(decideRank, decideRank, decideRank),
  decision.params = list(list(q = 0.25), list(q = 0.5), list(q = 1)),
  instance = instance,
  mu = 10L, lambda = 5L,
  n.vehicles = 1L,
  p.swap = 0.8,
  local.search.method = NULL, #"eax",
  local.search.gens = c(1L, 2000L),
  init.keep = TRUE,
  n.timeslots = NULL,
  time.resolution = 350,
  stop.conds = list(ecr::stopOnIters(1000L))
)
time.passed = proc.time() - st

# #stop("YAY!")
# current.era = length(fronts)

# the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
# pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

fronts = emoa.res2$pareto.front
tours = getListOfToursByEras(emoa.res2, eras = c(1, 2, 3))

transparent_bg = function(pl) {
  return(theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)))
}

source("R/visualization.R")
pl = plotNetworkFancy(instance, time.resolution = 350L,
  tours = tours$dm.tours,
  init.tours = tours$init.tours)
#pl = pl + theme_bw()
pl = pl + transparent_bg()
print(pl)
ggsave("multiple_vehicles.pdf", plot = pl, width = 8, height = 5, device = cairo_pdf, bg = "transparent")




stop("Done :)")
