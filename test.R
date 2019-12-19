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
instance.path = file.path(PATH.TO.INSTANCES, " vrp_n100_gm_dyn75_r1.csv")
inst = " vrp_n100_gm_dyn75_r1.csv"

# EMOA test environment
debug = NA

catf("Importing instance %s", instance.path)
instance = salesperson::importFromFile(instance.path)
instance = toVRPInstance(instance)

#instance.apost = aposteriori

set.seed(1)

if (TRUE) {
st = proc.time()
emoa.res2 = dynamicVRPEMOA(
  fitness.fun2,
  decision.fun = list(decideRank, decideRank, decideRank, decideRank, decideRank, decideRank, decideRank),
  decision.params = list(list(q = 0.25), list(q = 0.25), list(q = 0.25), list(q = 0.25), list(q = 0.25), list(q = 0.25), list(q = 0.25)),
  instance = instance,
  mu = 75L, lambda = 5L,
  n.vehicles = 3L,
  p.swap = 0.8,
  local.search.method = NULL,#"eax",
  local.search.gens = c(1L, 500, 1000L),
  init.keep = TRUE,
  n.timeslots = 7L,
  time.resolution = NULL,
  stop.conds = list(ecr::stopOnIters(500L))
)
time.passed = proc.time() - st

aposteriori = dynamicVRPEMOA(
  fitness.fun2,
  instance = instance,
  mu = 75L, lambda = 5L,
  n.vehicles = 1L,
  p.swap = 0.8,
  local.search.method = NULL,#"eax",
  local.search.gens = c(1L, 500, 1000L),
  init.keep = TRUE,
  n.timeslots = 7L,
  time.resolution = NULL,
  aposteriori = TRUE,
  stop.conds = list(ecr::stopOnIters(500L))
)
}
stop()

pl = ggplot(emoa.res2$pareto.front, aes(x = f1, y = f2shifted))
pl = pl + geom_point(aes(colour = as.factor(era)))
pl = pl + geom_point(data = aposteriori, colour = "black", alpha = 0.8)
print(pl)


stop()

# #stop("YAY!")
# current.era = length(fronts)

# the.aposteriori = aposteriori[aposteriori$prob == gsub(" ", "", basename(inst)), , drop = FALSE]
# pl.fronts = plotEras(fronts, current.era, length(fronts) * 100L, a.posteriori.approx = the.aposteriori)

fronts = emoa.res2$pareto.front

transparent_bg = function(pl) {
  return(theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)))
}

source("R/visualization.R")
tours = getToursFromMeta(emoa.res2$meta)

pl = plotNetworkFancy(instance, time.resolution = 160L,
  tours = tours$dm.tours,
  init.tours = tours$init.tours,
  filter.eras = c(1, 3, 7))
print(pl)


#  filter.eras = c(1, 5))
pl = pl + theme_bw()
pl = pl + transparent_bg()
print(pl)
ggsave("multiple_vehicles.pdf", plot = pl, width = 8, height = 5, device = cairo_pdf, bg = "transparent")




stop("Done :)")
