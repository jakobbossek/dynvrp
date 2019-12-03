#' Visualize tours driven by vehicles by era.
#'
#' @param instance [\code{Network}]\cr
#'   Network.
#' @param time.resolution [\code{numeric(1)}]\cr
#'   Time resolution used by \code{\link{dynamicVRPEMOA}}.
#' @param tours [\code{list}]\cr
#'   List of lists of integer vectors. Each top.level list represents an era
#'   and the second-level lists contain the tours of the vehicles.
#' @param init.tours [\code{list}]\cr
#'   See documentation of \code{tours} for the structure. Contains for each
#'   era to depict the tours already traveled by the vehicles until the
#'   beginning of the corresponding era.
#' @param customers.by.era [\code{logical(1)}]\cr
#'   Colour customers by the era they arrived at?
#'   Default is \code{TRUE}.
#' @param highlight.depots [\code{logical(1)}]\cr
#'   Show start and end depot as large white circles with black border?
#'   Default is \code{TRUE}.
#' @param desaturate.nonvisited [\code{logical(1)}]\cr
#'   This one is experimental.
#'   Shall non-visited customers by depicted with reduced opacity?
#'   Default is \code{FALSE}.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @export
plotNetworkFancy = function(instance,
  time.resolution,
  tours = NULL,
  init.tours = NULL,
  customers.by.era = TRUE,
  highlight.depots = TRUE,
  desaturate.nonvisited = FALSE,
  filter.eras = NULL,
  facet.type = "grid", facet.args = list()) {

  checkmate::assertClass(instance, "Network")
  checkmate::checkNumber(time.resolution, lower = 1)
  checkmate::assertFlag(customers.by.era)

  df = as.data.frame(instance, include.extras = TRUE)
  #print(head(df))

  # if (!is.null(tours))
  #   checkmate::assertList(tours, types = "numeric")
  # if (!is.null(init.tours))
  #   checkmate::assertList(tours, types = "numeric")

  if (!is.null(tours) && !is.null(init.tours) && length(tours) != length(init.tours))
    BBmisc::stopf("[plotNetworkFancy] tours and init.tours need to be of same length.")

  if (is.null(instance$arrival.times))
    BBmisc::stopf("[plotNetworkFancy] Network needs to be dynamic.")

  # determine number of eras
  max.arrival.time = max(instance$arrival.times)
  n.eras = ceiling(max.arrival.time / time.resolution) + 1L
  n.vehicles = length(tours[[1L]])

  eras.to.show = as.integer(names(tours))

  if (any(eras.to.show > n.eras))
    BBmisc::stopf("[plotNetworkFancy] There are %i eras with time resolution %i, but
      eras to show are %s", n.eras, time.resolution, BBmisc::collapse(eras.to.show, sep = ", "))

  tour.grid = expand.grid(era = eras.to.show, vehicle = seq_len(n.vehicles))

  #print(tour.grid)
  df.tours = lapply(seq_len(nrow(tour.grid)), function(i) {
    era = tour.grid[i, 1L]
    vehicle = tour.grid[i, 2L]
    tour = tours[[as.character(era)]][[vehicle]]
    tour.coords = df[tour, , drop = FALSE]
    tour.coords$era = era
    tour.coords$vehicle = vehicle
    return(tour.coords)
  })
  df.tours = do.call(rbind, df.tours)
  #print(df.tours)

  df.init.tours = lapply(seq_len(nrow(tour.grid)), function(i) {
    era = tour.grid[i, 1L]
    vehicle = tour.grid[i, 2L]
    tour = init.tours[[as.character(era)]][[vehicle]]
    tour.coords = df[tour, , drop = FALSE]
    tour.coords$era = era
    tour.coords$vehicle = vehicle
    return(tour.coords)
  })
  df.init.tours = do.call(rbind, df.init.tours)
  #print(df.init.tours)

  # categorize customers by era
  arrival.times = c(0, 0, instance$arrival.times)
  BBmisc::messagef("Mandatory: %i", sum(arrival.times == 0))
  print(c(-1, 0, seq_len(n.eras - 1L)) * time.resolution)
  df$era2 = cut(arrival.times,
    breaks = (c(-1, 0, seq_len(n.eras - 1L))) * time.resolution,
    labels = seq_len(n.eras),
    right = TRUE)

  #print(df$era2)

  # if (!is.null(tours)) {
  #   visited = unique(unlist(tours))
  #   df[visited, "visited"] = 1.0
  #   df[!visited, "visited"] = 0.5
  # }

  df = do.call(rbind, lapply(eras.to.show, function(era) {
    tmp = df[as.integer(df$era2) <= era, , drop = FALSE]
    tmp$era = era
    return(tmp)
  }))
  #print(df)

  if (!is.null(filter.eras)) {
    df = df[df$era %in% filter.eras, , drop = FALSE]
    df.init.tours = df.init.tours[df.init.tours$era %in% filter.eras, , drop = FALSE]
    df.tours = df.tours[df.tours$era %in% filter.eras, , drop = FALSE]
  }

  #print(head(df))
  #print(table(df$visited))

  pl = ggplot(data = df, aes_string(x = "x1", y = "x2"))
  pl = pl + geom_path(data = df.tours)
  if (!is.null(init.tours)) {
    pl = pl + geom_path(data = df.init.tours, size = 2.2, alpha = 0.6)
  }
  if (customers.by.era) {
    if (desaturate.nonvisited) {
      pl = pl + geom_point(aes_string(colour = "era2"))#, alpha = visited))
    } else {
      pl = pl + geom_point(aes_string(colour = "era2"))
    }
  } else {
    pl = pl + geom_point()
  }

  if (highlight.depots) {
    df.depots = df[df$types == "depot", , drop = FALSE]
    pl = pl + geom_point(data = df.depots, aes_string(colour = NULL), colour = "black", size = 2.6)
    pl = pl + geom_point(data = df.depots, aes_string(colour = NULL), colour = "white", size = 2.2)
  }

  # facet.fun = if (facet.type == "grid") ggplot2::facet_grid else ggplot2::facet_wrap
  # facet.args = BBmisc::insert(list(facets = . ~ era + vehicle, labeller = label_both), facet.args)
  #pl = pl + do.call(facet.fun, facet.args)
  if (n.vehicles == 1L)
    pl = pl + facet_grid(. ~ era, labeller = label_both)#, ncol = 4)
  else
    pl = pl + facet_grid(era ~ vehicle, labeller = label_both)#, ncol = 4)
    #pl = pl + facet_grid(. ~ era + vehicle, labeller = label_both)#, ncol = 4)

  pl = pl + labs(
    colour = "Era",
    x = "", y = ""
  )
  pl = pl + viridis::scale_colour_viridis(discrete = TRUE, end = 0.85)
  return(pl)
}


plotEras = function(fronts, current.era, current.time, a.posteriori.approx = NULL, selected = NULL) {
  assertList(fronts)
  assertNumber(current.era, lower = 1L)
  assertNumber(current.time, lower = 1L)
  assertDataFrame(a.posteriori.approx, null.ok = TRUE)

  df = if (current.era == 1L)
    fronts[[1]]
  else
    do.call(rbind, fronts[1:current.era])
  df$era = as.factor(df$era)

  pl = ggplot2::ggplot(data = df, ggplot2::aes_string(x = "f1", y = "f2")) + ggplot2::geom_point(aes_string(colour = "era"))

  # now highlight selected decision
  if (!is.null(selected)) {
    current.era.df = fronts[[current.era]]
    catf("Selected element: %i", selected)
    current.era.df = current.era.df[selected, , drop = FALSE]
    #print(current.era.df)
    pl = pl + ggplot2::geom_point(data = current.era.df, size = 3, colour = "black") + ggplot2::geom_point(data = current.era.df, size = 2.8, colour = "white")
  }

  if (!is.null(a.posteriori.approx)) {
    pl = pl + ggplot2::geom_point(data = a.posteriori.approx, ggplot2::aes_string(shape = "LS"), alpha = 0.8, colour = "black")
  }

  pl = pl + ggplot2::labs(
    subtitle = sprintf("Era %i\nCurrent time: %.2f", current.era, current.time),
    x = "tour length",
    y = "# of unvisited dyn. customers"
  )
  return(pl)
}
