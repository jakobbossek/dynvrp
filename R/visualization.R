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

  pl = ggplot2::ggplot(data = df, ggplot2::aes(x = f1, y = f2)) + ggplot2::geom_point(aes(colour = era))

  # now highlight selected decision
  if (!is.null(selected)) {
    current.era.df = fronts[[current.era]]
    catf("Selected element: %i", selected)
    current.era.df = current.era.df[selected, , drop = FALSE]
    print(current.era.df)
    pl = pl + ggplot2::geom_point(data = current.era.df, size = 3, colour = "black") + ggplot2::geom_point(data = current.era.df, size = 2.8, colour = "white")
  }

  if (!is.null(a.posteriori.approx)) {
    pl = pl + ggplot2::geom_point(data = a.posteriori.approx, ggplot2::aes(shape = LS), alpha = 0.8, colour = "black")
  }

  pl = pl + ggplot2::labs(
    subtitle = sprintf("Era %i\nCurrent time: %.2f", current.era, current.time),
    x = "tour length",
    y = "# of unvisited dyn. customers"
  )
  return(pl)
}

plotNetworkFancy = function(object,
  path = NULL, close.path = FALSE, path.colour = "gray",
  current.time = Inf,
  last.time = NULL,
  ...) {
  if (!is.null(path)) {
    if (!testNumeric(path, min.len = 2L, any.missing = FALSE) & !testList(path, min.len = 2L, any.missing = FALSE)) {
      stopf("Path argument needs to be a vector or a list.")
    }
  }
  assertString(path.colour, na.ok = FALSE)
  assertFlag(close.path, na.ok = FALSE)

  if (ncol(object$coordinates) > 2L) {
    stopf("Only 2-dimensional networks can be plotted.")
  }

  df = as.data.frame(object, include.extras = TRUE)

  if (testClass(object, "ClusteredNetwork")) {
    df$membership = as.factor(df$membership)
  }

  if (!is.null(object$arrival.times)) {
    arrival.times = c(0, 0, object$arrival.times)
    df$customer = "mandatory"
    df$customer[arrival.times > 0] = "dynamic"

    if (!is.null(last.time)) {
      df$customer[arrival.times <= current.time & arrival.times > last.time & df$customer == "dynamic"] = "NEW"
    }
  }

  if (hasDepots(object)) {
    depot.idx = which(df$types == "depot")
    df.depots = df[depot.idx, , drop = FALSE]
  }

  # handle arrival times
  df2 = df
  if (!is.null(object$arrival.times)) {
    arrival.times = c(0, 0, object$arrival.times)
    df2 = df2[which(arrival.times <= current.time), , drop = FALSE]
  }


  print(head(df2))

  if (is.list(path)) {
    n = nrow(df)
    df = df[rep(seq_len(n), length(path)), ]
    ns = if (!is.null(names(path))) names(path) else as.character(seq_len(length(path)))
    df$Path = rep(ns, each = n)
  }

  pl = ggplot(data = df2, mapping = aes_string(x = "x1", y = "x2"))

  # facets if multiple paths given
  if (is.list(path)) {
    pl = pl + facet_grid(. ~ Path)
  }

  if (!is.null(path)) {
    # if we have a list of pathes
    if (is.list(path)) {
      # sequentially build pathes (one path per facet)
      path.coords = data.frame()
      for (i in seq_len(length(path))) {
        p = path[[i]]
        pname = names(path)[i]
        if (close.path) {
          p = c(p, p[1])
        }
        the.path.coords = df[p, , drop = FALSE]
        the.path.coords$Path = pname
        path.coords = rbind(path.coords, the.path.coords)
      }
    } else {
      # do the same as above only for one path
      if (close.path) {
        path = c(path, path[1])
      }
      path.coords = df[path, , drop = FALSE]
    }
    pl = pl + geom_path(data = path.coords, colour = path.colour)
  }

  if (!is.null(df2$customer)) {
    pl = pl + geom_point(aes_string(colour = "customer"))
  } else {
    pl = pl + geom_point(colour = "black")
  }

  if (hasDepots(object)) {
    pl = pl + geom_point(data = df.depots, colour = "black", size = 4)
    pl = pl + geom_point(data = df.depots, colour = "white", size = 3)
  }
  pl = pl + ggtitle(as.character(object))
  #pl = salesperson:::decorateGGPlot(pl, lower = object$lower, upper = object$upper)
  return(pl)
}

