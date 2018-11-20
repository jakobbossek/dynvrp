#' Extract list of decision maker tours and init tours from EMOA result.
#'
#' @param result [\code{list}]\cr
#'   Return value of \code{\link{dynamicVRPEMOA}}.
#' @param eras [\code{integer}]\cr
#'   Which eras to consider.
#'   Default is all eras.
#' @return [\code{list}] List with components \dQuote{dm.tours} and \dQuote{init.tours}
#' as needed by \code{\link{plotNetworkFancy}}.
#' @export
getListOfToursByEras = function(result, eras = NULL) {
  n.eras = length(result$era.results)
  checkmate::assertNumeric(eras, lower = 1, upper = n.eras)

  dm.tours = lapply(result$era.results, function(res) {
    tours = getToursFromIndividual(res$dm.choice.ind, append.depots = TRUE)
    names(tours) = paste0("vehicle", seq_len(res$dm.choice.ind$n.vehicles))
    return(tours)
  })
  names(dm.tours) = as.character(seq_len(n.eras))

  init.tours = lapply(result$era.results, function(res) {
    tours = getInitToursFromIndividual(res$dm.choice.ind, append.depot = TRUE)
    names(tours) = paste0("vehicle", seq_len(res$dm.choice.ind$n.vehicles))
    return(tours)
  })
  names(init.tours) = as.character(seq_len(n.eras))

  if (!is.null(eras)) {
    dm.tours = dm.tours[as.integer(names(dm.tours)) %in% eras]
    init.tours = init.tours[as.integer(names(init.tours)) %in% eras]
  }

  return(list(dm.tours = dm.tours, init.tours = init.tours))
}
