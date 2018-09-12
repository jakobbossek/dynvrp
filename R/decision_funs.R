# Simulation of decision maker, i.e., the human
# being, which decides after each epoch which tour
# to choose.

#' @title Decision maker
#'
#' @description Function to simulate a decision maker. Function \code{decideRandom}
#' selects a solution uniformly at random. Function \code{decideInteractive} pauses
#' the execution and asks the user for manual selection in the terminal. Function
#' \code{decideRank} performs an order-based selection, i.e., one selects an objective
#' and a quantile and the corresponding elements ordered by the selected objective
#' is chosen.
#'
#' @param fitness [matrix]\cr
#'   Fitness matrix where each column represents one non-dominated solution.
#' @param q [\numeric(1)]\cr
#'   Numeric values in \eqn{[0, 1]}. Used by \code{decideRank}.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [integer(1)] Index, i.e., column number, of solution to decide for.
#' @rdname decide
#' @name decide
#' @export
decideRandom = function(fitness, ...) {
  n = ncol(fitness)
  sample(seq_len(n), 1L)
}

#' @rdname decide
#' @export
decideInteractive = function(fitness, ...) {
  catf("INTERACTIVE DECISION:")
  print(fitness)
  dm.choice = as.integer(readline(prompt = "Choose index of preferred solution: "))
  return(dm.choice)
}

#' @rdname decide
#' @export
decideRank = function(obj, q = 0.5, ...) {
  assertNumber(obj, lower = 1, upper = 2)
  assertNumber(q, lower = 0, upper = 1)

  force(obj)
  force(q)

  decide = function(fitness, ...) {
    n = ncol(fitness)
    vals = fitness[obj, ]
    rk = floor(q * n)
    if (rk == 0)
      rk = 1L
    ord = order(vals)
    dm.choice = ord[rk]
    return(dm.choice)
  }
  return(decide)
}
