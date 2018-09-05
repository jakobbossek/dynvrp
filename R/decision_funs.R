# Simulation of decision maker, i.e., the human
# being, which decides after each epoch which tour
# to choose.

#' @title Decision makers.
#'
#' @description Decision maker selects a solution uniformly at random.
#'
#' @param fitness [matrix]\cr
#'   Fitness matrix where each column represents one non-dominated solution.
#' @param q [\numeric(1)]\cr
#'   Numeric values in \eqn{[0, 1]}. Used by \code{decideRank}.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [integer(1)] Index, i.e., column number, of solution to decide for.
decideRandom = function(fitness, ...) {
  n = ncol(fitness)
  sample(seq_len(n), 1L)
}

decideInteractive = function(fitness, ...) {
  catf("INTERACTIVE DECISION:")
  print(fitness)
  dm.choice = as.integer(readline(prompt = "Choose index of preferred solution: "))
  return(dm.choice)
}

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
