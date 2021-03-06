% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual.R
\name{initIndividual}
\alias{initIndividual}
\title{Individual initializer.}
\usage{
initIndividual(instance, current.time = 0, init.tours = integer(),
  n.vehicles = 1L, template.ind = NULL,
  init.distribution = "binomial")
}
\arguments{
\item{instance}{[\code{Network}]\cr
Network instance.}

\item{current.time}{[\code{numeric(1)}]\cr
Current point in time.
Default is 0.}

\item{init.tours}{[\code{integer}]\cr
List of fixed prefix tours, i.e., part of a vehicles tour which is already fixed,
because time passed and vehicle already visited some customers.}

\item{n.vehicles}{[\code{integer(1)}]\cr
Number of vehicles.
Defaults to 1.}

\item{template.ind}{[\code{integer}]\cr
Tour used as a \dQuote{template} for a newly generated individual. Here,
we aim to pass as much information from \code{template.ind} as possible.}

\item{init.distribution}{[\code{character(1)}]\cr
How shall available dynamic customers be sampled?
Option \dQuote{binomial}: each dynamic available customer is active with probability \eqn{0.5}
independently.
Option \dQuote{uniform}: if there are \eqn{n_d} available dynamic customers,
we have \eqn{P(X = i) = \frac{1}{n_d}} for \eqn{i \in \{1, \ldots, n_d\}}. In a second step
\eqn{i} positions are sampled at random.}
}
\value{
[\code{VRPIndividual}] List with following components:
\describe{
  \item{\code{b}}{Binary vector of length |V| - 2. b[i] is 1, if customer i is active, i.e., in tour.}
  \item{\code{t}}{Permutation vector.}
  \item{\code{p}}{Vector of mutation probablities. I.e., p[i] is the probability to flip b[i]. p[i] is zero if customer i is already fixed or not yet released.}
  \item{\code{it}}{Binary vector of length |V| - 2. it[i] is 1 if and only if customer i is in initial tour.}
  \item{n.mandatory \code{integer(1)}}{Number of mandatory customers.}
  \item{idx.dynamic.available \code{integer}}{IDs/positions of dynamic customers which already requested serving.}
  \item{n.dynamic.active \code{integer(1)}}{Number of active dynamic customers i (i.e., b[i] = 1)}
  \item{n.dynamic.inactive \code{integer(1)}}{Number of available, but not active dynamic customers i (i.e., b[i] = 0)}
  \item{init.tours \code{integer}}{Fixed tour part, i.e., sequence of nodes already visited.}
}
}
\description{
Individual initializer.
}
