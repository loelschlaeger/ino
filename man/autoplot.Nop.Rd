% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.Nop}
\alias{autoplot.Nop}
\alias{autoplot.Nop_optima}
\alias{autoplot.Nop_deviation}
\alias{autoplot.Nop_results}
\title{Plotting methods}
\usage{
\method{autoplot}{Nop}(object, xlim = NULL, xlim2 = NULL, ...)

\method{autoplot}{Nop_optima}(object, ...)

\method{autoplot}{Nop_deviation}(object, jitter = TRUE, ...)

\method{autoplot}{Nop_results}(
  object,
  which_element = "seconds",
  group_by = NULL,
  relative = FALSE,
  ...
)
}
\arguments{
\item{object}{Depends on the method:
\itemize{
\item for \code{autoplot.Nop()}, a \code{Nop} object
\item for \code{autoplot.Nop_results()}, the value \code{Nop$results}
\item for \code{autoplot.Nop_optima()}, the value \code{Nop$optima}
\item for \code{autoplot.Nop_deviation()}, the value \code{Nop$deviation}
}}

\item{xlim, xlim2}{[\code{numeric(2)}]\cr
Ranges for the first and second parameter to plot.

If \code{NULL}, they are derived from the specified initial values in \code{object}.}

\item{...}{Other arguments passed to specific methods.}

\item{jitter}{[\code{logical(1)}]\cr
Apply jitter to the points?}

\item{which_element}{[\verb{character(1)\]\\cr A column name of }object` to plot.}

\item{group_by}{[`character(1)]\cr
Selects how the plot is grouped. Either:
\itemize{
\item \code{NULL} to not group,
\item \code{"optimization"} to group by optimization label,
\item `"optimizer"`` to group by optimizer label.
}}

\item{relative}{[`logical(1)]\cr
Plot values relative to the overall median?}
}
\value{
A \code{ggplot} object.
}
\description{
\itemize{
\item \code{autoplot.Nop()} plots the objective function
\item \code{autoplot.Nop_results()} plots boxplots of optimization results
\item \code{autoplot.Nop_optima()} plots a bar chart of the found optima
\item \code{autoplot.Nop_deviation()} plots deviations per dimension from a reference
}
}
