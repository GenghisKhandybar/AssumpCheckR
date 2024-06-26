% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_linearity.R
\name{check_linearity}
\alias{check_linearity}
\title{Checks the linearity assumption of an x and y variable.}
\usage{
check_linearity(
  x,
  y,
  sig_level = 0.05,
  include_graph = TRUE,
  include_interpretation = TRUE
)
}
\arguments{
\item{x}{A single predictor variable.}

\item{y}{A numeric response variable.}

\item{sig_level}{The significance level for the test. 0.05 by default.}

\item{include_graph}{Whether to include the residual plot. TRUE by default.}

\item{include_interpretation}{Whether to include an interpretation of the results.}
}
\value{
A test, interpretation, and plot (optional)
}
\description{
Checks the linearity assumption of an x and y variable.
}
