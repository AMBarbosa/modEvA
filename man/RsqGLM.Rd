\name{RsqGLM}
\alias{RsqGLM}
\title{R-squared measures for GLMs}
\description{
This function calculates some (pseudo) R-squared statistics for binomial Generalized Linear Models. It is now DEPRECATED, please use \code{\link{pseudoRsq}} instead.
}
\usage{
RsqGLM(model = NULL, obs = NULL, pred = NULL, use = "pairwise.complete.obs",
plot = TRUE, plot.type = "lollipop", na.rm = TRUE, rm.dup = FALSE, 
verbosity = 2, ...)
}
\arguments{
  \item{model}{see \code{\link{pseudoRsq}}}
  \item{obs}{see \code{\link{pseudoRsq}}}
  \item{pred}{see \code{\link{pseudoRsq}}}
  \item{use}{see \code{\link{pseudoRsq}}}
  \item{plot}{see \code{\link{pseudoRsq}}}
  \item{plot.type}{see \code{\link{pseudoRsq}}}
  \item{na.rm}{see \code{\link{pseudoRsq}}}
  \item{rm.dup}{see \code{\link{pseudoRsq}}}
  \item{verbosity}{see \code{\link{pseudoRsq}}}
  \item{\dots}{see \code{\link{pseudoRsq}}}
}
\details{See \code{\link{pseudoRsq}}.}
\value{See \code{\link{pseudoRsq}}.}
\author{A. Marcia Barbosa}
\examples{
# This function is DEPRECATED, please see \code{\link{pseudoRsq}} instead.
}
