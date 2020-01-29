\name{modEvAmethods}
\alias{modEvAmethods}
\title{
Methods implemented in modEvA functions
}
\description{
This function allows retrieving the methods available for some of the functions in modEvA, such as \code{\link{threshMeasures}}, \code{\link{optiThresh}}, \code{\link{multModEv}} and \code{\link{getBins}}.
}
\usage{
modEvAmethods(fun)
}
\arguments{
  \item{fun}{a character vector of length 1 specifying the name (in quotes) of the function for which to obtain the available methods.}
}
\value{a character vector of the available methods for the specified function.}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{optiThresh}}, \code{\link{getBins}}, \code{\link{multModEv}}
}
\examples{
modEvAmethods("threshMeasures")

modEvAmethods("multModEv")

modEvAmethods("optiThresh")

modEvAmethods("getBins")
}
