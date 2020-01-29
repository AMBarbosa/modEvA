\name{arrangePlots}
\alias{arrangePlots}
\title{
Arrange plots
}
\description{
Get an appropriate row/column combination (for \code{par(mfrow)}) for arranging a given number of plots within a plotting window.
}
\usage{
arrangePlots(n.plots, landscape = FALSE)
}
\arguments{
  \item{n.plots}{
number of plots to be placed in the graphics device.
}
  \item{landscape}{
logical, whether the plotting window should be landscape/horizontal (number of columns larger than the number of rows) or not. The value does not make a difference if the number of plots makes for a square plotting window.
}
}
\details{
This function is used internally by \code{optiThresh}, but can also be useful outside it.
}
\value{
An integer vector of the form c(nr, nc) indicating, respectively, the number of rows and of columns of plots to set in the graphics device.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{plot}}, \code{\link{layout}}
}
\examples{
arrangePlots(10)

arrangePlots(10, landscape = TRUE)


# a more practical example:

data(iris)

names(iris)

# say you want to plot all columns in a nicely arranged plotting window:

par(mfrow = arrangePlots(ncol(iris)))

for (i in 1:ncol(iris)) {
  plot(1:nrow(iris), iris[, i])
}
}
