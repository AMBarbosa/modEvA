\name{lollipop}
\alias{lollipop}
\title{Lollipop chart}
\description{
This function creates a lollipop chart (by default), a points chart (if \code{sticks = FALSE}) or a point-range chart (if \code{sticks} are confidence limits) from a (named) numeric vector.
}
\usage{
  lollipop(x, names = NULL, ymin = 0, ylim = "auto0", sticks = TRUE, 
  col = "royalblue4", grid = TRUE, cex = 1, cex.axis = 1, las = 2, 
  horiz = FALSE, bold = FALSE, ...)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{names}{a vector of the same length as 'x' with the names to be plotted below the lollipops. If this argument is left NULL and 'x' has names, then these will be used.}
  \item{ymin}{numeric value for the lower limit of the y axis (default 0). If set to NA, the minimum of 'x' will be used. This argument is deprecated, as it is now superseded by, and should be replaced with, 'ylim' (below).}
  \item{ylim}{either a numeric vector of length 2 specifying the limits (minimum, maximum) for the y axis; or "auto" to fit the y axis to the minimum and maximum existing values in 'x'; or "auto0" (the new default) to fit the top of the y axis to the maximum existing values, and the bottom to zero or the minimum existing value (whichever is lower). If \code{horiz} is TRUE, these limits refer instead to the x axis.}
  \item{sticks}{either a logical value indicating whether the sticks of the lollipops (lines from zero to each value) should be drawn (default TRUE); or a two-column matrix or dataframe with one row for each element in x, with the minimum and maximum values for the error bars or confidence intervals (in which case a point-range chart is drawn).}
  \item{col}{colour for the lollipops.}
  \item{grid}{logical value (default TRUE) indicating whether or not to add a grid to the plot.}
  \item{cex}{numeric value indicating the size of the lollipops. Will be passed as 'cex' to \code{\link{points}} and as 'lwd' to \code{\link{arrows}} (the lines or lollipop sticks).}
  \item{cex.axis}{numeric value to pass to \code{\link{axis}} and to \code{\link{mtext}} indicating the size of the x and y axis labels.}
  \item{las}{argument to pass to \code{\link{par}} indicating the orientation of the axis labels.}
  \item{horiz}{logical value (default FALSE) indicating whether the plot should be drawn horizontally rather than vertically.}
  \item{bold}{logical vector (recycled if shorter than \code{x}, default FALSE) indicating which name labels should appear in bold font. Can be used e.g. to highlight significant values (see e.g. \code{\link{plotCoeffs}}).}
  \item{\dots}{additional arguments that can be passed to \code{\link{plot}}, e.g. 'main', 'xlab' or 'ylab'.}
}
\details{
  According to modern data viz recommendations, lollipop charts (or instead point charts) are generally a better alternative to bar charts, as they reduce the visual distortion caused by the length of the bars, making it easier to visually compare the values.
}
\value{
  A plot.
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link{plotCoeffs}}; \code{\link{barplot}}; \code{\link[ggplot2:geom_pointrange]{ggplot2::geom_pointrange()}}
}
\examples{
par(mar = c(6, 3, 2, 1))

lollipop(mtcars[,1], names = rownames(mtcars), las = 2, 
ylab = names(mtcars)[1], cex.axis = 0.6)

lollipop(mtcars[,1], names = rownames(mtcars), las = 2, 
ylab = names(mtcars)[1], cex.axis = 0.6, ylim = NA)


par(mar = c(3, 6, 2, 1))

lollipop(mtcars[,1], names = rownames(mtcars), las = 2, 
xlab = "Miles/gallon", cex.axis = 0.6, main = "Lollipop chart of mtcars", 
horiz = TRUE)
}
