\name{predDensity}
\alias{predDensity}
\title{
Plot the density of predicted or predictor values for presences and absences (or background)
}
\description{
This function produces a histogram and/or a kernel density plot of predicted or predictor values for a binary-response model, optionally separately for the observed presences and absences (or background, if pbg = TRUE), given either a model object or a vector of predicted (or predictor) values, and optionally a vector of the corresponding observed values (with 0 for absence and 1 for presence). When there are multiple predicted values for each site (e.g. for BART models), it can also plot a confidence interval.
}
\usage{
predDensity(model = NULL, obs = NULL, pred = NULL, separate = TRUE,
type = "both", ci = NA, pbg = FALSE, legend.pos = "topright",
main = "Density of pred values", na.rm = TRUE, rm.dup = FALSE, xlim = NULL,
verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'pred' (and optionally 'obs') argument(s) instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', an optional numeric vector (in the same order of 'pred') of observed presences (1) and absences (0) of a binary response variable. If pbg = TRUE, the presences will be appended to the absences to create the background. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument may be omitted (to show the density plot of all 'pred' values combined), and it is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model', a vector of predicted values of presence probability, habitat suitability, environmental favourability or alike; or a vector of values of a continuous predictor variable. Must be of the same length and in the same order as 'obs' (if the latter is provided). Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{separate}{logical value indicating whether prediction densities should be computed separately for observed presences (ones) and absences (zeros), or for presences (ones) and background (ones and zeros) if pbg = TRUE. Defaults to TRUE, but it is automatically changed to FALSE if either 'model' or 'obs' are not provided, or if 'ci' is not NA.}
  \item{type}{character vector specifying whether to produce a "histogram", a "density" plot, or "both" (the default). Partial argument matching is used.}
  \item{ci}{optional numeric value for a confidence interval to add to the plot, e.g. 0.95 for 95\%. The default is NA. If specified, argument 'separate' is set to FALSE.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{legend.pos}{character specifying the position for the legend; NA or "n" for no legend. Position can be "topright" (the default), "topleft, "bottomright"", "bottomleft", "top", "bottom", "left", "right", or "center". Partial argument matching is used.}
  \item{main}{main title for the plot.}
  \item{na.rm}{logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{if \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{xlim}{numeric vector of length 2 setting the limits for the x axis of the plot. The default is NULL, for the range of the \code{\link{density}} of predicted or predictor values.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to pass to \code{\link{hist}}, e.g. 'breaks' or 'border'.}
}
\details{
For more details, see \code{\link{density}} and/or \code{\link{hist}}.
}
\value{
This function outputs and plots the object(s) specified in 'type' -- by default, a \code{\link{density}} object and a \code{\link{hist}}ogram.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{hist}}, \code{\link{density}}, \code{\link{predPlot}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]


# compute predDensity with different parameters:

predDensity(model = mod)

predDensity(model = mod, breaks = seq(0, 1, by = 0.05))

predDensity(model = mod, type = "histogram")

predDensity(model = mod, type = "density")

predDensity(model = mod, ci = 0.975)


# you can also use 'predDensity' with vectors of
# observed and predicted values, instead of a model object:

obs <- mod$y
pred <- mod$fitted.values

predDensity(obs = obs, pred = pred)

predDensity(pred = pred, ci = 0.95)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
