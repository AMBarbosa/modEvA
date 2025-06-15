\name{predPlot}
\alias{predPlot}
\title{
Plot predicted values for presences and absences, optionally classified according to a prediction threshold.
}
\description{
This function plots predicted values separated into observed presences and absences and (optionally and by default) coloured according to whether they are above or below a given prediction threshold. The plot imitates (with permission from the author) one of the graphical outputs of the 'summary' of models built with the \pkg{embarcadero} package (Carlson, 2020), but it can be applied to other types of models or to a set of observed and predicted values, and it allows specifying a user-defined threshold.
}
\usage{
predPlot(model = NULL, obs = NULL, pred = NULL, thresh = "preval", pbg = FALSE,
main = "Classified predicted values", legend.pos = "n", pch = 1, cex = 0.5,
col = c("black", "grey"), na.rm = TRUE, rm.dup = FALSE, interval = 0.01,
quant = 0, verbosity = 2)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{thresh}{threshold value to separate predicted presences from predicted absences in 'pred'; can be "preval" (the default), to use the \code{\link{prevalence}} (i.e. proportion of presences) in 'obs'; or any real number between 0 and 1; or any of the options available on modEvAmethods("getThreshold") -- see Details in \code{\link{getThreshold}} for their description. This value, if not NA or NULL, will be used to draw a vertical line on the plot and to colour the points (predicted values) according to whether they fall above or below the threshold.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{main}{Main title for the plot.}
  \item{legend.pos}{character value specifying the position for the legend on the plot. Can be "bottomleft", "bottom", "bottomright", "topleft", "left", "top", "topright", "right", "center", or NA or "n" for no legend (the default). Partial argument matching is used.}
  \item{pch}{plotting character for the presences and absences (see \code{\link{par}}).}
  \item{cex}{relative size of the plotting character (see \code{\link{par}}).}
  \item{col}{vector of length 2 indicating the colours with which to plot predicted presences and absences (points above and below the threshold), respectively. If 'thresh' is NA or NULL, all points will have the first of the specified colours.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{interval}{Argument to pass to \code{\link{optiThresh}} indicating the interval between the thresholds to test, if 'thresh' implies optimizing a threshold-based measure. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{Numeric value indicating the proportion of presences to discard if thresh="MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\note{
Points are \code{\link{jitter}}ed randomly along the y axis to minimize visual overlap. So, each run of 'predPlot' (unless you use \code{\link{set.seed}} first) will produce a different arrangement of points for the same data, although their x-axis values are faithful.
}
\value{
This function outputs a plot as per 'Description'.
}
\references{
Carlson C.J. (2020) embarcadero: Species distribution modelling with Bayesian additive regression trees in R. Methods in Ecology and Evolution, 11: 850-858.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{predDensity}}, \code{\link{plotGLM}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

predPlot(model = mod)

predPlot(model = mod, thresh = 0.5)


# you can first select a threshold optimized according to a particular metric:

\dontrun{
threshold <- optiThresh(mod, measures = "TSS", optimize = "each")
threshold <- threshold$optimals.each[ , "threshold"]
threshold
predPlot(model = mod, thresh = threshold)
}

# you can also use 'predPlot' with vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

predPlot(obs = presabs, pred = prediction)

predPlot(obs = presabs, pred = prediction, thresh = 0.5)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
