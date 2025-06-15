\name{confusionLabel}
\alias{confusionLabel}
\title{
Label predictions according to their confusion matrix category
}
\description{
This function labels the (typically continuous) predictions of a binary-response model according to their confusion matrix categories, i.e., it classifies each prediction into a false positive, false negative, true positive or true negative, given a user-defined threshold value.
}
\usage{
confusionLabel(model = NULL, obs = NULL, pred = NULL, thresh,
interval = 0.01, quant = 0, verbosity = 2, na.rm = FALSE,
rm.dup = FALSE, plot = TRUE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments (e.g. for external test data) instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{thresh}{numeric value of the threshold to separate predicted presences from predicted absences; can be "preval", to use the prevalence of 'obs' (or of the response variable in 'model') as the threshold, or any real number between 0 and 1. See Details in \code{\link{threshMeasures}} for an informed choice.}
  \item{interval}{numeric value, used if 'thresh' is a threshold optimization method such as "maxKappa" or "maxTSS", indicating the interval between the thresholds to test. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{numeric value indicating the proportion of presences to discard if thresh="MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
    \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{na.rm}{logical argument indicating whether to remove (with a warning saying how many) rows with NA in any of the 'obs' or 'pred' values. The default is FALSE.}
  \item{rm.dup}{if \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{plot}{if \code{TRUE} (the default) and if 'pred' is a SpatRaster, the output (also a SpatRaster) is automatically plotted. Map categories have a set colour table, built with terra::coltab().}
  \item{\dots}{additional arguments to pass to terra::plot() (if 'pred' is a SpatRaster and plot=TRUE), such as 'mar', 'axes' or 'legend'.}
}
\value{
This function returns a categorical (factor) vector (or a categorical 'SpatRaster' if 'pred' is of that class) of the same length as 'pred', or of the same number of rows as the data in 'model', containing the confusion matrix label for each value.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{confusionMatrix}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

confusionLabel(model = mod, thresh = 0.5)


# you can instead use vectors of observed and predicted values:

presabs <- mod$y
prediction <- mod$fitted.values

confusionLabel(obs = presabs, pred = prediction, thresh = 0.5)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
