\name{applyThreshold}
\alias{applyThreshold}
\title{Apply threshold(s) to model predictions}
\description{
This function applies a threshold value to the continuous predictions of a model, converting them to binary predictions: 1 for values above the threshold, and 0 for values below it. If two thresholds are provided (e.g. to separate high, low and intermediate predictions), the result is 0 below the lowest threshold, 1 above the highest threshold, and 0.5 between them.
}
\usage{
applyThreshold(model = NULL, obs = NULL, pred = NULL, thresh, pbg = FALSE,
right = FALSE, interval = 0.01, quant = 0, na.rm = TRUE, verbosity = 2)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{thresh}{numeric vector of length 1 or 2, containing the threshold value(s) with which to reclassify 'pred', or the criteria under which to compute these thresholds -- run modEvAmethods("getThreshold") for available options, and see Details in \code{\link{getThreshold}} for their description.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{right}{logical value indicating if the interval should be closed on the right (and open on the left) or vice versa, i.e., if predictions equalling the threshold value(s) should be classified as lower rather than higher. The default is FALSE.}
  \item{interval}{Argument to pass to \code{\link{optiThresh}} indicating the interval between the thresholds to test, if 'thresh' implies optimizing a threshold-based measure. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{Numeric value indicating the proportion of presences to discard if any of 'thresh' is "MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
  \item{na.rm}{Logical value indicating whether NA values should be ignored. Defaults to TRUE.}
      \item{verbosity}{integer value indicating the amount of messages to print. Defaults to 2, for the maximum amount of messages.}
}
\details{
Several criteria have been proposed for selecting thresholds with which to convert continuous model predictions (of presence probability, habitat suitability or alike) into binary predictions of presence or absence. A threshold is required for computing threshold-based model evaluation metrics, such as those in \code{\link{threshMeasures}}. This function reclassifies the predictions of a model given one or two numeric thresholds, or one or two threshold selection criteria implemented in \code{\link{getThreshold}}.
}
\value{
This function returns an object of the same class as 'pred' with the reclassified values after application of the \code{thresh}old.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{getThreshold}}, \code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

applyThreshold(model = mod, thresh = "maxTSS")


# you can also use applyThreshold with vectors of observed and predicted values:

presabs <- mod$y
prediction <- mod$fitted.values

applyThreshold(pred = prediction, thresh = 0.5)

applyThreshold(pred = prediction, thresh = c(0.2, 0.8))

applyThreshold(pred = prediction, thresh = "meanPred")

applyThreshold(obs = presabs, pred = prediction, thresh = "preval")

applyThreshold(obs = presabs, pred = prediction, thresh = "MTP")

applyThreshold(obs = presabs, pred = prediction, thresh = "MTP",
quant = 0.05)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
