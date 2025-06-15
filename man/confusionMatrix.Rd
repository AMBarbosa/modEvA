\name{confusionMatrix}
\alias{confusionMatrix}
\title{
Confusion matrix
}
\description{
This function computes the confusion (or contingency) matrix for a binary-response model, containing the numbers of false positives, false negatives, true positives and true negatives, given a user-defined threshold value.
}
\usage{
confusionMatrix(model = NULL, obs = NULL, pred = NULL, thresh, interval = 0.01,
quant = 0, verbosity = 2, na.rm = TRUE, rm.dup = FALSE, plot = FALSE,
classes = FALSE, ...)
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
  \item{plot}{logical argument indicating whether to also plot the matrix as an image. The default is FALSE (for back-compatibility).}
  \item{classes}{logical argument indicating whether the matrix image (if plot=TRUE) should have qualitative colours, matching the output of \code{\link{confusionLabel}} for SpatRasters. The default is FALSE, in which case the colours are proportional to the values in each section of the matrix, and the palette can be user-specified with the 'col' argument for 'plot' (see Examples).}
  \item{\dots}{some additional arguments can be passed to \code{\link{image}} (and through to \code{\link{plot}}) if plot=TRUE, such as 'main', 'font.main' or 'cex.main' (not 'axes', 'xlab' or 'ylab', which are already defined by confusionMatrix).}
}
\value{
This function returns a data frame containing the four values of the confusion matrix.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{confusionLabel}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

confusionMatrix(model = mod, thresh = 0.5)

confusionMatrix(model = mod, thresh = 0.5, plot = TRUE)

confusionMatrix(model = mod, thresh = 0.5, plot = TRUE,
col = hcl.colors(100, "blues"))

confusionMatrix(model = mod, thresh = 0.5, plot = TRUE, classes = TRUE,
main = "Confusion matrix")


# you can instead use vectors of observed and predicted values:

presabs <- mod$y
prediction <- mod$fitted.values

confusionMatrix(obs = presabs, pred = prediction, thresh = 0.5, plot = TRUE)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
