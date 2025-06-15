\name{getThreshold}
\alias{getThreshold}
\title{Prediction threshold for a given criterion}
\description{
This function computes the prediction threshold under a given criterion.
}
\usage{
getThreshold(model = NULL, obs = NULL, pred = NULL, threshMethod,
interval = 0.01, quant = 0, na.rm = TRUE, verbosity = 2, pbg = FALSE)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{threshMethod}{Criterion under which to compute the threshold. Run modEvAmethods("getThreshold") for available options.}
  \item{interval}{Argument to pass to \code{\link{optiThresh}} indicating the interval between the thresholds to test. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{Numeric value indicating the proportion of presences to discard if threshMethod="MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
  \item{na.rm}{Logical value (default TRUE) indicating whether NA values should be ignored.}
  \item{verbosity}{integer indicating the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
}
\details{
Several criteria have been proposed for selecting a threshold with which to convert continuous model predictions (of presence probability, habitat suitability or alike) into binary predictions of presence or absence. Such threshold is required for computing threshold-based model evaluation metrics, such as those in \code{\link{threshMeasures}}. This function implements a few of these threshold selection criteria, including those outlined in Liu et al. (2005, 2013) and a couple more:

- "preval", "trainPrev": prevalence (proportion of presences) in the supplied 'model' or 'obs'

- "meanPred": mean predicted value in the supplied 'model' or 'pred'

- "midPoint": median predicted value in the supplied 'model' or 'pred'

- "maxKappa": threshold that maximizes Cohen's kappa

- "maxCCR", "maxOA", "maxOPS": threshold that maximizes the Correct Classification Rate, aka Overall Accuracy, aka Overall Prediction Success

- "maxF": threshold that maximizes the F value

- "maxSSS": threshold that maximizes the sum of sensitivity and specificity

- "maxTSS": threshold that maximizes the True Skill Statistic

- "maxSPR": threshold that maximizes the sum of precision and recall

- "minDSS": threshold that minimizes the difference between sensitivity and specificity

- "minDPR": threshold that minimizes the difference between precision and recall

- "minD01": threshold that minimizes the distance between the ROC curve and the 0,1 point

- "minD11": threshold that minimizes the distance between the PR curve and the 1,1 point

- "equalPrev": predicted and observed prevalence equalization

- "MTP": minimum training presence, or the lowest predicted value where presence is recorded in 'obs' or 'model'. Optionally, with the 'quant' argument, this threshold leaves out predicted values lower than the value for the lowest specified proportion of presences
}
\value{
This function returns a numeric value indicating the threshold selected under the specified 'threshMethod'.
}
\references{
Liu C., Berry P.M., Dawson T.P. & Pearson R.G. (2005) Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28: 385-393

Liu C., White M. & Newell G. (2013) Selecting thresholds for the prediction of species occurrence with presence-only data. Journal of Biogeography 40: 778-789
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{optiThresh}}, \code{\link{optiPair}}, \code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

getThreshold(model = mod, threshMethod = "maxTSS")


# you can also use getThreshold with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

getThreshold(obs = presabs, pred = prediction, threshMethod = "maxTSS")

getThreshold(obs = presabs, pred = prediction, threshMethod = "MTP")

getThreshold(obs = presabs, pred = prediction, threshMethod = "MTP",
quant = 0.05)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
