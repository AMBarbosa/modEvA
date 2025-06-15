\name{optiPair}
\alias{optiPair}
\title{
Optimize the classification threshold for a pair of related model evaluation measures.
}
\description{
This function can optimize a model's classification threshold based on a pair of model evaluation measures that balance each other, such as sensitivity-specificity, precision-recall (i.e., positive predictive power vs. sensitivity), or omission-commission, or underprediction-overprediction (Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013). The function plots both measures of the given pair against all thresholds with a given interval, and calculates the optimal sum, difference and mean of the two measures.
}
\usage{
optiPair(model = NULL, obs = NULL, pred = NULL,
measures = c("Sensitivity", "Specificity"), interval = 0.01, pbg = FALSE,
plot = TRUE, plot.sum = FALSE, plot.diff = FALSE, col1 = "darkblue", 
col2 = "lightblue3", ylim = NULL, na.rm = TRUE, exclude.zeros = TRUE, 
rm.dup = FALSE, verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{measures}{a character vector of length 2 indicating the pair of measures whose curves to plot and whose combined threshold to optimize. Available measures can be obtained with 'modEvAmethods("threshMeasures")', but note that this function expects you to use two measures that counter-balance one another, such as c("Sensitivity", "Specificity") [the default], c("Omission", "Commission"), or c("Precision", "Recall").}
  \item{interval}{the interval of thresholds at which to calculate the measures. The default is 0.01.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{plot}{logical indicating whether or not to plot the pair of measures.}
  \item{plot.sum}{logical, whether to plot the sum (+) of both measures in the pair. Defaults to FALSE.}
  \item{plot.diff}{logical, whether to plot the difference (-) between both measures in the pair. Defaults to FALSE.}
  \item{col1}{plot colour for the 1st element in 'measures'.}
  \item{col2}{plot colour for the 2nd element in 'measures'.}
  \item{ylim}{a character vector of length 2 indicating the lower and upper limits for the y axis. The default is NULL for an automatic definition of 'ylim' based on the values of the measures and their sum and/or difference if any of these are set to TRUE.}
  \item{na.rm}{logical, whether NA values should be removed from the calculation of minimum/maximum/mean values to get the optimized measures. Defaults to TRUE.}
  \item{exclude.zeros}{logical, whether non-finite and zero values should be removed from the calculation of minimum/maximum/mean values to get the optimized measures. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to be passed to the \code{\link{plot}} function.}
}
\value{
The output is a list with the following components:
\item{measures.values}{a data frame with the values of the chosen pair of measures, as well as their difference, sum and mean, at each threshold.}
\item{MinDiff}{numeric value, the minimum difference between both measures.}
\item{ThreshDiff}{numeric value, the threshold that minimizes the difference between both measures.}
\item{MaxSum}{numeric value, the maximum sum of both measures.}
\item{ThreshSum}{numeric value, the threshold that maximizes the sum of both measures.}
\item{MaxMean}{numeric value, the maximum mean of both measures.}
\item{ThreshMean}{numeric value, the threshold that maximizes the mean of both measures.}
If plot=TRUE (the default), a plot is also produced with the value of each of 'measures' at each threshold, and horizontal and vertical lines marking, respectively, the threshold and value at which the difference between the two 'measures' is minimal.
}
\references{
Barbosa, A.M., Real, R., Munoz, A.-R. & Brown, J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions 19: 1333-1338

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Liu C., White M., & Newell G. (2011) Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography, 34, 232-243.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{getThreshold}}, \code{\link{optiThresh}}, \code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)


# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

optiPair(model = mod)

optiPair(model = mod, measures = c("Precision", "Recall"))

optiPair(model = mod, measures = c("UPR", "OPR"))

optiPair(model = mod, measures = c("CCR", "F1score"))


# you can also use 'optiPair' with vectors of observed
# and predicted values, instead of a model object:

optiPair(obs = mod$y, pred = mod$fitted.values)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
