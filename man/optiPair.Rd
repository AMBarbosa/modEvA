\name{optiPair}
\alias{optiPair}
\title{
Optimize the classification threshold for a pair of related model evaluation measures.
}
\description{
This function can optimize a model's classification threshold based on a pair of model evaluation measures that balance each other, such as sensitivity-specificity, precision-recall (i.e., positive predictive power vs. sensitivity), omission-commission, or underprediction-overprediction (Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013). The function plots both measures of the given pair against all thresholds with a given interval, and calculates the optimal sum, difference and mean of the two measures.
}
\usage{
optiPair(model = NULL, obs = NULL, pred = NULL,
measures = c("Sensitivity", "Specificity"), interval = 0.01, 
plot = TRUE, plot.sum = FALSE, plot.diff = FALSE, ylim = NULL, ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another
  binary response variable. This argument is ignored if 'model' is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence
  probability, habitat suitability, environmental favourability or alike. This argument is ignored if 'model' is provided.}
  \item{measures}{a character vector of length 2 indicating the pair of measures
  whose curves to plot and whose thresholds to optimize. The default is
  c("Sensitivity", "Specificity").}
  \item{interval}{the interval of thresholds at which to calculate the measures. The default is 0.01.}
  \item{plot}{logical indicating whether or not to plot the pair of measures.}
  \item{plot.sum}{logical, whether to plot the sum (+) of both measures in the pair. Defaults to FALSE.}
  \item{plot.diff}{logical, whether to plot the difference (-) between both measures in the pair. Defaults to FALSE.}
  \item{ylim}{a character vector of length 2 indicating the lower and upper limits for the y axis. The default is NULL for an automatic definition of 'ylim' based on the values of the measures and their sum and/or difference if any of these are set to TRUE.}
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
}
\note{
"Sensitivity" is the same as "Recall", and "PPP" (positive predictive power) is the same as "Precision". "F1score"" is the harmonic mean of precision and recall.
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
\code{\link{optiThresh}}, \code{\link{threshMeasures}}
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
}
