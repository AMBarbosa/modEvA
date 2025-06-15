\name{optiThresh}
\alias{optiThresh}
\title{
Optimize threshold for model evaluation.
}
\description{
This function computes a number of model evaluation measures at optimal thresholds (see \code{\link{threshMeasures}}). Optimization is given for each measure, and/or for all measures according to particular criteria (e.g. Jimenez-Valverde & Lobo 2007; Liu et al. 2005; Nenzen & Araujo 2011). Results are given numerically and in plots. See \code{\link{getThreshold}} if you're after the optimized threshold rather than the threshold-optimized evaluation measures.}
\usage{
optiThresh(model = NULL, obs = NULL, pred = NULL, interval = 0.01,
measures = c(modEvAmethods("threshMeasures"), modEvAmethods("similarity")),
optimize = modEvAmethods("optiThresh"), simplif = FALSE, pbg = FALSE,
plot = TRUE, sep.plots = FALSE, reset.par = TRUE, xlab = "Threshold",
na.rm = TRUE, rm.dup = FALSE, pch = 20, cex = 0.2, col = "darkblue", 
verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{interval}{numeric value between 0 and 1 indicating the interval between the thresholds at which to calculate the evaluation measures. Defaults to 0.01.}
  \item{measures}{character vector indicating the names of the model evaluation measures for which to calculate optimal thresholds. The default is using all measures available in 'c(modEvAmethods("threshMeasures"), modEvAmethods("similarity"))'.}
  \item{optimize}{character vector indicating the threshold optimization criteria to use; "each" calculates the optimal threshold for each model evaluation measure, while the remaining options optimize all measures according to the specified criterion. The default is using all criteria available in 'modEvAmethods("optiThresh")'.}
  \item{simplif}{logical, whether to compute a faster simplified version. Used internally in other functions.}
  \item{plot}{logical, whether to plot the values of each evaluation measure at all thresholds. Ignored if simplif=TRUE.}
  \item{sep.plots}{logical. If TRUE, each plot is shown separately (you need to be recording R plot history to be able to browse through them all); if FALSE (the default), plots are combined (using \code{\link{arrangePlots}}) in the same plotting window; if NA, the current \code{par()["mfrow"]} is used.}
  \item{reset.par}{logical. If TRUE (the default), plotting \code{\link{par}}ameters are reset in the end. FALSE can be useful if the user wants to set their own parameters and combine this with other plots.}
  \item{xlab}{character vector indicating the label of the x axis.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{pch}{plotting character (symbol), argument to pass to \code{\link{plot}}.}
  \item{cex}{character expansion (size), argument to pass to \code{\link{plot}}.}
  \item{col}{colour, argument to pass to \code{\link{plot}}.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments that can be passed to \code{\link{plot}}.}
}
\value{
This function returns a list with the following components:
  \item{all.thresholds}{a data frame with the values of all analysed measures at all analysed thresholds.}
  \item{optimals.each}{if "each" is among the threshold criteria specified in 'optimize', optimals.each is output as a data frame with the value of each measure at its optimal threshold, as well as the type of optimal for that measure (which may be the maximum for measures of goodness such as "Sensitivity", or the minimum for measures of badness such as "Omission").}
  \item{optimals.criteria}{a data frame with the values of measure at the threshold that maximizes each of the criteria specified in 'optimize' (except for "each", see above).}
}
\note{
"Sensitivity" is the same as "Recall", and "PPP" (positive predictive power) is the same as "Precision". "F1score" is the harmonic mean of precision and recall.
}
\references{
Jimenez-Valverde A. & Lobo J.M. (2007) Threshold criteria for conversion of probability of species presence to either-or presence-absence. Acta Oecologica 31: 361-369.

Liu C., Berry P.M., Dawson T.P. & Pearson R.G. (2005) Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28: 385-393.

Nenzen H.K. & Araujo M.B. (2011) Choice of threshold alters projections of species range shifts under climate change. Ecological Modelling 222: 3346-3354.
}
\author{
A. Marcia Barbosa
}
\note{
Some measures cannot be calculated for thresholds at which there are zeros in the confusion matrix, hence the eventual 'NaN' or 'Inf' in results. Also, optimization may be deceiving for some measures; use 'plot = TRUE' and inspect the plot(s).
}
\seealso{
\code{\link{getThreshold}}, \code{\link{optiPair}}, \code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

\dontrun{
optiThresh(model = mod)
}


# change some of the parameters:

optiThresh(model = mod, pch = 20,
measures = c("CCR", "Sensitivity", "kappa", "TSS", "Jaccard", "F1score"),
ylim = c(0, 1))


# you can also use optiThresh with vectors of observed and predicted
# values instead of with a model object:

\dontrun{
optiThresh(obs = mod$y, pred = mod$fitted.values, pch = 20)
}


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
