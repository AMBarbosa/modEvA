\name{AUC}
\alias{AUC}
\title{
Area Under the Curve
}
\description{
This function calculates the Area Under the Curve of the receiver operating characteristic (ROC) plot, or alternatively the precision-recall (PR) plot, for either a model object or two matching vectors of observed binary (1 for occurrence vs. 0 for non-occurrence) and predicted continuous (e.g. occurrence probability) values, respectively.
}
\usage{
AUC(model = NULL, obs = NULL, pred = NULL, simplif = FALSE,
interval = "auto", FPR.limits = c(0, 1), curve = "ROC", pbg = FALSE,
method = NULL, plot = TRUE, diag = TRUE, diag.col = "lightblue3",
diag.lty = 2, curve.col = "darkblue", curve.lty = 1, curve.lwd = 2,
plot.values = TRUE, plot.digits = 3, plot.preds = FALSE,
grid = FALSE, grid.lty = 1, xlab = "auto", ylab = "auto", cex.lab = 0.9,
ticks = FALSE, na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{simplif}{logical, whether to use a faster version that returns only the AUC value (and the plot if 'plot = TRUE').}
  \item{FPR.limits}{(NOT YET IMPLEMENTED) numerical vector of length 2 indicating the limits of false positive rate between which to calculate a partial AUC. The default is c(0, 1), for considering the whole AUC. Pending implementation. Meanwhile, you can try e.g. the \code{roc} function in the \pkg{pROC} package.}
  \item{curve}{character indicating whether to compute the "ROC" (receiver operating charateristic) or the "PR" (precision-recall) curve.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{interval}{interval of threshold values at which to compute and plot the true- and false-positive rates. Can be a (preferably small) numeric value between 0 and 1; or the new default "auto", which will be either 0.01 (old default) or 0.001, depending on whether 'pred' does or doesn't have values in all thirds of the [0, 1] range. Larger intervals provide faster computation, but may generate innacurate curves especially if 'pred' has too few values towards one of the extremes (e.g. towards 1). Note that, if method = "rank" (the default if curve = "ROC"), 'interval' does not affect the obtained AUC value (although it can visually affect the size of the plotted curve, especially when there are very few 'pred' values towards 0 or towards 1), as the AUC is calculated with the Mann-Whitney-Wilcoxon statistic and is therefore threshold-independent. If method != "rank" (or, by extension, if curve = "PR" -- see 'method' argument), smaller 'interval' values will provide more accurate AUC values. Smaller 'interval' values also improve the output 'meanPrecision', as this is averaged across all threshold values.}
  \item{method}{character indicating with which method to calculate the AUC value. Available options are "rank" (the default and most accurate, but implemented only if curve = "ROC") and "trapezoid" (the default if curve = "PR"). The latter may be computed more accurately if 'interval' is smaller (see 'interval' argument).}
  \item{plot}{logical, whether or not to plot the curve. Defaults to TRUE.}
  \item{diag}{logical, whether or not to add the reference diagonal (if plot = TRUE). Defaults to TRUE.}
  \item{diag.col}{line colour for the reference diagonal (if diag = TRUE).}
  \item{diag.lty}{line type for the reference diagonal (if diag = TRUE).}
  \item{curve.col}{line colour for the curve.}
  \item{curve.lty}{line type for the curve.}
  \item{curve.lwd}{line width for the curve.}
  \item{plot.values}{logical, whether or not to show in the plot the values associated to the curve (e.g., the AUC). Defaults to TRUE.}
  \item{plot.digits}{integer number indicating the number of digits to which the values in the plot should be \code{\link{round}}ed. Defaults to 3. This argument is ignored if 'plot' or 'plot.values' are set to FALSE.}
  \item{plot.preds}{logical value indicating whether the proportions of 'pred' values for each threshold should be plotted as proportionally sized blue circles. Can also be provided as a character vector specifying if the circles should be plotted on the "curve" (the default) and/or at the "bottom" of the plot. The default is FALSE for no circles, but it may be interesting to try it, especially if your curve has long straight lines or does not cover the full length of the plot.}
  \item{grid}{logical, whether or not to add a grid to the plot, marking the analysed thresholds. Defaults to FALSE.}
    \item{grid.lty}{line type for the grid (if grid = TRUE).}
  \item{xlab}{label for the x axis. By default, a label is automatically generated according to the specified 'curve'.}
  \item{ylab}{label for the y axis. By default, a label is automatically generated according to the specified 'curve'.}
  \item{cex.lab}{'cex' (amount by which the text should be magnified) for the axis labels. Default 0.9.}
  \item{ticks}{logical, whether or not to add blue tick marks at the bottom of the plot to mark the thresholds at which there were values from which to to draw the curve. Defaults to FALSE.}
  \item{na.rm}{Logical value indicating if missing values should be ignored in computations. The default is TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{further arguments to be passed to the \code{\link{plot}} function.}
}
\details{
In the case of the "ROC" curve (the default), the AUC is a measure of the overall discrimination power of the predictions, or the probability that an occurrence site has a higher predicted value than a non-occurrence site. It can thus be calculated with the Wilcoxon rank sum statistic, as is done with the default method="rank". There's also an option to compute, instead of the ROC curve, the precision-recall ("PR") curve, which is more robust to imbalanced data, e.g. species rarity (Sofaer et al. 2019), as it doesn't value true negatives.

If 'curve' is set to "PR", or if 'method' is manually set to "trapezoid", the AUC value will be more accurate if 'interval' is decreased (see 'method' and 'interval' arguments above). The plotted curve will also be more accurate with smaller 'interval' values, especially for imbalanced datasets (which can cause an apparent disagreement between the look of the curve and the actual value of the AUC).

Swets (1988) proposed the following thresholds for interpreting ROC AUC scores: below 0.6 = fail; 0.6 to 0.7 = poor; 0.7 to 0.8 = fair; 0.8 to 0.9 = good; 0.9 or higher = excellent. Mind that the ROC AUC has been widely criticized (e.g. Lobo et al. 2008, Jimenez-Valverde et al. 2013), though is still a widely used metric in model evaluation. It is highly correlated with species prevalence (as are the generality of discrimination and classification metrics, including TSS), so prevalence is also output by the AUC function (if \code{simplif = FALSE}, the default) for reference.

Although there are functions to calculate the AUC in other R packages (e.g. \pkg{ROCR}, \pkg{PresenceAbsence}, \pkg{verification}, \pkg{Epi}, \pkg{PRROC}, \pkg{PerfMeas}, \pkg{precrec}), the \code{AUC} function is more compatible (in terms of arguments and outputs) with the remaining functions in \pkg{modEvA}; and it can be applied not only to a set of observed vs. predicted values, but also directly to a model object of class "glm", "gam", "gbm", "randomForest" or "bart", or even to a set of presence point coordinates and a raster map of the predictions in the model evaluation region.
}
\value{
If \code{simplif = TRUE}, the function returns only the AUC value (a numeric value between 0 and 1). Otherwise (the default), it returns a \code{list} with the following components:
\item{thresholds}{a data frame of the true and false positives, the sensitivity, specificity and recall of the predictions, and the number of predicted values at each analysed threshold.}
\item{N}{the total number of obervations.}
\item{prevalence}{the proportion of presences (i.e., ones) in the data (which correlates with the AUC of the "ROC" plot).}
\item{AUC}{the value of the AUC).}
\item{AUCratio}{the ratio of the obtained AUC value to the null expectation (0.5).}
\item{meanPrecision}{the arithmetic mean of precision (proportion of predicted presences actually observed as presences) across all threshold values (defined by 'interval'). It is close to the AUC of the precision-recall (PR) curve.}
\item{GiniCoefficient}{the Gini coefficient (aka Gini index or Gini ratio), a measure of statistical dispersion intended to represent inequality, computed as 2*AUC-1.}
\item{SomersD}{Somers' D, a measure of ordinal association between two variables (i.e. a rank correlation), also used as a quality measure of binary choice, computed as 2*(AUC-0.5). It is mathematically equivalent to the Gini coefficient; they are both here to match the terminology familiar to different audiences.}
}
\references{
Lobo, J.M., Jimenez-Valverde, A. & Real, R. (2008) AUC: a misleading measure of the performance of predictive distribution models. Global Ecology and Biogeography 17: 145-151

Jimenez-Valverde, A., Acevedo, P., Barbosa, A.M., Lobo, J.M. & Real, R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516

Sofaer, H.R., Hoeting, J.A. & Jarnevich, C.S. (2019). The area under the precision-recall curve as a performance metric for rare binary events. Methods in Ecology and Evolution, 10: 565-577

Swets, J.A. (1988) Measuring the accuracy of diagnostic systems. Science, 240: 1285-1293 (doi: 10.1126/science.3287615)
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]


# compute the AUC:

AUC(model = mod)

AUC(model = mod, simplif = TRUE)

AUC(model = mod, curve = "PR")

AUC(model = mod, interval = 0.1, grid = TRUE)

AUC(model = mod, plot.preds = TRUE)

AUC(model = mod, ticks = TRUE)

AUC(model = mod, plot.preds = c("curve", "bottom"))


# you can also use vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

AUC(obs = presabs, pred = prediction)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
