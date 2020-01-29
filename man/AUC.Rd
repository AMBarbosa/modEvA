\name{AUC}
\alias{AUC}
\title{
Area Under the Curve
}
\description{
This function calculates the Area Under the Curve of the receiver operating characteristic (ROC) plot, or alternatively the precision-recall (PR) plot, for either a model object of class \code{"glm"}, or two matching vectors of observed (binary, 1 for occurrence vs. 0 for non-occurrence) and predicted (continuous, e.g. occurrence probability) values, respectively.
}
\usage{
AUC(model = NULL, obs = NULL, pred = NULL, simplif = FALSE, 
interval = 0.01, FPR.limits = c(0, 1), curve = "ROC",
method = "rank", plot = TRUE, diag = TRUE, diag.col = "grey", 
diag.lty = 1, curve.col = "black", curve.lty = 1, curve.lwd = 2,
plot.values = TRUE, plot.digits = 3, plot.preds = FALSE, 
grid = FALSE, xlab = "auto", ylab = "auto", ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another
  binary response variable. This argument is ignored if \code{model} is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as \code{obs}. This argument is ignored if \code{model} is provided.}
  \item{simplif}{logical, whether to use a faster version that returns only the AUC value (and the ROC plot if plot = TRUE).}
  \item{FPR.limits}{(NOT YET IMPLEMENTED) numerical vector of length 2 indicating the limits of false positive rate between which to calculate a partial AUC. The default is c(0, 1), for considering the whole AUC.}
  \item{curve}{character indicating whether to compute the "ROC" (receiver operating charateristic) or the "PR" (precision-recall) curve. Area calculation is not currently implemented for this curve.}
  \item{interval}{interval of threshold values at which to calculate the true and false positives and negatives. Defaults to 0.01. Note that, if method = "rank" (the default if curve = "ROC"), this does not affect the obtained AUC value (although it can affect the size of the plotted curve, especially when prevalence is low), as the AUC is calculated with the Mann-Whitney-Wilcoxon statistic and is therefore threshold-independent. If method != "rank" (or, by extension, if curve = "PR"), setting 'interval' to smaller values will provide more accurate AUC values. The size of the 'interval' also affects the resulting 'meanPrecision', as this is averaged across all threshold values.}
  \item{method}{character indicating which method should be used to calculate the AUC. Available options are "rank" (the default and most exact, but implemented only if curve = "ROC"), "trapezoid" (the default if curve = "PR"), or "integrate". The last two compute more exactly if 'interval' is decreased (see above).}
  \item{plot}{logical, whether or not to plot the curve. Defaults to TRUE.}
  \item{diag}{logical, whether or not to add the reference diagonal (if plot = TRUE). Defaults to TRUE.}
  \item{diag.col}{line colour for the reference diagonal (if diag=TRUE).}
  \item{diag.lty}{line type for the reference diagonal (if diag=TRUE).}
  \item{curve.col}{line colour for the curve.}
  \item{curve.lty}{line type for the curve.}
  \item{curve.lwd}{line width for the curve.}
  \item{plot.values}{logical, whether or not to show in the plot the values associated to the curve (e.g., the AUC). Defaults to TRUE.}
  \item{plot.digits}{integer number indicating the number of digits to which the values in the plot should be \code{\link{round}}ed. Defaults to 3. This argument is ignored if 'plot' or 'plot.values' are set to FALSE.}
  \item{plot.preds}{logical value indicating whether the proportions of analysed model predictions should be plotted as proportionally sized circles for each threshold. Can also be provided as a character value specifying if the circles should be plotted only on the "curve" or at the "bottom" of the plot. The default is FALSE.}
  \item{grid}{logical, whether or not to add a grid to the plot, marking the analysed thresholds. Defaults to FALSE.}
  \item{xlab}{label for the x axis. By default, a label is automatically generated according to the specified 'curve'.}
  \item{ylab}{label for the y axis. By default, a label is automatically generated according to the specified 'curve'.}
  \item{\dots}{further arguments to be passed to the \code{\link{plot}} function.}
}
\details{
In the case of the "ROC" curve (the default), the AUC is a measure of the overall discrimination power of the predictions, or the probability that an occurrence site has a higher predicted value than a non-occurrence site. It can thus be calculated with the Wilcoxon rank sum statistic. Mind that the AUC has been widely criticized (e.g. Lobo et al. 2008, Jimenez-Valverde et al. 2013), but is still among the most widely used metrics in model evaluation. It is highly correlated with species prevalence, so prevalence is also output by the AUC function (if \code{simplif = FALSE}, the default) for reference. The AUC function provides an option to compute, instead of the ROC curve, the precision-recall ("PR") curve, which is more robust to imbalanced data, e.g. species rarity (Sofaer et al. 2019).

Although there are functions to calculate the AUC in other R packages (e.g. \pkg{ROCR}, \pkg{PresenceAbsence}, \pkg{verification}, \pkg{Epi}, \pkg{PRROC}, \pkg{PerfMeas}, \pkg{precrec}), the \code{AUC} function is more compatible with the remaining functions in \pkg{modEvA} and can be applied not only to a set of observed versus predicted values, but also directly to a model object of class \code{"glm"}.
}
\value{
If \code{simplif = TRUE}, the function returns only the AUC value (a numeric value between 0 and 1). Otherwise (the default), it returns a \code{list} with the following components:
\item{thresholds}{a data frame of the true and false positives, the sensitivity, specificity and recall of the predictions, and the number of predicted values at each analysed threshold.}
\item{N}{the total number of obervations.}
\item{prevalence}{the proportion of presences (i.e., ones) in the data (which correlates with the AUC of the "ROC" plot).}
\item{AUC}{the value of the AUC).}
\item{AUCratio}{the ratio of the obtained AUC value to the null expectation (0.5).}
\item{meanPrecision}{the arithmetic mean of precision (proportion of predicted presences actually observed as presences) across all threshold values (defined by 'interval'). It is close to the AUC of the precision-recall (PR) curve.}
}
\references{
Lobo, J.M., Jimenez-Valverde, A. & Real, R. (2008). AUC: a misleading measure of the performance of predictive distribution models. Global Ecology and Biogeography 17: 145-151

Jimenez-Valverde, A., Acevedo, P., Barbosa, A.M., Lobo, J.M. & Real, R. (2013). Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516

Sofaer, H.R., Hoeting, J.A. & Jarnevich, C.S. (2019). The area under the precision-recall curve as a performance metric for rare binary events. Methods in Ecology and Evolution, 10: 565-577
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


AUC(model = mod, simplif = TRUE)

AUC(model = mod, curve = "PR")

AUC(model = mod, interval = 0.1, grid = TRUE)

AUC(model = mod, plot.preds = "curve")


# you can also use AUC with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

AUC(obs = presabs, pred = prediction)
}
