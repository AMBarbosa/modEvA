\name{threshMeasures}
\alias{threshMeasures}
\title{
Threshold-based measures of model evaluation
}
\description{
This function calculates a number of measures (described in Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013; Wunderlich et al. 2019) for evaluating the classification accuracy of a species distribution (or ecological niche, or bioclimatic envelope) model against observed presence-(pseudo)absence data, upon the choice of a threshold value above which the model is considered to predict that the species should be present.
}
\usage{
threshMeasures(model = NULL, obs = NULL, pred = NULL, thresh,
measures = modEvAmethods("threshMeasures")[-grep("OddsRatio",
modEvAmethods("threshMeasures"))], simplif = FALSE, pbg = FALSE, plot = TRUE,
plot.type = "lollipop", ylim = "auto0", plot.ordered = FALSE, standardize = TRUE,
verbosity = 2, interval = 0.01, quant = 0, na.rm = TRUE, rm.dup = FALSE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{thresh}{threshold to separate predicted presences from predicted absences in 'model' or 'pred'; can be a numeric value between 0 and 1, or any of the options provided with \code{modEvAmethods("getThreshold")}. See Details in \code{\link{getThreshold}} for a description of the available options, and also Details below for a more informed choice.}
  \item{measures}{character vector of the evaluation metrics to use. By default, all metrics available through \code{modEvAmethods("threshMeasures")} are included, except for "OddsRatio" which usually yields overly large values that stand out in the plot.}
  \item{simplif}{logical, whether to calculate a faster, simplified version. Used internally by other functions in the package. Defaults to FALSE.}
    \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{plot}{logical, whether to produce a bar chart or (by default) a lollipop chart of the calculated measures. Defaults to TRUE.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "\code{\link{lollipop}}" (the default) or "\code{\link{barplot}}".}
    \item{ylim}{either a numeric vector of length 2 specifying the limits (minimum, maximum) for the y axis; or "auto" to fit the y axis to the existing minimum and maximum values; or "auto0" (the new default) to fit the top of the y axis to the maximum existing values, and the bottom to zero or the minimum existing value (whichever is lower).}
  \item{plot.ordered}{logical, whether to plot the measures in decreasing order rather than in input order. Defaults to FALSE.}
  \item{standardize}{logical, whether to change measures that may range between -1 and +1 (namely kappa and TSS) to their corresponding value in the 0-to-1 scale (skappa and sTSS), so that they can compare directly to other measures (see \code{\link{standard01}}). The default is TRUE, but a message is displayed to inform the user about it.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{interval}{Numeric value, used if 'thresh' is a threshold optimization method such as "maxKappa" or "maxTSS", indicating the interval between the thresholds to test. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{Numeric value indicating the proportion of presences to discard if thresh="MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{\dots}{additional arguments to be passed to the \code{\link{plot}} function.}
}
\details{
The metrics implemented in this function are based on the confusion (or contingency) matrix, and they are described in dedicated publications (Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013; Wunderlich et al. 2019). All of them require a threshold value to separate continuous into binary predictions.

The threshold value can be chosen according to a number of criteria (see e.g. Liu et al. 2005, 2013; Jimenez-Valverde & Lobo 2007; Nenzen & Araujo 2011). You can choose a fixed numeric value, or set 'thresh' to "preval" (species' prevalence or proportion of presences \bold{in the data input to this function}), or calculate optimal threshold values according to different criteria with the \code{\link{getThreshold}} function (see also \code{\link{optiThresh}} and \code{\link{optiPair}}). If you are using "environmental favourability" as input 'pred' data (Real et al. 2006; see 'Fav' function in R package \pkg{fuzzySim}), then the 0.5 threshold equates to using training prevalence in presence probability as given by logistic regression (GLM with binomial error distribution and logit link function).

While most of these threshold-based measures range from 0 to 1, some of them (such as kappa and TSS) may range from -1 to 1 (Allouche et al. 2006), so their raw scores are not directly comparable. 'threshMeasures' includes an option (used by default) to standardize these measures to 0-1 (Barbosa 2015) using the \code{\link{standard01}} function, so that you obtain the standardized versions skappa and sTSS.

This function can also be used to calculate the agreement between different presence-absence (or other types of binary) data, as e.g. Barbosa et al. (2012) did for comparing mammal distribution data from atlas and range maps. Notice, however, that some of these measures, such as TSS or NMI, are not symmetrical (obs vs. pred is different from pred vs. obs).

Note that most of these metrics are correlated with the species' prevalence (proportion of presences) in the modelled sample, so they can over-inflate the perception of model accuracy for restricted species (as does the \code{\link{AUC}} of the ROC curve). This is the case even for metrics initially alleged to be prevalence-independent, such as TSS (Allouche et al. 2006), as was later shown for real-world data (e.g. Somodi et al. 2017). See \code{\link{similarity}} for alternative, less prevalence-dependent metrics.
}
\value{
If 'simplif=TRUE', the output is a numeric matrix with the name and value of each measure. If 'simplif=FALSE' (the default), the ouptut is a list with the following components:
  \item{N}{the number of observations (records) in the analysis.}
  \item{Prevalence}{the prevalence (proportion of presences) in 'obs'.}
  \item{Threshold}{the threshold value used to calculate the 'measures'.}
  \item{ConfusionMatrix}{the confusion matrix obtained with the used threshold.}
  \item{ThreshMeasures}{a numeric matrix with the name and value of each measure.}
}
\references{
Allouche O., Tsoar A. & Kadmon R. (2006) Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS). Journal of Applied Ecology 43: 1223-1232.

Barbosa, A.M. (2015) Re-scaling of model evaluation measures to allow direct comparison of their values. The Journal of Brief Ideas, 18 Feb 2015, DOI: 10.5281/zenodo.15487

Barbosa A.M., Estrada A., Marquez A.L., Purvis A. & Orme C.D.L. (2012) Atlas versus range maps: robustness of chorological relationships to distribution data types in European mammals. Journal of Biogeography 39: 1391-1400

Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions 19: 1333-1338

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Jimenez-Valverde A. & Lobo J.M. (2007) Threshold criteria for conversion of probability of species presence to either-or presence-absence. Acta Oecologica 31: 361-369

Liu C., Berry P.M., Dawson T.P. & Pearson R.G. (2005) Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28: 385-393

Liu C., White M. & Newell G. (2011) Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography 34: 232-243

Liu C., White M. & Newell G. (2013) Selecting thresholds for the prediction of species occurrence with presence-only data. Journal of Biogeography, 40: 778-789

Nenzen H.K. & Araujo M.B. (2011) Choice of threshold alters projections of species range shifts under climate change. Ecological Modelling 222: 3346-3354

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245

Somodi I., Lepesi N., Botta-Dukat Z. (2017) Prevalence dependence in model goodness measures with special emphasis on true skill statistics. Ecology and Evolution, 7(3):863-872. doi: 10.1002/ece3.2654

Wunderlich R.F., Lin Y.-P., Anthony J., Petway J.R. (2019) Two alternative evaluation metrics to replace the true skill statistic in the assessment of species distribution models. Nature Conservation 35: 97-116
}
\author{
A. Marcia Barbosa
}
\note{
"Sensitivity" is the same as "Recall", and "PPP" (positive predictive power) is the same as "Precision".

Some of the implemented measures (like NMI, UPR, OPR, PPP, NPP) cannot be calculated for thresholds at which there are zeros in the confusion matrix, so they can yield NaN values.
}
\seealso{
\code{\link{similarity}}, \code{\link{optiThresh}}, \code{\link{optiPair}}, \code{\link{AUC}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

threshMeasures(model = mod, simplif = TRUE, thresh = 0.5)

threshMeasures(model = mod, thresh = "preval")

threshMeasures(model = mod, plot.ordered = TRUE, thresh = "preval")

threshMeasures(model = mod, measures = c("CCR", "TSS", "kappa"),
thresh = "preval")

threshMeasures(model = mod, plot.ordered = TRUE, thresh = "preval")


# you can also use threshMeasures with vectors of observed and
# predicted values instead of with a model object:

threshMeasures(obs = mod$y, pred = mod$fitted.values, thresh = "preval")


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
