\name{threshMeasures}
\alias{threshMeasures}
\title{
Threshold-based measures of model evaluation
}
\description{
This function calculates a number of measures for evaluating the classification accuracy of a species distribution (or ecological niche, or bioclimatic envelope...) model against observed presence-absence data (Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013), upon the choice of a threshold value above which the model is considered to predict that the species should be present.
}
\usage{
threshMeasures(model = NULL, obs = NULL, pred = NULL, thresh,
measures = modEvAmethods("threshMeasures"), simplif = FALSE, 
plot = TRUE, plot.ordered = FALSE, standardize = TRUE, 
verbosity = 2, ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another binary response variable. Not necessary (and ignored) if 'model' is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Not necessary (and ignored) if 'model' is provided.}
  \item{thresh}{numeric value of the threshold to separate predicted presences from predicted absences in 'pred'; can be "preval", to use the prevalence of 'obs' as the threshold, or any real number between 0 and 1.  See Details for an informed choice.}
  \item{measures}{character vector of the evaluation measures to use. By default, all measures available in 'modEvAmethods("threshMeasures")' are calculated.}
  \item{simplif}{logical, whether to calculate a faster, simplified version. Used internally by other functions in the package. Defaults to FALSE.}
  \item{plot}{logical, whether to produce a barplot of the calculated measures. Defaults to TRUE.}
  \item{plot.ordered}{logical, whether to plot the measures in decreasing order rather than in input order. Defaults to FALSE.}
  \item{standardize}{logical, whether to change measures that may range between -1 and +1 (namely kappa and TSS) to their corresponding value in the 0-to-1 scale (skappa and sTSS), so that they can compare directly to other measures (see \code{\link{standard01}}). The default is TRUE, but a message is displayed to inform the user about it.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to be passed to the \code{\link{plot}} function.}
}
\details{
The threshold value can be chosen according to a number of criteria (see e.g. Liu et al. 2005, Jimenez-Valverde & Lobo 2007, Nenzen & Araujo 2011). You can set 'thresh' to "preval" (species' prevalence or proportion of presences \bold{in the data input to this function}), or calculate optimal threshold values according to different criteria with the \code{\link{optiThresh}} or the \code{\link{optiPair}} function. If you are using "environmental favourability" as input 'pred' data (Real et al. 2006; see 'Fav' function in R package 'fuzzySim'), then the 0.5 threshold equates to using training prevalence in logistic regression (GLM with binomial error distribution and logit link function).

While most of these threshold-based measures range from 0 to 1, some of them (such as kappa and TSS) may range from -1 to 1 (Allouche et al. 2006), so their raw scores are not directly comparable. 'threshMeasures' includes an option (used by default) to standardize these measures to 0-1 (Barbosa 2015) using the \code{\link{standard01}} function, so that you obtain the standardized versions skappa and sTSS.

This function can also be used to calculate the agreement between different presence-absence (or other types of binary) data, as e.g. Barbosa et al. (2012) did for comparing mammal distribution data from atlas and range maps. Notice, however, that some of these measures, such as TSS or NMI, are not symmetrical (obs vs. pred is different from pred vs. obs).
}
\value{
If 'simplif = TRUE', the output is a numeric matrix with the name and value of each measure. If 'simplif = FALSE' (the default), the ouptut is a bar plot of the calculated measures and a list with the following components:
  \item{N}{the number of observations (records) in the analysis.}
  \item{Prevalence}{the prevalence (proportion of presences) in 'obs'.}
  \item{Threshold}{the threshold value used to calculate the 'measures'.}
  \item{ConfusionMatrix}{the confusion matrix obtained with the used threshold.}
  \item{ThreshMeasures}{a numeric matrix with the name and value of each measure.}
}
\note{
"Sensitivity" is the same as "Recall", and "PPP" (positive predictive power) is the same as "Precision". "F1score"" is the harmonic mean of precision and recall.
}
\references{
Allouche O., Tsoar A. & Kadmon R. (2006) Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS). Journal of Applied Ecology 43: 1223-1232.

Barbosa, A.M. (2015) Re-scaling of model evaluation measures to allow direct comparison of their values. The Journal of Brief Ideas, 18 Feb 2015, DOI: 10.5281/zenodo.15487

Barbosa A.M., Estrada A., Marquez A.L., Purvis A. & Orme C.D.L. (2012) Atlas versus range maps: robustness of chorological relationships to distribution data types in European mammals. Journal of Biogeography 39: 1391-1400

Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions 19: 1333-1338

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Jimenez-Valverde A. & Lobo J.M. (2007) Threshold criteria for conversion of probability of species presence to either-or presence-absence. Acta Oecologica 31: 361-369

Liu C., Berry P.M., Dawson T.P. & Pearson R.G. (2005) Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28: 385-393.

Liu C., White M. & Newell G. (2011) Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography 34: 232-243.

Nenzen H.K. & Araujo M.B. (2011) Choice of threshold alters projections of species range shifts under climate change. Ecological Modelling 222: 3346-3354

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245.
}
\author{
A. Marcia Barbosa
}
\note{
Some of these measures (like NMI, UPR, OPR, PPP, NPP) cannot be calculated for thresholds at which there are zeros in the confusion matrix.
}
\seealso{
\code{\link{optiThresh}}, \code{\link{optiPair}}, \code{\link{AUC}}
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
}
