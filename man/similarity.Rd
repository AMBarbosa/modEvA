\name{similarity}
\alias{similarity}
\title{
Similarity measures
}
\description{
This function computes similarity indices for evaluating the classification accuracy of a species distribution (or ecological niche, or bioclimatic envelope...) model against observed presence-absence data, upon the choice of a threshold value above which the model is considered to predict that the species is expected to be present rather than absent. These metrics were proposed for model evaluation by Li & Guo (2013) and Leroy et al. (2018) -- see Details.
}
\usage{
similarity(model = NULL, obs = NULL, pred = NULL, thresh,
measures = modEvAmethods("similarity"), simplif = FALSE, pbg = FALSE,
plot = TRUE, plot.type = "lollipop", plot.ordered = FALSE, verbosity = 2,
interval = 0.01, quant = 0, na.rm = TRUE, rm.dup = FALSE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{thresh}{threshold to separate predicted presences from predicted absences in 'model' or 'pred'; can be a numeric value between 0 and 1, or any of the options provided with \code{modEvAmethods("getThreshold")}. See Details in \code{\link{getThreshold}} for a description of the available options, and also Details below for a more informed choice.}
  \item{measures}{character vector of the similarity indices to use. By default, all metrics available through \code{modEvAmethods("similarity")} are included.}
  \item{simplif}{logical, whether to calculate a faster, simplified version. Used internally by other functions in the package. Defaults to FALSE.}
    \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{plot}{logical, whether to produce a bar chart or (by default) a lollipop chart of the calculated measures. Defaults to TRUE.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "\code{\link{lollipop}}" (the default) or "\code{\link{barplot}}".}
  \item{plot.ordered}{logical, whether to plot the measures in decreasing order rather than in input order. Defaults to FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{interval}{Numeric value, used if 'thresh' is a threshold optimization method such as "maxKappa" or "maxTSS", indicating the interval between the thresholds to test. The default is 0.01. Smaller values may provide more precise results but take longer to compute.}
  \item{quant}{Numeric value indicating the proportion of presences to discard if thresh="MTP" (minimum training presence). With the default value 0, MTP will be the threshold at which all observed presences are classified as such; with e.g. quant=0.05, MTP will be the threshold at which 5\% presences will be classified as absences.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{\dots}{additional arguments to be passed to the \code{\link{plot}} function, e.g. ylim=c(0, 1).}
}
\details{
Commonly used threshold-based metrics of model evaluation, such as the True Skill Statistic (TSS) implemented in the \code{\link{threshMeasures}} function, are conditioned by species prevalence in the modelled sample. To overcome this, Leroy et al. (2018) propose using the similary indices of Sorensen and Jaccard for model evaluation, which they show to be (unlike the TSS) independent of prevalence. This function implements such indices in a model evaluation context.

Leroy et al. (2018) point out that Sorensen's index is equivalent to the F-measure (or F1 score, which is also implemented in the \code{\link{threshMeasures}} function), and that Jaccard's index is half the proxy of the F-measure previously proposed by Li & Guo (2013) for evaluating presence-background models.
}
\value{
If 'simplif=TRUE', the output is a numeric matrix with the name and value of each measure. If 'simplif=FALSE' (the default), the ouptut is a list with the following components:
  \item{N}{the number of observations (records) in the analysis.}
  \item{Threshold}{the threshold value used to calculate the 'measures'.}
  \item{similarity}{a numeric matrix with the name and value of each measure.}
}
\references{
Leroy B., Delsol R., Hugueny B., Meynard C.M., Barhoumi C., Barbet-Massin M. & Bellard C. (2018) Without quality presence-absence data, discrimination metrics such as TSS can be misleading measures of model performance. Journal of Biogeography 45(9):1994-2002

Li W. & Guo Q. (2013) How to assess the prediction accuracy of species presence-absence models without absence data? Ecography 36(7):788-799
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{optiThresh}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

similarity(model = mod, thresh = 0.5)

similarity(model = mod, thresh = 0.5, simplif = TRUE, ylim = c(0, 1))

similarity(model = mod, thresh = "maxJaccard")
# or thresh = "maxTSS", "MTP", etc.

# you can also use similarity with vectors of observed and
# predicted values instead of with a model object:

similarity(obs = mod$y, pred = mod$fitted.values, thresh = "maxJaccard")


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
