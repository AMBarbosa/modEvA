\name{Boyce}
\alias{Boyce}
\title{
Boyce Index
}
\description{
This function computes the (continuous) Boyce index (Boyce 2002; Hirzel et al. 2006) for either: 1) a model object; or 2) two paired numeric vectors of observed (binary, 1 for occurrence vs. 0 for no occurrence records) and predicted (continuous, e.g. environmental suitability) values; or 3) a set of presence point coordinates and a raster map with the predicted values for the entire model evaluation area.

This metric is designed for evaluating model predictions against presence/background data (i.e. presence/available, where "available" includes both occupied and unoccupied sites; Boyce 2002), so the function uses the model predictions for the presence sites (ones) against the predictions for the entire dataset (ones and zeros).
}
\usage{Boyce(model = NULL, obs = NULL, pred = NULL, n.bins = NA,
bin.width = "default", res = 100, method = "spearman", rm.dup.classes = FALSE,
rm.dup.points = FALSE, pbg = FALSE, simplif = FALSE, plot = TRUE, 
plot.lines = TRUE, plot.values = TRUE, plot.digits = 3, 
xlim = c(0, 1), ylim = "auto", na.rm = TRUE, verbosity = 2, ...)}
\arguments{
  \item{model}{optional binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments (e.g. for external test data) instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{n.bins}{number of classes or bins (e.g. 10) in which to group the 'pred' values, or a vector with the bin thresholds. If \code{n.bins = NA} (the default), a moving window is used (see next parameters), so as to compute the "continuous Boyce index" (Hirzel et al. 2006).}
  \item{bin.width}{width of the moving window (if n.bins = NA), in the units of 'pred' (e.g. 0.1). By default, it is 1/10th of the 'pred' range.}
  \item{res}{resolution of the moving window (if n.bins = NA). By default it is 100 focals, providing 100 moving bins).}
  \item{method}{argument to be passed to \code{\link{cor}} indicating which correlation coefficient to use. The default is \code{'spearman'} as per Boyce et al. (2002), but \code{'pearson'} and \code{'kendall'} could be more informative in many cases.}
  \item{rm.dup.classes}{Logical, default FALSE. If \code{TRUE} (as in 'ecospat::ecospat.boyce') and if there are different bins with the same predicted/expected ratio, only one of each is used to compute the correlation (see Examples).}
  \item{rm.dup.points}{Logical (default FALSE); if \code{TRUE} and if 'pred' is a SpatRaster and if there are >1 points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}.}
  \item{pbg}{logical (default FALSE) to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data.}
  \item{simplif}{logical value (default FALSE) indicating whether to return just a simplified output containing only the value estimate, with no associated statistics.}
  \item{plot}{logical (default TRUE), whether or not to plot the predicted/expected ratio against the median prediction of each bin.}
  \item{plot.lines}{logical (default TRUE), whether or not to add lines connecting the points in the plot (if plot=TRUE).}
  \item{plot.values}{logical (default TRUE), whether or not to show in the plot the value of the Boyce index.}
  \item{plot.digits}{number of digits to which the value in the plot should be \code{\link{round}}ed (if 'plot' and 'plot.values' are TRUE). Defaults to 3.}
    \item{xlim}{either "auto" for fitting the x axis to the existing values, or a length-2 numeric vector setting its limits. The default is c(0, 1).}
    \item{ylim}{either "auto" (default) for fitting the y axis to the existing values, or a length-2 numeric vector setting its limits (e.g. to compare Boyce plots across different datasets).}
    \item{na.rm}{Logical (default TRUE) value indicating if missing values should be removed from computations.}
    \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{some additional arguments can be passed to \code{\link{plot}}, e.g. 'main' or 'xlim'.}
}
\details{
The Boyce index is the correlation between model predictions and area-adjusted frequencies (i.e., observed vs. expected proportion of occurrences) along different prediction classes (bins). In other words, it measures how model predictions differ from a random distribution of the observed presences across the prediction gradient (Boyce et al. 2002). It can take values between -1 and 1. Positive values indicate that presences are more frequent than expected by chance (given availability) in areas with higher predicted values. Values close to zero mean that predictions are no better than random (i.e. presences are distributed among prediction classes as expected by chance), and negative values indicate counter predictions (i.e., presences are proportionally more frequent in areas with lower predicted values).

The R code is largely based on the 'ecospat.boyce' function of the \pkg{ecospat} package (version 3.2.1), but it is modified to match the input formats in the remaining functions of 'modEvA', and to return a more complete and informative output. It also does not 'rm.dup.classes' by default, like the 'ecospat' version does (though the user can control this parameter); and it implements a bug fix identified at https://github.com/ecospat/ecospat/issues/99#issue-2528136160.
}
\note{
This index is designed for evaluating predictions of habitat suitability, not presence probability (which also depends on the species' presence/absence ratio: rare species do not usually show high proportions of presences, even in highly suitable areas). If your predictions are of presence probability based on a training prevalence different from 50\% presences, it is generally better to convert those predictions e.g. with the \code{Fav} function of package \pkg{fuzzySim}, before evaluating them with the Boyce index.

In bins with overly small sample sizes, the comparison between median prediction and random expectation may not be meaningful, although these bins will equally contribute to the overall Boyce index. When there are bins with fewer than 15 values (the minimum required for reliable comparisons within bins; Jovani & Tella 2006, Jimenez-Valverde et al. 2013), a warning is emitted and their values are plotted as red triangles. See the $bins$bin.N section of the console output, and use the 'bin.width' argument to enlarge the bins if necessary.
}
\value{
This function returns a \code{list} with the following components:
\item{bins}{a data frame with the number of values in each bin, their median and range of predicted values, and the corresponding predicted/expected ratio of presences.}
\item{B}{the numeric value of the Boyce index, i.e. the coefficient of correlation between the median predicted value in each bin and the corresponding predicted/expected ratio.}
If plot=TRUE (the default), the function also plots the predicted/expected ratio for the utilized bins along the prediction range. A good model should yield a monotonically increasing curve (but see Note).
}
\author{
A. Marcia Barbosa, with significant chunks of code from the 'ecospat::ecospat.boyce' function by Blaise Petitpierre and Frank Breiner (\pkg{ecospat} package version 3.2.1).
}
\references{
Boyce, M.S., P.R. Vernier, S.E. Nielsen & F.K.A. Schmiegelow (2002) Evaluating resource selection functions. Ecological Modelling 157: 281-300

Hirzel, A.H., G. Le Lay, V. Helfer, C. Randin & A. Guisan (2006) Evaluating the ability of habitat suitability models to predict species presences. Ecological Modelling 199: 142-152

Jimenez-Valverde A., Acevedo P., Barbosa A.M., Lobo J.M. & Real R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516

Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218
}
\seealso{
\code{\link{Miller}}; \code{\link{HLfit}}; \code{ecospat::ecospat.boyce}; \code{enmSdmX::evalContBoyce}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

# compute the Boyce index:
Boyce(model = mod, main = "My model Boyce plot")
Boyce(model = mod, main = "My model Boyce plot", rm.dup.classes = TRUE)


# you can also use vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

Boyce(obs = presabs, pred = prediction)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
