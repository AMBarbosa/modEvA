\name{HLfit}
\alias{HLfit}
\title{
Hosmer-Lemeshow goodness of fit
}
\description{
This function calculates a model's calibration performance (reliability) with the Hosmer & Lemeshow goodness-of-fit statistic, which compares predicted probability to observed occurrence frequency at each portion of the probability range.
}
\usage{
HLfit(model = NULL, obs = NULL, pred = NULL, bin.method, 
n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, 
min.prob.interval = 0.1, quantile.type = 7, simplif = FALSE, 
verbosity = 2, alpha = 0.05, plot = TRUE, plot.values = TRUE, 
plot.bin.size = TRUE, xlab = "Predicted probability", 
ylab = "Observed prevalence", na.rm = TRUE, rm.dup = FALSE, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{bin.method}{argument to pass to \code{\link{getBins}} specifying the method for grouping the records into bins within which to compare predicted probability to observed prevalence; type modEvAmethods("getBins") for available options, and see Details for more information.}
  \item{n.bins}{argument to pass to \code{\link{getBins}} specifying the number of bins to use if bin.method = n.bins or bin.method = quantiles. The default is 10.}
  \item{fixed.bin.size}{argument to pass to \code{\link{getBins}}, a logical value indicating whether to force bins to have (approximately) the same size. The default is FALSE.}
  \item{min.bin.size}{argument to pass to \code{\link{getBins}} specifying the minimum number of records in each bin. The default is 15, the minimum required for accurate comparisons within bins (Jovani & Tella 2006, Jimenez-Valverde et al. 2013).}
  \item{min.prob.interval}{argument to pass to \code{\link{getBins}} specifying the minimum interval (range) of probability values within each bin. The default is 0.1.}
  \item{quantile.type}{argument to pass to \code{\link{quantile}} specifying the algorithm to use if bin.method = "quantiles". The default is 7 (the \code{\link{quantile}} default in R), but check out other types, e.g. 3 (used by SAS), 6 (used by Minitab and SPSS) or 5 (appropriate for deciles, which correspond to the default n.bins = 10).}
  \item{simplif}{logical, wheter to perform a faster simplified version returning only the basic statistics. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages or warnings to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{alpha}{alpha value for confidence intervals if \code{plot = TRUE}.}
  \item{plot}{logical, whether to produce a plot of the results. The default is TRUE.}
  \item{plot.values}{logical, whether to report measure values in the plot. The default is TRUE.}
  \item{plot.bin.size}{logical, whether to report bin sizes in the plot. The default is TRUE.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{\dots}{further arguments to pass to the \code{\link{plot}} function.}
}
\details{
Most of the commonly used measures for evaluating model performance focus on the discrimination or the classification capacity, i.e., how well the model is capable of distinguishing or classifying presences and absences (often after the model's continuous predictions of presence probability or alike are converted to binary predictions of presence or absence). However, there is another important facet of model evaluation: calibration or reliability, i.e., the relationship between predicted probability and observed occurrence frequency (Pearce & Ferrier 2000; Jimenez-Valverde et al. 2013). The \code{HLfit} function measures model reliability with the Hosmer & Lemeshow goodness-of-fit statistic (Hosmer & Lemeshow 1980).

Note that this statistic has strong limitations and caveats (see e.g. http://www.statisticalhorizons.com/hosmer-lemeshow, Allison 2014), mainly due to the need to group the values into bins within which to compare probability and prevalence, and the strong influence of the binning method on the results. The 'HLfit' function can use several binning methods, which are implemented and roughly explained in the \code{\link{getBins}} function and can be accessed by typing 'modEvAmethods("getBins")'. You should try 'HLfit' with different binning methods to see how if the results are robust.
}
\value{
\code{HLfit} returns a list with the following components:
\item{bins.table}{a data frame of the obtained bins and the values resulting from the hosmer-Lemeshow goodness-of-fit analysis.}
\item{chi.sq}{the value of the Chi-squared test.}
\item{DF}{the number of degrees of freedom.}
\item{p.value}{the p-value of the Hosmer-Lemeshow test. Note that this is one of those tests for which higher p-values are better.}
\item{RMSE}{the root mean squared error.}
}
\references{
Allison P.D. (2014) Measures of Fit for Logistic Regression. SAS Global Forum, Paper 1485

Hosmer D.W. & Lemeshow S. (1980) A goodness-of-fit test for the multiple logistic regression model. Communications in Statistics, A10: 1043-1069

Jimenez-Valverde A., Acevedo P., Barbosa A.M., Lobo J.M. & Real R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516

Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218

Pearce J. & Ferrier S. (2000) Evaluating the Predictive Performance of Habitat Models Developed using Logistic Regression. Ecological Modeling, 133: 225-245
}
\author{
A. Marcia Barbosa
}
\note{
The 4 lines of code from "observed" to "p.value" were adapted from the 'hosmerlem' function available at http://www.stat.sc.edu/~hitchcock/diseaseoutbreakRexample704.txt. The plotting code was loosely based on the \code{calibration.plot} function in package \pkg{PresenceAbsence}. \code{HLfit} still needs some code simplification, and may fail for some datasets and binning methods. Fixes are being applied. Feedback is welcome.
}
\seealso{
\code{\link{getBins}}, \code{\link{MillerCalib}}
}
\examples{
# load sample models:

data(rotif.mods)


# choose a particular model to play with:

mod <- rotif.mods$models[[1]]


# try HLfit using different binning methods:

HLfit(model = mod, bin.method = "round.prob", 
main = "HL GOF with round.prob (n=10)")

HLfit(model = mod, bin.method = "prob.bins", 
main = "HL GOF with prob.bins (n=10)")

HLfit(model = mod, bin.method = "size.bins", 
main = "HL GOF with size.bins (min size=15)")

HLfit(model = mod, bin.method = "size.bins", min.bin.size = 30, 
main = "HL GOF with size.bins min size 30")

HLfit(model = mod, bin.method = "n.bins", 
main = "HL GOF with 10 bins")

HLfit(model = mod, bin.method = "n.bins", fixed.bin.size = TRUE, 
main = "HL GOF with 10 bins of fixed size")

HLfit(model = mod, bin.method = "n.bins", n.bins = 20, 
main = "HL GOF with 20 bins")

HLfit(model = mod, bin.method = "quantiles", 
main = "HL GOF with quantile bins (n=10)")

HLfit(model = mod, bin.method = "quantiles", n.bins = 20,
main = "HL GOF with quantile bins (n=20)")


# you can also use 'predPlot' with vectors of observed and predicted values
# instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

HLfit(obs = presabs, pred = prediction, bin.method = "round.prob")


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
