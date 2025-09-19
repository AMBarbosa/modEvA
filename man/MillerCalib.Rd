\name{MillerCalib}
\alias{MillerCalib}
\title{Miller's calibration satistics for logistic regression models}
\description{
This function calculates Miller's (1991) calibration statistics for a presence probability model -- namely, the intercept and slope of a logistic regression of the response variable on the logit of predicted probabilities. Optionally and by default, it also plots the corresponding regression line over the reference diagonal (identity line). If the model is well calibrated, the line should lie along (or at least be nearly parallel to) the reference diagonal, i.e. the slope should ideally equal 1.
}
\usage{
MillerCalib(model = NULL, obs = NULL, pred = NULL, plot = TRUE,
line.col = "darkblue", diag = TRUE, diag.col = "lightblue3", 
plot.values = TRUE, digits = 2, xlab = "", ylab = "", 
main = "Miller calibration", na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{plot}{logical (default TRUE), whether or not to produce a plot of the Miller regression line.}
  \item{line.col}{colour for the Miller regression line (if plot = TRUE).}
  \item{diag}{logical (default TRUE), whether or not to add the reference diagonal (if plot = TRUE).}
  \item{diag.col}{line colour for the reference diagonal.}
  \item{plot.values}{logical (default TRUE), whether or not to show in the plot the values of the intercept, slope, and absolute difference between the slope and the ideal value of 1.}
  \item{digits}{integer number indicating the number of digits to which the values in the plot should be rounded. Dafaults to 2. This argument is ignored if 'plot' or 'plot.values' are set to FALSE.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.}
  \item{main}{title for the plot.}
  \item{na.rm}{Logical value (default TRUE) indicating whether missing values should be ignored in computations.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\details{
Calibration or reliability measures how a model's predicted probabilities relate to observed species prevalence or proportion of presences in the modelled data (Pearce & Ferrier 2000; Wintle et al. 2005; Franklin 2010). If predictions are perfectly calibrated, the slope will equal 1 and the intercept will equal 0, so the model's calibation line will perfectly overlap with the reference diagonal or identity line.

Note that Miller's statistics assess the model globally: a model is well calibrated if the average of all predicted probabilities equals the proportion of presences in the modelled data. For logistic regression models, perfect calibration is always attained on the same data used for building the model (Miller 1991); Miller's calibration statistics are mainly useful when projecting a model outside those training data.

Calibration can be separated into two measurable components, bias and spread, and a third component, unexplained error. Bias describes a consistent overestimate or underestimate of presence probability, which is reflected by a Miller intercept above or below 0 (i.e., a model line above or below the reference diagonal). Spread describes a departure of the model line from the 45-degree slope. A slope greater than 1 indicates that predicted values above 0.5 are underestimating, and predicted values below 0.5 are overestimating, the probability of presence. A slope smaller than 1 (while greater than 0) implies that predicted values below 0.5 are underestimating, and values above 0.5 are overestimating, the probability of presence (Pearce & Ferrier 2000). A Miller slope very different from 1 indicates a poorly calibrated model. Baquero et al. (2021) proposed that a value between 0.5 and 1.5 (i.e., absolute slope difference up to 0.5) can be considered not very different from 1. The unexplained error component can be assessed, though only in part, through residual analysis (Miller 1991; Pearce & Ferrier 2000).

While Miller's calibration statistics were originally conceived for generalized linear models with binomial distribution and logit link (Miller 1991), they may also apply to other models that also estimate presence probability, including those that use different link functions such as probit or cloglog. Regardless of how they get there, these models attempt to estimate presence probabilities as well calibrated as possible, and the logit is the canonical link for the Bernoulli distribution which is appropriate for a binary response variable (Jane Elith, pers. comm.). Indeed, the Miller slope is visibly worse when those other link functions are used for computing it.

Miller's calibration slope (though not the intercept) is also adequate to assess the calibration of other predictions related to presence probability, such as suitability and favourability (see e.g. 'Fav' function in the \pkg{fuzzySim} package). Indeed, the slope is the same for presence probability and its corresponding favourability value (see Examples, bottom).
}
\value{
This function returns a list of three numeric values:
\item{intercept}{the calibration intercept.}
\item{slope}{the calibration slope.}
\item{slopeDiff}{the absolute difference between the obtained slope and the optimal value of 1.}
If \code{plot = TRUE}, a plot will be produced with the model calibration line, optionally (if diag = TRUE) over the reference diagonal, and also optionally (if plot.values = TRUE) with the output values printed on it.
}
\references{
Baquero R.A., Barbosa A.M., Ayllon D., Guerra C., Sanchez E., Araujo M.B. & Nicola G.G. (2021) Potential distributions of invasive vertebrates in the Iberian Peninsula under projected changes in climate extreme events. Diversity and Distributions, 27(11): 2262-2276

Franklin, J. (2010) Mapping Species Distributions: Spatial Inference and Prediction. Cambridge University Press, Cambridge

Miller M.E., Hui S.L. & Tierney W.M. (1991) Validation techniques for logistic regression models. Statistics in Medicine, 10: 1213-1226

Pearce J. & Ferrier S. (2000) Evaluating the predictive performance of habitat models developed using logistic regression. Ecological Modelling, 133: 225-245

Wintle B.A., Elith J. & Potts J.M. (2005) Fauna habitat modelling and mapping: A review and case study in the Lower Hunter Central Coast region of NSW. Austral Ecology, 30: 719-738
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{HLfit}}, \code{\link{Dsquared}}, \code{\link{RsqGLM}}, \code{\link{Boyce}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

MillerCalib(model = mod)
MillerCalib(model = mod, plot.values = FALSE, main = "Model calibration line")


# you can also use MillerCalib with vectors of observed and predicted values
# instead of a model object:

MillerCalib(obs = mod$y, pred = mod$fitted.values)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values


# Miller slope can also apply to predictions other than probability:

\dontrun{
# (the following code requires the 'fuzzySim' pkg installed)

MillerCalib(obs = mod$y, pred = mod$fitted.values)  # probability
fav <- fuzzySim::Fav(model = mod)  # favourability
MillerCalib(obs = mod$y, pred = fav)  # same slope, different intercept
}
}
