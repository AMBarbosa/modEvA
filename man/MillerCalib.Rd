\name{MillerCalib}
\alias{MillerCalib}
\title{Miller's calibration satistics for logistic regression models}
\description{
This function calculates Miller's (1991) calibration statistics for a generalized linear model with binomial distribution and logistic link, namely the intercept and slope of the regression of the response variable on the logit of predicted probabilities. Optionally and by default, it also plots the corresponding regression line over the reference diagonal.
}
\usage{
MillerCalib(model = NULL, obs = NULL, pred = NULL, plot = TRUE,
line.col = "black", diag = TRUE, diag.col = "grey", 
plot.values = TRUE, digits = 2, xlab = "", ylab = "", 
main = "Miller calibration", ...)
}
\arguments{
  \item{model}{a model object of class "glm" and family "binomial".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another binary response variable. Not necessary (and ignored) if 'model' is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability. Must be of the same length and in the same order as 'obs'. A warning is emitted if it includes values outside the [0, 1] interval. Not necessary (and ignored) if 'model' is provided.}
  \item{plot}{logical, whether or not to produce a plot of the Miller regression line. Defaults to TRUE.}
  \item{line.col}{colour for the Miller regression line (if plot = TRUE).}
  \item{diag}{logical, whether or not to add the reference diagonal (if plot = TRUE). Defaults to TRUE.}
  \item{diag.col}{line colour for the reference diagonal.}
  \item{plot.values}{logical, whether or not to report the values of the intercept and slope on the plot. Defaults to TRUE.}
  \item{digits}{integer number indicating the number of digits to which the values in the plot should be rounded. Dafaults to 2. This argument is ignored if 'plot' or 'plot.values' are set to FALSE.}
  \item{xlab}{label for the x axis.}
  \item{ylab}{label for the y axis.}
  \item{main}{title for the plot.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\details{
Calibration or reliability measures how a model's predicted probabilities relate to observed species prevalence or proportion of presences in the modelled data (Pearce & Ferrier 2000; Wintle et al. 2005; Franklin 2010). If predictions are perfectly calibrated, the slope will equal 1 and the intercept will equal 0, so the model's calibation line will perfectly overlap with the reference diagonal. Note that Miller's statistics assess the model globally: a model is well calibrated if the average of all predicted probabilities equals the proportion of presences in the modelled data. Good calibration is always attained on the same data used for building the model (Miller 1991); Miller's calibration statistics are mainly useful when extrapolating a model outside those training data.
}
\value{
This function returns a list of three integer values:
\item{intercept}{the calibration intercept.}
\item{slope}{the calibration slope.}
If \code{plot = TRUE}, a plot will be produced with the model calibration line, optionally (if diag = TRUE) over the reference diagonal, and optionally (if plot.values = TRUE) with the intercept and slope values printed on it.
}
\references{
Franklin, J. (2010) Mapping Species Distributions: Spatial Inference and Prediction. Cambridge University Press, Cambridge.

Miller M.E., Hui S.L. & Tierney W.M. (1991) Validation techniques for logistic regression models. Statistics in Medicine, 10: 1213-1226

Pearce J. & Ferrier S. (2000) Evaluating the predictive performance of habitat models developed using logistic regression. Ecological Modelling, 133: 225-245

Wintle B.A., Elith J. & Potts J.M. (2005) Fauna habitat modelling and mapping: A review and case study in the Lower Hunter Central Coast region of NSW. Austral Ecology, 30: 719-738
}
\author{A. Marcia Barbosa}
\seealso{
\code{\link{HLfit}}, \code{\link{Dsquared}}, \code{\link{RsqGLM}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

MillerCalib(model = mod)
MillerCalib(model = mod, plot.values = FALSE)
MillerCalib(model = mod, main = "Model calibration line")


# you can also use MillerCalib with vectors of observed and predicted values
# instead of a model object:

MillerCalib(obs = mod$y, pred = mod$fitted.values)
}
