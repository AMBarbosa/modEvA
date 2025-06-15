\name{logLike}
\alias{logLike}
\title{
Log-likelihood
}
\description{
This function computes the log-likelihood of a model, given a model object or a set of observed and predicted values (or presence points and raster predictions).
}
\usage{
logLike(model = NULL, obs = NULL, pred = NULL, na.rm = TRUE, plot = TRUE, verbosity = 2)
}
\arguments{
  \item{model}{a model object of class implemented in \code{\link{mod2obspred}}. If this argument is provided, 'obs' and 'pred' will be extracted with that function. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed values of the response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of presence points, in which case the 'obs' vector of presences and absences will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values, of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
    \item{na.rm}{logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
    \item{plot}{logical value indicating whether to plot the sorted values in log() (see Details). The default is TRUE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\details{
The log-likelihood can be a measure of model calibration (Lawson et al. 2014, Fithian et al. 2015, Fletcher & Fortin 2018). The higher the value, the better the predictions fit the observations.

The log-likelihood is computed as sum(log(pred*obs+(1-pred)*(1-obs))) (Fletcher & Fortin 2018, p. 234). If plot=TRUE, a plot is shown with the values within sum(), both within and outside log().

In the first instance of 'pred' in the formula above, a very small constant is added to the 'pred' values of zero, because the logarithm of zero is undefined. This added constant is smaller (i.e. closer to the original value of zero) here than in the R code accompanying Fletcher & Fortin (2018), specifically 2e-16 rather than 0.001, which may justify some differences in the results when there are predictions of exactly zero. In any case, whatever constant is used, it should be the same across the models being compared.
}
\value{
This function returns a numeric value indicating the log-likelihood of the input model predictions given the input observations.
}
\references{
Fithian, W., Elith, J., Hastie, T., Keith, D.A., 2015. Bias correction in species distribution models: pooling survey and collection data for multiple species. Methods in Ecology and Evolution 6:424-438. https://doi.org/10.1111/2041-210X.12242

Fletcher R. & Fortin M.-J. (2018) Spatial Ecology and Conservation Modeling. Applications with R. Springer Nature Switzerland. Cham: 532 pp. https://www.fletcherlab.com/spatial-ecology-conservation-modeli

Lawson, C.R., Hodgson, J.A., Wilson, R.J., Richards, S.A., 2013. Prevalence, thresholds and the performance of presence-absence models. Methods in Ecology and Evolution 5, 54-64. https://doi.org/10.1111/2041-210X.12123
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{logLik}}, \code{\link{MillerCalib}}, \code{\link{HLfit}},
\code{\link{Boyce}}, \code{\link{RMSE}}, \code{\link{Dsquared}},
\code{\link{RsqGLM}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

logLike(model = mod)


# you can also use logLike with vectors of observed and predicted values
# instead of with a model object:

obs <- mod$y
pred <- mod$fitted.values

logLike(obs = obs, pred = pred)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
