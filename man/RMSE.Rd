\name{RMSE}
\alias{RMSE}
\title{
Root mean square error
}
\description{
This function computes the root mean square error of a model object or a set of observed and predicted values or maps. THIS FUNCTION IS NOW DEPRECATED, AS THE RMSE HAS BEEN INCLUDED ALONG OTHER METRICS IN THE NEW \code{\link{errorMeasures}} FUNCTION.
}
\usage{
RMSE(model = NULL, obs = NULL, pred = NULL, na.rm = TRUE, rm.dup = FALSE, 
verbosity = 2)
}
\arguments{
  \item{model}{a model object of class implemented in \code{\link{mod2obspred}}. If this argument is provided, 'obs' and 'pred' will be extracted with that function. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed values of the response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of presence points, in which case the 'obs' vector of presences and absences will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values, of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
    \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\details{
The root mean square error is computed as the square root of the mean of the squared differences between observed and predicted values. It is (approximately) the same as the standard deviation of the model residuals (prediction errors), i.e., a measure of how spread out these residuals are, or how concentrated the observations are around the model prediction line. The smaller the RMSE, the better.
}
\value{
The function returns a numeric value indicating the root mean square error of the model predictions.
}
\references{
Kenney J.F. & Keeping E.S. (1962) Root Mean Square. "Mathematics of Statistics", 3rd ed. Princeton, NJ: Van Nostrand, pp. 59-60.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{errorMeasures}, \code{\link{plotGLM}}, \code{\link{RsqGLM}}, \code{Dsquared}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

RMSE(model = mod)


# you can also use RMSE with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

RMSE(obs = presabs, pred = prediction)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
