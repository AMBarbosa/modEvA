\name{errorMeasures}
\alias{errorMeasures}
\title{
Measures of model prediction error.
}
\description{
This function computes metrics such as the (root) mean square error or the Brier score, of a model object or a set of observed and predicted values or maps. The smaller the returned values, the better the modol predictions fit the observations.
}
\usage{
errorMeasures(model = NULL, obs = NULL, pred = NULL, na.rm = TRUE, 
rm.dup = FALSE, verbosity = 2)
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
The Brier score measures the accuracy of probabilistic predictions against a binary response variable. It is computed as the mean of the squared differences between observed and predicted values, i.e. the mean square error (MSE). It may take values between 0 and 1. The smaller the score, the better the model fit.

The root mean square error is more usually applied to models of coninuous response variables. It is computed as the square root of the mean of the squared differences between observed and predicted values. It is (approximately) the same as the standard deviation of the model residuals (prediction errors), i.e., a measure of how spread out these residuals are, or how concentrated the observations are around the model prediction line.
}
\value{
This function returns a named list of numeric values indicating various measures of model error.
}
\references{
Kenney J.F. & Keeping E.S. (1962) Root Mean Square. "Mathematics of Statistics", 3rd ed. Princeton, NJ: Van Nostrand, pp. 59-60.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{RMSE}}, \code{\link{plotGLM}}, \code{\link{RsqGLM}}, \code{Dsquared}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

errorMeasures(model = mod)


# you can also use errorMeasures with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values

errorMeasures(obs = presabs, pred = prediction)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
