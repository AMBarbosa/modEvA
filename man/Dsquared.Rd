\name{Dsquared}
\alias{Dsquared}
\title{
Explained deviance
}
\description{
This function computes the (adjusted) amount of deviance accounted for by a model, given a model object or a set of observed and predicted values.
}
\usage{
Dsquared(model = NULL, obs = NULL, pred = NULL, family = NULL,
adjust = FALSE, npar = NULL, na.rm = TRUE, rm.dup = FALSE, pbg = FALSE,
dismo.version = FALSE, verbosity = 2)
}
\arguments{
  \item{model}{a model object of a class implemented in \code{\link{mod2obspred}}. If this argument is provided, 'obs' and 'pred' will be extracted with that function. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed values of the response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of presence points, in which case the 'obs' vector of presences and absences will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values, of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{family}{a character vector (i.e. in quotes) of length 1 specifying the family of the model that generated the 'pred' values. This argument is ignored if \code{model} is provided and is of a class for which \code{\link{family}} provides a result; otherwise (i.e. if 'obs' and 'pred' are provided rather than a model object), family can be specified by the user, or (if left NULL) will be guessed (with a message) given the values of the response variable.}
  \item{adjust}{logical, whether or not to adjust the D-squared value for the number of observations and parameters in the model (see Details). The default is \code{FALSE}; \code{TRUE} requires either providing the \code{model} object of class GLM, or specifying the number of parameters in the model that produced the \code{pred} values.}
  \item{npar}{integer value indicating the number of parameters in the model. This argument is ignored and taken from \code{model} if this argument is provided and of class GLM, or if \code{adjust = FALSE}.}
    \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{pbg}{logical value to pass to \code{\link{inputMunch}} indicating whether to use presence/background (rather than presence/absence) data. Default FALSE.}
  \item{dismo.version}{Logical value indicating whether the deviance should be computed with code from the dismo::calc.deviance() function. The default is FALSE, for back-compatibility.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\details{
Linear models have an R-squared value (commonly provided with the model summary) which measures the proportion of variation that the model accounts for. For generalized linear models (GLMs) and others based on non-continuous response variables, an equivalent is the amount of deviance accounted for (D-squared; Guisan & Zimmermann 2000), though this value is not routinely provided with the model summary. The \code{Dsquared} function calculates it as the proportion of the null deviance (i.e. the deviance of a model with no predictor variables) that is accounted for by the model. There is also an option to compute the adjusted D-squared, which takes into account the number of observations and the number of parameters, thus allowing direct comparison among the output for different models (Weisberg 1980, Guisan & Zimmermann 2000).

The function computes the mean residual deviance (as in the \code{calc.deviance} function of package \pkg{dismo}) of the observed (response) against the predicted values, and the mean deviance of a null model (with no predictor variables), i.e. of the response against the mean of the response. Finally, it gets the explained deviance as (null-residual)/null.
}
\value{
The function returns a numeric value indicating the (optionally adjusted) proportion of deviance accounted for by the input model predictions.
}
\references{
Guisan, A. & Zimmermann, N.E. (2000) Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186

Weisberg, S. (1980) Applied Linear Regression. Wiley, New York
}
\author{
A. Marcia Barbosa, with parts of code from 'dismo::calc.deviance' by John R. Leathwick and Jane Elith
}
\seealso{
\code{\link{plotGLM}}, \code{\link{RsqGLM}}, \code{dismo::calc.deviance}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

Dsquared(model = mod)

Dsquared(model = mod, adjust = TRUE)


# you can also use Dsquared with vectors of observed and predicted values
# instead of with a model object:

presabs <- mod$y
prediction <- mod$fitted.values
parameters <- attributes(logLik(mod))$df

Dsquared(obs = presabs, pred = prediction, family = "binomial")

Dsquared(obs = presabs, pred = prediction, family = "binomial",
adjust = TRUE, npar = parameters)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
