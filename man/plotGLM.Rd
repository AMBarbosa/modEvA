\name{plotGLM}
\alias{plotGLM}
\title{
Plot a generalized linear model
}
\description{
This function plots the observed (presence/absence) data and the predicted (probability) values of a Generalized Linear Model against the y regression equation (logit) values. Only logistic regression (binomial response, logit link) is currently implemented.
}
\usage{
plotGLM(model = NULL, obs = NULL, pred = NULL, link = "logit", 
plot.values = TRUE, plot.digits = 3, xlab = "Logit (Y)", 
ylab = "Predicted probability", main = "Model plot", na.rm = TRUE, 
rm.dup = FALSE, verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "\code{\link{glm}}". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a numeric vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{link}{the link function of the GLM; only 'logit' (the default) is implemented.}
  \item{plot.values}{logical, whether to include in the plot diagnostic values such as explained deviance (calculated with the \code{\link{Dsquared}} function) and pseudo-R-squared measures (calculated with the \code{\link{RsqGLM}} function). Defaults to TRUE.}
   \item{plot.digits}{integer number indicating the number of digits to which the values in the plot should be \code{\link{round}}ed (if 'plot.values = TRUE'). Defaults to 3.}
  \item{xlab}{character string specifying the label for the x axis.}
  \item{ylab}{character string specifying the label for the y axis.}
  \item{main}{character string specifying the title for the plot.}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\value{
This function outputs a plot of model predictions against observations.
}
\references{
Guisan A. & Zimmermann N.E. (2000) Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186

Weisberg S. (1980) Applied Linear Regression. Wiley, New York
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{predPlot}}, \code{\link{predDensity}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

plotGLM(model = mod)

plotGLM(model = mod, plot.values = FALSE)


# you can also use 'plotGLM' with vectors of observed and
# predicted values instead of with a model object:

plotGLM(obs = mod$y, pred = mod$fitted.values)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
