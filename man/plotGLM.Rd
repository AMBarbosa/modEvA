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
ylab = "Predicted probability", main = "Model plot", ...)
}
\arguments{
  \item{model}{a model object of class "\code{\link{glm}}".}
  \item{obs}{a vector of presence/absence or other binary (1-0) observed data. Not necessary (and ignored) if 'model' is provided.}
  \item{pred}{a vector of the values predicted by a GLM of the binary observed data. Not necessary (and ignored) if 'model' is provided.}
  \item{link}{the link function of the GLM; only 'logit' (the default) is implemented.}
  \item{plot.values}{logical, whether to include in the plot diagnostic values such as explained deviance (calculated with the \code{\link{Dsquared}} function) and pseudo-R-squared measures (calculated with the \code{\link{RsqGLM}} function). Defaults to TRUE.}
    \item{plot.digits}{integer number indicating the number of digits to which the values in the plot should be \code{\link{round}}ed (if 'plot.values = TRUE'). Defaults to 3.}
  \item{xlab}{character string specifying the label for the x axis.}
  \item{ylab}{character string specifying the label for the y axis.}
  \item{main}{character string specifying the title for the plot.}
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
\code{\link{glm}}, \code{\link{Dsquared}}
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
}
