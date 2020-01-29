\name{RsqGLM}
\alias{RsqGLM}
\title{R-squared measures for GLMs}
\description{
This function calculates some (pseudo) R-squared statistics for binomial Generalized Linear Models.
}
\usage{
RsqGLM(model = NULL, obs = NULL, pred = NULL, 
use = "pairwise.complete.obs")
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another
  binary response variable. Not necessary (and ignored) if \code{model} is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability. Must be of the same length and in the same order as \code{obs}. Not necessary (and ignored) if \code{model} is provided.}
  \item{use}{argument to be passed to \code{\link{cor}} for handling mising values.}
}
\details{
Implemented measures include the R-squareds of McFadden (1974), Cox-Snell (1989), Nagelkerke (1991, which corresponds to the corrected Cox-Snell, eliminating its upper bound), and Tjur (2009). See Allison (2014) for a brief review of these measures.
}
\value{The function returns a named list of the calculated R-squared values.}
\references{
Allison P. (2014) Measures of fit for logistic regression. SAS Global Forum, Paper 1485-2014

Cox, D.R. & Snell E.J. (1989) The Analysis of Binary Data, 2nd ed. Chapman and Hall, London

McFadden, D. (1974) Conditional logit analysis of qualitative choice behavior. In: Zarembka P. (ed.) Frontiers in Economics. Academic Press, New York

Nagelkerke, N.J.D. (1991) A note on a general definition of the coefficient of determination. Biometrika, 78: 691-692

Tjur T. (2009) Coefficients of determination in logistic regression models - a new proposal: the coefficient of discrimination. The American Statistician, 63: 366-372.
}
\author{A. Marcia Barbosa}
\note{
Tjur's R-squared can only be calculated for models with binomial response variable; otherwise, NA will be returned.
}
\seealso{\code{\link{Dsquared}}, \code{\link{AUC}}, \code{\link{threshMeasures}}, \code{\link{HLfit}}}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

RsqGLM(model = mod)


# you can also use RsqGLM with vectors of observed and predicted values
# instead of a model object:

RsqGLM(obs = mod$y, pred = mod$fitted.values)
}
