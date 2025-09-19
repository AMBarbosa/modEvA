\name{pseudoRsq}
\alias{pseudoRsq}
\title{Pseudo-R-squared measures for binary-response models}
\description{
This function computes several pseudo R-squared statistics for a binomial Generalized Linear Model object, or for observations and predictions of any binary-response model.
}
\usage{
pseudoRsq(model = NULL, obs = NULL, pred = NULL, use = "pairwise.complete.obs",
plot = TRUE, plot.type = "lollipop", na.rm = TRUE, rm.dup = FALSE, 
verbosity = 2, ...)
}
\arguments{
  \item{model}{a binary-response model object of class "glm". Alternatively, you can input the 'obs' and 'pred' values of any other binary-response model.}
  \item{obs}{alternatively to 'model' and together with 'pred', a vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a numeric vector with the corresponding predicted values of presence probability. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{use}{argument to be passed to \code{\link{cor}} for handling mising values.}
  \item{plot}{logical value indicating whether or not to display a bar chart or (by default) a lollipop chart of the calculated measures.}
  \item{plot.type}{character value indicating the type of plot to produce (if plot=TRUE). Can be "\code{\link{lollipop}}" (the default) or "\code{\link{barplot}}".}
  \item{na.rm}{Logical value indicating whether missing values should be ignored in computations. Defaults to TRUE.}
  \item{rm.dup}{If \code{TRUE} and if 'pred' is a SpatRaster and if there are repeated points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{additional arguments to pass to the plotting function (see Examples).}
}
\details{
Implemented measures include the pseudo R-squared values of McFadden (1974), Cox & Snell (1989), Nagelkerke (1991, which corresponds to the corrected Cox-Snell, eliminating its upper bound), and Tjur (2009). See Allison (2014) for a brief review of these measures. Note that Tjur's R-squared can only be calculated for models with a binomial response variable; otherwise, NA will be returned.

Note also that pseudo R-squared values tend to be considerably lower than those of the R-squared for ordinary regression analysis, and they should not be judged by the same standards for a "good fit". For example, for McFadden's R-squared, values of 0.2 to 0.4 represent an excellent fit (McFadden, 1979).
}
\value{The function returns a named list of the calculated R-squared values.}
\note{This function was previously named RsqGLM.}
\references{
Allison P. (2014) Measures of fit for logistic regression. SAS Global Forum, Paper 1485-2014

Cox, D.R. & Snell E.J. (1989) The Analysis of Binary Data, 2nd ed. Chapman and Hall, London

McFadden, D. (1974) Conditional logit analysis of qualitative choice behavior. In: Zarembka P. (ed.) Frontiers in Economics. Academic Press, New York

McFadden, D. (1979) Quantitative Methods for Analyzing Travel Behaviour on Individuals: Some Recent Developments. Chapter 15 in Behavioural Travel Modelling. Edited by David Hensher and Peter Stopher.

Nagelkerke, N.J.D. (1991) A note on a general definition of the coefficient of determination. Biometrika, 78: 691-692

Tjur T. (2009) Coefficients of determination in logistic regression models - a new proposal: the coefficient of discrimination. The American Statistician, 63: 366-372.
}
\author{A. Marcia Barbosa}
\seealso{\code{\link{Dsquared}}, \code{\link{AUC}}, \code{\link{threshMeasures}}, \code{\link{HLfit}}, \code{\link{MillerCalib}}}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

pseudoRsq(model = mod)


# you can also use pseudoRsq with vectors of observed and predicted values
# instead of a model object:

pseudoRsq(obs = mod$y, pred = mod$fitted.values)


# plotting arguments can be modified:

par(mar = c(6, 3, 2, 1))

pseudoRsq(obs = mod$y, pred = mod$fitted.values, col = "seagreen", border = NA,
ylim = c(0, 1), main = "Pseudo-R-squared values")
}
