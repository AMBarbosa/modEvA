\name{predDensity}
\alias{predDensity}
\title{
Plot the density of predicted values for presences and absences.
}
\description{
This function produces a histogram and/or a kernel density plot of predicted values for a binomial GLM, by default separately for the observed presences and absences, given a model object or a vector of predicted values and (optionally) a vector of the corresponding observed values.
}
\usage{
predDensity(model = NULL, obs = NULL, pred = NULL, separate = TRUE, 
type = c("both"), legend.pos = "topright")
}
\arguments{
  \item{model}{a model object of class "glm" and family "binomial".}
  \item{obs}{numeric vector of the observed data, consisting of zeros and ones. This argument is ignored if 'model' is provided.}
  \item{pred}{numeric vector of the values predicted by a GLM of the observed data. This argument is ignored if 'model' is provided. Must be of the same length and in the same order as 'obs'.}
  \item{separate}{logical value indicating whether prediction densities should be computed separately for observed presences (ones) and absences (zeros). Defaults to TRUE, but is changed to FALSE if either 'model' or 'obs' not provided.}
  \item{type}{character vector specifying whether to produce a "histogram", a "density" plot, or "both" (the default). Partial argument matching is used.}
  \item{legend.pos}{character specifying the position for the legend, or "n" for no legend. Position can be "topright" (the default), "topleft, "bottomright"", "bottomleft", "top", "bottom", "left", "right", or "center". Partial argument matching is used.}
}
\details{
For more details, please refer to the documentation of the functions mentioned under "See Also".
}
\value{
This function outputs and plots the object(s) specified in 'type' -- by default, a \code{\link{density}} object and a \code{\link{hist}}ogram.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{hist}}, \code{\link{density}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

predDensity(model = mod)

predDensity(model = mod, type = "histogram")

predDensity(model = mod, type = "density")

# you can also use 'predDensity' with vectors of 
# observed and predicted values, instead of a model object:

presabs <- mod$y
prediction <- mod$fitted.values

predDensity(obs = presabs, pred = prediction)

predDensity(pred = prediction)
}
