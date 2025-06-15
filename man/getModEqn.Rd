\name{getModEqn}
\alias{getModEqn}
\title{
Get model equation
}
\description{
This function retrieves the equation of a model, to print or apply elsewhere.
}
\usage{
getModEqn(model, type = "Y", digits = NULL, prefix = NULL, 
suffix = NULL)
}
\arguments{
  \item{model}{
a model object of class 'lm' or glm'.
}
  \item{type}{
the type of equation to get; can be either "Y" (the default, for the linear model equation), "P" (for probabiity) or "F" (for favourability).
}
  \item{digits}{
the number of significant digits to which to round the coefficient estimates in the equation.
}
  \item{prefix}{
the prefix to add to each variable name in the equation.
}
  \item{suffix}{
the suffix to add to each variable name in the equation.
}
}
\details{
The summary of a model in R gives you a table of the coefficient estimates and other parameters. Sometimes it may be useful to have a string of text with the model's equation, so that you can present it in an article (e.g. Real et al. 2005) or apply it in a (raster map) calculation, either in R (although here you can usually use the 'predict' function for this) or in a GIS software (e.g. Barbosa et al. 2010). The \code{getModEqn} function gets this equation for linear or generalized linear models.

By default it prints the \code{"Y"} linear equation, but for generalized linear models you can also set \code{type = "P"} (for the equation of probability) or \code{type = "F"} (for favourability, which modifies the intercept to eliminate the effect of modelled prevalence - see Real et al. 2006).

If the variables to which you want to apply the model have a prefix or suffix (e.g. something like prefix = "raster.stack$" for the R 'raster' or 'terra' package, or prefix = "mydata$" for a data frame, or suffix = "@1" in QGIS, or suffix = "@mapset" in GRASS), you can get these in the equation too, using the \code{prefix} and/or the \code{suffix} argument.
}
\value{
A charachter string of the model equation.
}
\references{
Barbosa A.M., Real R. & Vargas J.M. (2010) Use of coarse-resolution models of species' distributions to guide local conservation inferences. Conservation Biology 24: 1378-87

Real R., Barbosa A.M., Martinez-Solano I. & Garcia-Paris, M. (2005) Distinguishing the distributions of two cryptic frogs (Anura: Discoglossidae) using molecular data and environmental modeling. Canadian Journal of Zoology 83: 536-545

Real R., Barbosa A.M. & Vargas J.M. (2006) Obtaining environmental favourability functions from logistic regression. Environmental and Ecological Statistics 13: 237-245
}
\author{
A. Marcia Barbosa
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

getModEqn(mod)

getModEqn(mod, type = "P", digits = 3, suffix = "@mapset")

getModEqn(mod, type = "F", digits = 2)
}
