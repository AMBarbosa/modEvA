\name{prevalence}
\alias{prevalence}
\title{
Prevalence
}
\description{
For building and evaluating species distribution models, the porportion of presences of the species may be an issue to take into account (e.g. Jimenez-Valverde & Lobo 2006, Barbosa et al. 2013). The \code{prevalence} function calculates this measure.
}
\usage{
prevalence(obs, model = NULL, event = 1, na.rm = TRUE)
}
\arguments{
  \item{obs}{a vector or a factor of binary observations (e.g. 1 vs. 0, male vs. female, disease vs. no disease, etc.). This argument is ignored if 'model' is provided.}
  \item{model}{alternatively to 'obs', a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' will be extracted with \code{\link{mod2obspred}}.}
  \item{event}{the value whose prevalence we want to calculate (e.g. 1, "present", etc.). This argument is ignored if 'model' is provided.}
  \item{na.rm}{logical, whether NA values should be excluded from the calculation. The default is TRUE.}
}
\value{
Numeric value of the prevalence of \code{event} in the \code{obs} vector.
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions, in press

Jimenez-Valverde A. & Lobo J.M. (2006) The ghost of unbalanced species distribution data in geographical model predictions. Diversity and Distributions, 12: 521-524.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{evenness}}
}
\examples{

# calculate prevalence from binary vectors:

(x <- rep(c(0, 1), each = 5))

(y <- c(rep(0, 3), rep(1, 7)))

(z <- c(rep(0, 7), rep(1, 3)))

prevalence(x)

prevalence(y)

prevalence(z)


(w <- c(rep("yes", 3), rep("nope", 7)))

prevalence(w, event = "yes")


# calculate prevalence from a model object:

data(rotif.mods)

prevalence(mod = rotif.mods$models[[1]])
}
