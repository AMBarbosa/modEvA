\name{mod2obspred}
\alias{mod2obspred}
\title{
Extract observed and predicted, or predictor, values from a model object.
}
\description{
This function takes a model object and returns the observed and (optionally) the predicted (fitted) values from that model; or the values of the predictors in the model.
}
\usage{
mod2obspred(model, obs.only = FALSE, x.only = FALSE)
}
\arguments{
  \item{model}{a model object of class "glm", "gam", "gbm", "GBMFit", "randomForest" or "bart" from which the response variable and fitted (predicted) values can be extracted. Note that, for "randomForest" models, only the out-of-bag prediction is available from the model object (see \code{?randomForest::predict.randomForest}), so here you'll get different results if you provide 'model' or the modelled 'obs' (and corresponding 'pred') values.}
  \item{obs.only}{logical value (default FALSE) indicating whether only 'obs' should be obtained. Saves computing time when 'pred' not needed. Used e.g. by \code{\link{prevalence}}).}
  \item{x.only}{logical value (default FALSE) indicating whether only 'x' or 'data' (the predictor values) should be obtained. Used e.g. by \code{\link{varImp}} if imp.type = "permutation".}
}
\value{
A data frame with one column containing the observed and (if obs.only=FALSE, the default) another column containing the predicted values from 'model'; or a data frame with the 'x', 'data' or predictor values from 'model' (if x.only=TRUE).
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{prevalence}}
}
\examples{
data(rotif.mods)
mod <- rotif.mods$models[[1]]

obspred <- mod2obspred(mod)
head(obspred)

obs <- mod2obspred(mod, obs.only = TRUE)
head(obs)

data <- mod2obspred(mod, x.only = TRUE)
head(data)
}
