\name{inputMunch}
\alias{inputMunch}
\title{
Munch inputs into 'obs' and 'pred' vectors
}
\description{
This function is used internally by many other functions in this package to check and extract the 'obs' and 'pred' vectors from the user inputs.
}
\usage{
inputMunch(model = NULL, obs = NULL, pred = NULL, rm.dup = FALSE, na.rm = FALSE,
pbg = FALSE, verbosity = 2)
}
\arguments{
  \item{model}{a binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart". If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments (e.g. for external test data) instead of 'model'.}
  \item{obs}{alternatively to 'model' and together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{pred}{alternatively to 'model' and together with 'obs', a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. Must be of the same length and in the same order as 'obs'. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values for the entire evaluation region, in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided.}
  \item{rm.dup}{logical argument to be passed to \code{\link{ptsrast2obspred}} indicating whether repeated points within the same pixel should be removed. The default is FALSE.}
  \item{na.rm}{logical argument indicating whether to remove (with a warning saying how many) rows with NA in any of the resulting 'obs' or 'pred' values. The default is FALSE.}
  \item{pbg}{logical value indicating whether to output presence/background (rather than presence/absence) data. If set to TRUE, the 'pred' values at presences ('obs' = 1) are added to the background ('obs' = 0). Similar to what is done in presence/background modelling algorithms such as Maxent (e.g. analogous to the 'addsamplestobackground' argument of maxnet::maxnet). Experimental! Default FALSE.}
  \item{verbosity}{integer value indicating the amount of messages to display. Defaults to 2, for the maximum amount of messages.}
}
\value{
This function returns a two-column data frame containing the 'obs' and 'pred' values, or an error message if inputs are not as required.
}
\author{
A. Marcia Barbosa
}
