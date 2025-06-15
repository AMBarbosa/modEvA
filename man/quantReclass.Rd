\name{quantReclass}
\alias{quantReclass}
\title{Reclassify continuous values based on quantiles}
\description{
This function takes the continuous predictions of a model of suitability (e.g. the continuous Bioclim envelope model, computed by the \code{bioclim} function of the \pkg{dismo} package or the \code{envelope} function of the \pkg{predicts} package), and reclassifies them according to their quantiles.
}
\usage{
quantReclass(pred, by = 0.01, na.rm = TRUE)
}
\arguments{
  \item{pred}{a 'numeric' vector or a 'SpatRaster' map of predicted suitability values.}
  \item{by}{numeric value indicating which \code{\link{quantile}}s to compute, e.g. 0.01 for percentiles (the default), 0.1 for deciles, etc.}
  \item{na.rm}{logical value indicating whether NA values should be ignored when computing the quantiles. Defaults to TRUE.}
}
\details{
This function was created by Formoso-Freire et al. (2023) to reclassify continuous Bioclim predictions into ranked suitability values, rescaling them into relative suitability. Modern implementations of Bioclim compute a percentile distribution of the values of each environmental variable at species presence localities. Then, the closer to the 50th percentile (the median), the more suitable a location is according to that variable (Hijmans et al. 2020; Hijmans 2023). However, the more variables are included in the model, the less suitable any location becomes, because it is less likely to be close to the median for all variables. The proposed rescaling procedure removes the dependence of Bioclim predictions on the number of variables included, and it has shown to provide more realistic predictions (Formoso-Freire et al., 2023).
}
\value{
This function returns an object of the same class as 'pred' with the reclassified values.
}
\author{
A. Marcia Barbosa, Victoria Formoso-Freire, Andres Baselga, Carola Gomez-Rodriguez
}
\seealso{
\code{\link{getThreshold}}, \code{bioclim} in package \pkg{dismo}, \code{envelope} in package \pkg{predicts}
}
\references{
Formoso-Freire V., Barbosa A.M., Baselga A., Gomez-Rodriguez C. (2023) Predicting the spatio-temporal pattern of range expansion under lack of equilibrium with climate. Biological Conservation, 288: 110361

Hijmans R.J., Phillips S., Leathwick J. & Elith J. (2020). dismo: Species distribution modelling (1.3.5). https://CRAN.R-project.org/package=dismo

Hijmans R.J. (2023). predicts: Spatial Prediction Tools. R package version 0.1-11. https://CRAN.R-project.org/package=predicts
}
\examples{
# simulate some sample data:
set.seed(2023)
bioclim_pred <- runif(n = 10, min = 0, max = 1)
bioclim_pred

quantReclass(pred = bioclim_pred, by = 0.1)
}
