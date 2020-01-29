\name{standard01}
\alias{standard01}
\title{
Standardize to 0-1 (or vice-versa)
}
\description{
This function converts the score of a measure that ranges from -1 to 1 (e.g. a kappa or TSS value obtained for a model) into its (linearly) corresponding value in 0-to-1 scale, so that it can be compared directly with measures that range between 0 and 1 (such as CCR or AUC). It can also perform the conversion in the opposite direction.
}
\usage{
standard01(score, direction = c("-1+1to01", "01to-1+1"))
}
\arguments{
  \item{score}{numeric value indicating the score of the measure of interest.}
  \item{direction}{character value indicating the direction in which to perform the standardization. The default, "-1+1to01", can be switched to "01to-1+1".}
}
\details{
While most of the threshold-based measures of model evaluation range theoretically from 0 to 1, some of them (such as Cohen's kappa and the true skill statistic, TSS) may range from -1 to 1 (Allouche et al. 2006). Thus, the values of different measures may not be directly comparable (Barbosa 2015). We do not usually get negative values of TSS or kappa (nor values under 0.5 for CCR or AUC, for example) because that only happens when model predictions perform worse than random guesses; still, such values are mathematically possible, and can occur e.g. when extrapolating models to regions where where the species-environment relationships differ. This standardization is included as an option in the \code{\link{threshMeasures}} function.
}
\value{
The numeric value of 'score' when re-scaled to the 0-to-1 (or to the -1 to +1) scale.
}
\references{
Allouche O., Tsoar A. & Kadmon R. (2006) Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS). Journal of Applied Ecology 43: 1223-1232

Barbosa, A.M. (2015) Re-scaling of model evaluation measures to allow direct comparison of their values. The Journal of Brief Ideas, 18 Feb 2015, DOI: 10.5281/zenodo.15487
}
\author{
A. Marcia Barbosa
}
\note{
Note that this is not the same as re-scaling a vector so that it ranges between 0 and 1, which is done by \code{\link{range01}}.
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{range01}}
}
\examples{
standard01(0.6)

standard01(0.6, direction = "-1+1to01")

standard01(0.6, direction = "01to-1+1")
}
