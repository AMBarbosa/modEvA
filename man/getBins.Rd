\name{getBins}
\alias{getBins}
\title{
Get bins of continuous values.
}
\description{
Get continuous predicted values into bins according to specific criteria.
}
\usage{
getBins(model = NULL, obs = NULL, pred = NULL, id = NULL, 
bin.method, n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15,
min.prob.interval = 0.1, quantile.type = 7, simplif = FALSE,
verbosity = 2)
}
\arguments{
  \item{model}{a model object of class \code{"glm"}.
}
  \item{obs}{
a vector of 1-0 values of a modelled binary variable. This argument is ignored if \code{model} is provided.
}
  \item{pred}{
a vector of the corresponding predicted values. This argument is ignored if \code{model} is provided.
  }
  \item{id}{
optional vector of row identifiers; must be of the same length and in the same order of \code{obs} and \code{pred} (or of the cases used to build \code{model})
}
  \item{bin.method}{
the method with which to divide the values into bins. Type modEvAmethods("getBins") for available options and see Details for more information on these methods.
}
  \item{n.bins}{
the number of bins in which to divide the data.
}
  \item{fixed.bin.size}{
logical, whether all bins should have (approximally) the same size.
}
  \item{min.bin.size}{
integer value defining the minimum number of observations to include in each bin. The default is 15, the minimum required for accurate comparisons within bins (Jovani & Tella 2006, Jimenez-Valverde et al. 2013).
}
  \item{min.prob.interval}{
minimum range of probability values in each bin. The default is 0.1.
}
  \item{quantile.type}{
argument to pass to \code{\link{quantile}} specifying the algorithm to use if bin.method = "quantiles". The default is 7 (the \code{\link{quantile}} default in R), but check out other types, e.g. 3 (used by SAS), 6 (used by Minitab and SPSS) or 5 (appropriate for deciles, which correspond to the default n.bins = 10).
}
  \item{simplif}{logical, whether to calculate a faster, simplified version (used internally in other functions). The default is FALSE.}
  \item{verbosity}{integer specifying the amount of messages or warnings to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
}
\details{
Mind that different \code{bin.method}s can lead to visibly different results regarding the bins and any operations that depend on them (such as \code{\link{HLfit}}). Currently available \code{bin.method}s are:

  - \code{round.prob}: probability values are rounded to the number of digits of \code{min.prob.interval} - e.g., if min.prob.interval = 0.1 (the default), values under 0.05 get into bin 1 (rounded probability = 0), values between 0.05 and 0.15 get into bin 2  (rounded probability = 0.1), etc. until values with probability over 0.95, which get into bin 11. Arguments n.bins, fixed.bin.size and min.bin.size are ignored by this bin.method.
  
  - \code{prob.bins}: probability values are grouped into bins of the given probability intervals - e.g., if min.prob.interval = 0.1 (the default), bin 1 gets the values between 0 and 0.1, bin 2 gets the values between 0.1 and 0.2, etc. until bin 10 which gets the values between 0.9 and 1. Arguments n.bins, fixed.bin.size and min.bin.size are ignored by this bin.method.
  
  - \code{size.bins}: probability values are grouped into bins of (approximately) equal size, defined by argument min.bin.size. Arguments n.bins and min.prob.interval are ignored by this bin.method.
  
  - \code{n.bins}: probability values are divided into the number of bins given by argument n.bins, and their sizes may or may not be forced to be (approximately) equal, depending on argument fixed.bin.size (which is FALSE by default). Arguments min.bin.size and min.prob.interval are ignored by this bin.method.
  
  - \code{quantiles}: probability values are divided using R function \code{\link{quantile}}, with probability cutpoints defined by the given n.bins (i.e., deciles by default), and with the quantile algorithm defined by argument quantile.type. Arguments fixed.bin.size, min.bin.size and min.prob.interval are ignored by this bin.method.
}
\value{
The output of \code{getBins} is a list with the following components:
\item{prob.bin}{the first and last value of each bin}
\item{bins.table}{a data frame with the sample size, number of presences, number of absences, prevalence, mean and median probability, and the difference between predicted and observed values (mean probability - observed prevalence) in each bin.}
\item{N}{the total number of observations in the analysis.}
\item{n.bins}{the total number of bins obtained.}
}
\references{
Jimenez-Valverde A., Acevedo P., Barbosa A.M., Lobo J.M. & Real R. (2013) Discrimination capacity in species distribution models depends on the representativeness of the environmental domain. Global Ecology and Biogeography 22: 508-516.

Jovani R. & Tella J.L. (2006) Parasite prevalence and sample size: misconceptions and solutions. Trends in Parasitology 22: 214-218.
}
\author{
A. Marcia Barbosa
}
\note{
This function is still under development and may fail for some datasets and binning methods (e.g., ties may sometimes preclude binning under some bin.methods). Fixes and further binning methods are in preparation. Feedback is welcome.
}
\seealso{
\code{\link{HLfit}}}
\examples{
# load sample models:

data(rotif.mods)


# choose a particular model to play with:

mod <- rotif.mods$models[[1]]


# try getBins using different binning methods:

getBins(model = mod, bin.method = "quantiles")

getBins(model = mod, bin.method = "n.bins")

getBins(model = mod, bin.method = "n.bins", fixed.bin.size = TRUE)
}
