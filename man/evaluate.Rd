\name{evaluate}
\alias{evaluate}
\title{
Evaluate a model based on the elements of a confusion matrix.
}
\description{
This function evaluates the classification performance of a model based on the values of a confusion matrix obtained at a particular threshold.
}
\usage{
evaluate(a, b, c, d, N = NULL, measure = "CCR")
}
\arguments{
  \item{a}{number of correctly predicted presences (true positives)}
  \item{b}{number of absences incorrectly predicted as presences (false positives)}
  \item{c}{number of presences incorrectly predicted as absences (false negatives)
}
  \item{d}{number of correctly predicted absences (true negatives)}
  \item{N}{total number of cases. If NULL (the default), it is calculated automatically by adding up a, b, c and d.)}
  \item{measure}{a character vector of length 1 indicating the evaluation measure to use. Type \code{modEvAmethods("threshMeasures")} for available options.}
}
\details{
A number of measures can be used to evaluate continuous model predictions against observed binary occurrence data (Fielding & Bell 1997; Liu et al. 2011; Barbosa et al. 2013; Leroy et al. 2018). The 'evaluate' function can calculate a few threshold-based classification measures from the values of a confusion matrix obtained at a particular threshold. The 'evaluate' function is used internally by \code{\link{threshMeasures}}. It can also be accessed directly by the user, but it is usually more practical to use 'threshMeasures', which calculates the confusion matrix automatically.
}
\value{
Numeric value of the specified evaluation measure.
}
\note{
Some measures (e.g. NMI, odds ratio) don't work with zeros in (some parts of) the confusion matrix. Also, TSS and NMI are not symmetrical, i.e. "obs" vs "pred" is different from "pred" vs "obs".
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions, 19: 1333-1338

Fielding A.H. & Bell J.F. (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49

Leroy B., Delsol R., Hugueny B., Meynard C.M., Barhoumi C., Barbet-Massin M. & Bellard C. (2018) Without quality presence-absence data, discrimination metrics such as TSS can be misleading measures of model performance. Journal of Biogeography 45(9):1994-2002

Liu C., White M., & Newell G. (2011) Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography, 34, 232-243.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
evaluate(23, 44, 21, 34)

evaluate(23, 44, 21, 34, measure = "TSS")
}
