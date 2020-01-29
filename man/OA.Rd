\name{OA}
\alias{OA}
\title{Overlap Analysis}
\description{
This function analyses the range of values of the given environmental variables at the sites where a species has been recorded present.
}
\usage{
OA(data, sp.cols, var.cols)
}
\arguments{
  \item{data}{a data frame with your species' occurrence data and the predictor variables.}
  \item{sp.cols}{index number of the column containing the occurrence data of the species to be modelled. Currently only one species can be analysed at a time.}
  \item{var.cols}{index numbers of the columns containing the predictor variables to be used.}
}
\details{
Overlap Analysis is one of the simplest forms of modelling species' distributions. It assesses the ranges of values of the given environmental variables at the sites where a species has been recorded present, and predicts where that species should be able to occur based on those presence data (e.g. Brito et al. 1999, Arntzen & Teixeira 2006).

OA can also be useful when extrapolating models outside their original scope (geographical area, time period or spatial resolution), as it can identify which localities are within the model's domain - i.e., within the analysed ranges of values of the variables, outside which the model may not be reliable (e.g. Barbosa et al. 2009). In this case, the response is not a species' presence, but rather the sites that have been included in the model. See also the \code{\link{MESS}} function for a comparison between modelled and extrapolation environments.

Input data for the \code{OA} function are a vector or column with ones and zeros (presences vs. absences of a species if we want to model its occurrence, or modelled vs. non-modelled sites if we want to know which non-modelled sites are within the modelled range), and a matrix or data frame with the corresponding values of the environmental variables to consider (one variable in each column, values in rows).
}
\value{
A binary vector whith 1 where the values of all predictors lie within the ranges observed for the presence records, and 0 otherwise.
}
\references{
Arntzen J.W, Teixeira J. (2006) History and new developments in the mapping and modelling of the distribution of the golden-striped salamander, Chioglossa lusitanica. Zeitschrift fur Feldherpetologie, Supplement: 1-14.

Barbosa, A.M., Real, R. & Vargas, J.M. (2009) Transferability of environmental favourability models in geographic space: the case of the Iberian desman (Galemys pyrenaicus) in Portugal and Spain. Ecological Modelling 220: 747-754.

Brito J.C., Crespo E.G., Paulo O.S. (1999) Modelling wildlife distributions: Logistic Multiple Regression vs Overlap Analysis. Ecography 22: 251-260.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{MESS}}
}
\examples{
\dontrun{
# load package 'fuzzySim' and its sample data:
require(fuzzySim)
data(rotif.env)

names(rotif.env)

OA(rotif.env, sp.cols = 18, var.cols = 5:17)
}
}
