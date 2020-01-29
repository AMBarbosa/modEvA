\name{varPart}
\alias{varPart}
\title{
Variation partitioning
}
\description{
This function performs variation partitioning (Borcard et al. 1992) among two factors (e.g. Ribas et al. 2006) or three factors (e.g. Real et al. 2003) for either multiple linear regression models (LM) or generalized linear models (GLM).
}
\usage{
varPart(A, B, C = NA, AB, AC = NA, BC = NA, ABC = NA, 
model.type = NULL, A.name = "Factor A", B.name = "Factor B", 
C.name = "Factor C", plot = TRUE, plot.digits = 3, cex.names = 1.5, 
cex.values = 1.2, main = "", cex.main = 2, plot.unexpl = TRUE)
}
\arguments{
  \item{A}{numeric value of the R-squared of the regression of the response variable on the variables related to factor 'A'}
  \item{B}{numeric value of the R-squared of the regression of the response variable on the variables related to factor 'B'}
  \item{C}{(optionally, if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables related to factor 'C'}
  \item{AB}{numeric value of the R-squared of the regression of the response on the variables of factors 'A' and 'B' simultaneously}
  \item{AC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'A' and 'C' simultaneously}
  \item{BC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'B' and 'C' simultaneously
}
  \item{ABC}{(if there are 3 factors) numeric value of the R-squared of the regression of the response on the variables of factors 'A', 'B' and 'C' simultaneously}
  \item{model.type}{deprecated argument, kept here for back-compatibility}
  \item{A.name}{character string indicating the name of factor 'A'}
  \item{B.name}{character string indicating the name of factor 'B'
}
  \item{C.name}{character string indicating the name of factor 'C' (if there are 3 factors)}
  \item{plot}{logical, whether to plot the variation partitioning diagram. The default is TRUE.}
  \item{plot.digits}{integer value of the number of digits to which to \code{\link{round}} the values in the plot. The default is 3.}
  \item{cex.names}{numeric value indicating character expansion factor to define the size of the names of the factors displayed in the plot.}
  \item{cex.values}{numeric value indicating character expansion factor to define the size of the values displayed in the plot.}
  \item{main}{optional character string indicating the main title for the plot. The default is empty.}
  \item{cex.main}{numeric value indicating character expansion factor to define the font size of the plot title (if provided).}
  \item{plot.unexpl}{logical value indicating whether the amount of unexplained variation should be included in the plot. The default is TRUE.}
}
\details{
If you have linear models, input data for 'varPart' are the coefficients of determination (R-squared values) of the linear regressions of the target variable on all the variables in the model, on the variables related to each particular factor, and (when there are 3 factors) on the variables related to each pair of factors. The outputs are the amounts of variance explained exclusively by each factor, the amounts explained exclusively by the overlapping effects of each pair of factors, and the amount explained by the overlap of the 3 factors if this is the case (e.g. Real et al. 2003). The amount of variation not explained by the complete model is also provided.

If you have generalized linear models (GLMs) such as logistic regression (see \code{\link{glm}}), you have no true R-squared values; inputs can then be the squared coefficients of correlation between the model predictions given by each factor (or pair of factors) and the predictions of the complete model (e.g. Munoz & Real 2006), or the R-squared values of the corresponding logit (y) functions (Real et al. 2013), or an adjusted R-squared (De Araujo et al. 2013). In these cases, the "total variation" (AB or ABC, depending on whether you have two or three factors) is 1 (correlation of the predictions of the complete model with themselves), and output values are not the total amounts of variance (of the target variable) explained by factors and overlaps, but rather their proportional contribution to the total variation explained by the model.}
\value{
The output consists of a data frame indicating the proportion of variance accounted for by each of the factors, and (if 'plot = TRUE') a Venn diagram of the contributions of each factor.
}
\references{
Borcard D., Legendre P., Drapeau P. (1992) Partialling out the spatial component of ecological variation. Ecology 73: 1045-1055

De Araujo C.B., Marcondes-Machado L.O. & Costa G.C. (2013) The importance of biotic interactions in species distribution models: a test of the Eltonian noise hypothesis using parrots. Journal of Biogeography, early view (DOI: 10.1111/jbi.12234)

Munoz A.-R. & Real R. (2006) Assessing the potential range expansion of the exotic monk parakeet in Spain. Diversity and Distributions 12: 656-665.

Real R., Barbosa A.M., Porras D., Kin M.S., Marquez A.L., Guerrero J.C., Palomo L.J., Justo E.R. & Vargas J.M. (2003) Relative importance of environment, human activity and spatial situation in determining the distribution of terrestrial mammal diversity in Argentina. Journal of Biogeography 30: 939-947.

Real R., Romero D., Olivero J., Estrada A. & Marquez A.L. (2013) Estimating how inflated or obscured effects of climate affect forecasted species distribution. PLoS ONE 8: e53646.

Ribas A., Barbosa A.M., Casanova J.C., Real R., Feliu C. & Vargas J.M. (2006) Geographical patterns of the species richness of helminth parasites of moles (Talpa spp.) in Spain: separating the effect of sampling effort from those of other conditioning factors. Vie et Milieu 56: 1-8.
}
\author{
A. Marcia Barbosa
}
\note{
These results derive from arithmetic operations between your input values, and they always sum up to 1; if your input is incorrect, the results will be incorrect as well, even if they sum up to 1.

This function had a bug up to modEvA version 0.8: a badly placed line break prevented the ABC overlap from being calculated correctly. Thanks to Jurica Levatic for pointing this out and helping to solve it!
}
\examples{
# if you have a linear model (LM), use (non-adjusted) R-squared values 
# for each factor and for their combinations as inputs:

varPart(A = 0.456, B = 0.315, C = 0.281, AB = 0.051, BC = 0.444, 
AC = 0.569, ABC = 0.624, A.name = "Spatial", B.name = "Human", 
C.name = "Environmental", main = "Small whale")


# if you have a generalized linear model (GLM), 
# you can use squared correlation coefficients of the
# predictions of each factor with those of the complete model:

varPart(A = (-0.005)^2, B = 0.698^2, C = 0.922^2, AB = 0.696^2, 
BC = 0.994^2, AC = 0.953^2, ABC = 1, A.name = "Topographic", 
B.name = "Climatic", C.name = "Geographic", main = "Big bird")

# but "Unexplained variation" can be deceiving in these cases
# (see Details); try also adding 'plot.unexpl = FALSE'
}
