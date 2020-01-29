\name{optiThresh}
\alias{optiThresh}
\title{
Optimize threshold for model evaluation.
}
\description{
The 'optiThresh' function calculates optimal thresholds for a number of model evaluation measures (see \code{\link{threshMeasures}}). Optimization is given for each measure, and/or for all measures according to particular criteria (e.g. Jimenez-Valverde & Lobo 2007; Liu et al. 2005; Nenzen & Araujo 2011). Results are given numerically and in plots.}
\usage{
optiThresh(model = NULL, obs = NULL, pred = NULL, interval = 0.01,
measures = modEvAmethods("threshMeasures"),  
optimize = modEvAmethods("optiThresh"), simplif = FALSE, 
plot = TRUE, sep.plots = FALSE, xlab = "Threshold", ...)
}
\arguments{
  \item{model}{a model object of class "glm".}
  \item{obs}{a vector of observed presences (1) and absences (0) or another binary response variable. This argument is ignored if 'model' is provided.}
  \item{pred}{a vector with the corresponding predicted values of presence probability, habitat suitability, environmental favourability or alike. This argument is ignored if 'model' is provided.}
  \item{interval}{numeric value between 0 and 1 indicating the interval between the thresholds at which to calculate the evaluation measures. Defaults to 0.01.}
  \item{measures}{character vector indicating the names of the model evaluation measures for which to calculate optimal thresholds. The default is using all measures available in 'modEvAmethods("threshMeasures")'.}
  \item{optimize}{character vector indicating the threshold optimization criteria to use; "each" calculates the optimal threshold for each model evaluation measure, while the remaining options optimize all measures according to the specified criterion. The default is using all criteria available in 'modEvAmethods("optiThresh")'.}
  \item{simplif}{logical, whether to calculate a faster simplified version. Used internally in other functions.}
  \item{plot}{logical, whether to plot the values of each evaluation measure at all thresholds.}
  \item{sep.plots}{logical. If TRUE, each plot is presented separately (you need to be recording R plot history to be able to browse through them all); if FALSE(the default), all plots are presented together in the same plotting window.}
  \item{xlab}{character vector indicating the label of the x axis.}
  \item{\dots}{additional arguments to pass to \code{\link{plot}}.}
}
\value{
This function returns a list with the following components:
  \item{all.thresholds}{a data frame with the values of all analysed measures at all analysed thresholds.}
  \item{optimals.each}{if "each" is among the threshold criteria specified in 'optimize', optimals.each is output as a data frame with the value of each measure at its optimal threshold, as well as the type of optimal for that measure (which may be the maximum for measures of goodness such as "Sensitivity", or the minimum for measures of badness such as "Omission").}
  \item{optimals.criteria}{a data frame with the values of measure at the threshold that maximizes each of the criteria specified in 'optimize' (except for "each", see above).}
}
\note{
"Sensitivity" is the same as "Recall", and "PPP" (positive predictive power) is the same as "Precision". "F1score"" is the harmonic mean of precision and recall.
}
\references{
Jimenez-Valverde A. & Lobo J.M. (2007) Threshold criteria for conversion of probability of species presence to either-or presence-absence. Acta Oecologica 31: 361-369.

Liu C., Berry P.M., Dawson T.P. & Pearson R.G. (2005) Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28: 385-393.

Nenzen H.K. & Araujo M.B. (2011) Choice of threshold alters projections of species range shifts under climate change. Ecological Modelling 222: 3346-3354.
}
\author{
A. Marcia Barbosa
}
\note{
Some measures cannot be calculated for thresholds at which there are zeros in the confusion matrix, hence the eventual 'NaN' or 'Inf' in results. Also, optimization may be deceiving for some measures; use 'plot = TRUE' and inspect the plot(s).
}
\seealso{
\code{\link{threshMeasures}}, \code{\link{optiPair}}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

\dontrun{
optiThresh(model = mod)
}


# change some of the parameters:

optiThresh(model = mod, pch = 20, 
measures = c("CCR", "Sensitivity", "kappa", "TSS"), ylim = c(0, 1))


# you can also use optiThresh with vectors of observed and predicted
# values instead of with a model object:

\dontrun{
optiThresh(obs = mod$y, pred = mod$fitted.values, pch = ".")
}
}
