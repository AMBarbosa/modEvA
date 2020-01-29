\name{multModEv}
\alias{multModEv}
\title{
Multiple model evaluation
}
\description{
If you have a list of GLM model objects (created, e.g., with the \code{multGLM} function of the 'fuzzySim' R-Forge package), or a data frame with presence-absence data and the corresponding predicted values for a set of species, you can use the \code{multModEv} function to get a set of evaluation measures for all models simultaneously, as long as they all have the same sample size.
}
\usage{
multModEv(models = NULL, obs.data = NULL, pred.data = NULL,
measures = modEvAmethods("multModEv"), standardize = FALSE, 
thresh = NULL, bin.method = NULL, verbosity = 0, ...)
}
\arguments{
  \item{models}{a list of model object(s) of class "glm", all applied to the same data set. Evaluation is based on the cases included in the models.}
  \item{obs.data}{a data frame with observed (training or test) binary data. This argument is ignored if 'models' is provided.}
  \item{pred.data}{a data frame with the corresponding predicted (training or test) values, with both rows and columns in the same order as in 'obs.data'. This argument is ignored if 'models' is provided. Note that, for calibration measures (based on \code{\link{HLfit}} or \code{\link{MillerCalib}}), the results are only valid if the input predictions represent probability.}
  \item{measures}{character vector of the evaluation measures to calculate. The default is all implemented measures, which you can check by typing 'modEvAmethods("multModEv")'. But beware: calibration measures (i.e., HL and Miller) are only valid if your predicted values reflect actual presence probability (not favourability, habitat suitability or others); you should exclude them otherwise.}
  \item{standardize}{logical, whether to standardize measures that vary between -1 and 1 to the 0-1 scale (see \code{\link{standard01}}). The default is FALSE.}
  \item{thresh}{argument to pass to \code{\link{threshMeasures}} if any of 'measures' is calculated by that function. The default is NULL, but a valid method must be specified if any of 'measures' is threshold-based - i.e., any of those in 'modEvAmethods("threshMeasures")'.}
  \item{bin.method}{the method with which to divide the data into groups or bins, for calibration or reliability measures such as \code{\link{HLfit}}. The default is NULL, but a valid method must be specified if 'measures' includes "HL" or "HL.p". Type modEvAmethods("getBins") for available options), and see \code{\link{HLfit}} and \code{\link{getBins}} for more information.}
  \item{verbosity}{integer specifying the amount of messages or warnings to display. Defaults to 0, but can also be 1 or 2 for more messages from the functions within.}
  \item{\dots}{optional arguments to pass to \code{\link{HLfit}} (if "HL" or "HL.p" are included in 'measures'), namely n.bins, fixed.bin.size, min.bin.size, min.prob.interval or quantile.type.}
}
\value{
A data frame with the value of each evaluation measure for each model.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{threshMeasures}}
}
\examples{
data(rotif.mods)

eval1 <- multModEv(models = rotif.mods$models[1:6], thresh = 0.5, 
bin.method = "n.bins", fixed.bin.size = TRUE)

head(eval1)


eval2 <- multModEv(models = rotif.mods$models[1:6], 
thresh = "preval", measures = c("AUC", "AUCPR", "CCR", 
"Sensitivity", "TSS"))

head(eval2)


# you can also calculate evaluation measures for a set of 
# observed vs predicted data, rather than from model objects:

obses <- sapply(rotif.mods$models, `[[`, "y")
preds <- sapply(rotif.mods$models, `[[`, "fitted.values")

eval3 <- multModEv(obs.data = obses[ , 1:4], 
pred.data = preds[ , 1:4], thresh = "preval", 
bin.method = "prob.bins")

head(eval3)
}
