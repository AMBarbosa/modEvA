\name{modEvA-package}
\alias{modEvA-package}
\alias{modEvA}
\docType{package}
\title{Model Evaluation and Analysis}
\description{
The modEvA package can analyse species distribution models and evaluate their performance. It includes functions for performing variation partitioning; calculating several measures of model discrimination, classification, explanatory power, and calibration; optimizing prediction thresholds based on a number of criteria; performing multivariate environmental similarity surface (MESS) analysis; and displaying various analytical plots.
}
\details{
\tabular{ll}{
Package: \tab modEvA\cr
Type: \tab Package\cr
Version: \tab 3.46\cr
Date: \tab 2026-08-04\cr
License: \tab GPL-3\cr
}
}
\author{
Barbosa A.M., Brown J.A., Jimenez-Valverde A., Real R.

A. Marcia Barbosa <ana.marcia.barbosa@gmail.com>
}
\references{
Barbosa A.M., Real R., Munoz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity and Distributions 19: 1333-1338 (DOI: 10.1111/ddi.12100)
}
\keyword{ package }
\seealso{
\pkg{PresenceAbsence}, \pkg{ROCR}, \pkg{verification}, \pkg{Metrics}
}
\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[1]]

# plot this model:
plotGLM(model = mod)

# compute the Root Mean Square Error and other error measures:
errorMeasures(model = mod)

# extract variable importance from the model:
varImp(model = mod)
varImp(model = mod, imp.type = "perm")

# compute the area under the ROC curve for the model:
AUC(model = mod)
AUC(model = mod, curve = "PR")

# compute some threshold-based measures for this model:
threshMeasures(model = mod, thresh = 0.5)
threshMeasures(model = mod, thresh = "preval")
threshMeasures(model = mod, thresh = "maxTSS")

# compute similarity (also threshold-based) measures:
similarity(model = mod, thresh = "maxTSS")

# compute optimal thresholds based on several criteria:
optiThresh(model = mod, measures = c("CCR", "Sensitivity", "kappa", "TSS"),
ylim = c(0, 1), pch = 20, cex = 0.5)

# compute the optimal threshold balancing two evaluation measures:
optiPair(model = mod, measures = c("Sensitivity", "Specificity"))

# compute model calibration (reliability) metrics:
HLfit(model = mod, bin.method = "n.bins")
Miller(model = mod)
Boyce(model = mod)
Dsquared(model = mod)
pseudoRsq(model = mod)

# compute a bunch of evaluation measures for a set of models:
multModEv(models = rotif.mods$models[1:4], thresh = "preval",
bin.method = "quantiles")
}
