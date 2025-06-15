\name{MESS}
\alias{MESS}
\title{
Multivariate Environmental Similarity Surfaces based on a data frame
}
\description{
This function performs the MESS analysis of Elith et al. (2010) to determine
the extent of the environmental differences between model training and model
projection (extrapolation) data. It is applicable to variables in a matrix
or data frame.
}
\usage{
MESS(V, P, id.col = NULL, verbosity = 2)
}
\arguments{
  \item{V}{a matrix or data frame containing the variables (one in each column)
in the training dataset.}
  \item{P}{a matrix or data frame containing the same variables in the area to
  which the model(s) will be projected. Variables (columns) must be in the same
  order as in \code{V}, and \code{colnames(P)} must exist.}
  \item{id.col}{optionally, the index number of a column containing the row identifiers in P. If provided, this column will be excluded from MESS calculations but included in the output.}
  \item{verbosity}{Integer number indicating the amount of messages to display while computing the results. The default is to display all messages. Set verbosity=0 for no messages.}
}
\details{
When model predictions are projected into regions, times or spatial resolutions
not analysed in the training data, it may be important to measure the similarity
between the new environments and those in the training sample (Elith et al.
2010), as models are not so reliable when predicting outside their domain
(Barbosa et al. 2009). The Multivariate Environmental Similarity Surfaces (MESS)
analysis measures the similarity in the analysed variables between any given
locality in the projection dataset and the localities in the reference
(training) dataset (Elith et al. 2010).

MESS analysis is implemented in the MAXENT software (Phillips et al.
2006) and in the \pkg{dismo} R package, but there it requires input
variables in raster format. This implies not only the use of complex spatial
data structures, but also that the units of analysis are rectangular pixels,
whereas we often need to model distribution data recorded on less regular
units (e.g. provinces, river basins), or on equal-area cells that are not
necessarily rectangular (e.g. UTM cells, equal-area hexagons or other geometric
shapes). The MESS function computes this analysis for variables in a data frame,
where localities (in rows) may be of any size or shape.
}
\value{
The function returns a data frame with the same column names as \code{P},
plus a column named \code{TOTAL}, quantifying the similarity between each
point in the projection dataset and those in the reference dataset. Negative
values indicate localities that are environmentally dissimilar from the
reference region. The last column, \code{MoD}, indicates which of the column
names of P corresponds to the most dissimilar variable, i.e., the limiting
factor or the variable that drives the MESS in that locality (Elith et al. 2010).
}
\references{
Barbosa A.M., Real R. & Vargas J.M. (2009) Transferability of environmental favourability models in geographic space: the case of the Iberian desman (Galemys pyrenaicus) in Portugal and Spain. Ecological Modelling 220: 747-754

Elith J., Kearney M. & Phillips S. (2010) The art of modelling range-shifting
species. Methods in Ecology and Evolution 1: 330-342

Mesgaran M.B., Cousens R.D. & Webber B.L. (2014) Here be dragons: a tool for quantifying novelty due to covariate range and correlation change when projecting species distribution models. Diversity and Distributions, 20: 1147-1159

Owens H.L., Campbell L.P., Dornak L.L., Saupe E.E., Barve N., Soberon J., Ingenloff K., Lira-Noriega A., Hensz C.M., Myers C.E. & Peterson A.T. (2013) Constraints on interpretation of ecological niche models by limited environmental ranges on calibration areas. Ecological Modelling, 263: 10-18

Phillips S.J., Anderson R.P. & Schapire R.E. (2006) Maximum entropy modeling
of species geographic distributions. Ecological Modelling 190: 231-259
}
\author{
Alberto Jimenez-Valverde, A. Marcia Barbosa
}
\note{
Newer and apparently more complete methods for analysing environmental dissimilarities have been developed, such as extrapolation detection (ExDet; Mesgaran et al. 2014) and Mobility-Oriented Parity analysis (MOP; Owens et al. 2013).
}
\seealso{
\code{\link{OA}}; \code{mess} in packages \pkg{dismo} and \pkg{predicts}; \code{ecospat.climan} in package \pkg{ecospat}; \code{kuenm_mop} and \code{kuenm_mmop} in package \pkg{kuenm}
}
\examples{
\dontrun{
# load package 'fuzzySim' and its sample data:
require(fuzzySim)
data(rotif.env)


# add a column specifying the hemisphere:

unique(rotif.env$CONTINENT)

rotif.env$HEMISPHERE <- "Eastern"

rotif.env$HEMISPHERE[rotif.env$CONTINENT \%in\%
c("NORTHERN_AMERICA", "SOUTHERN_AMERICA")] <- "Western"

head(rotif.env)


# perform a MESS analysis
# suppose you'll extrapolate models from the Western hemisphere (Americas)
# to the Eastern hemisphere (rest of the world):

names(rotif.env)  # variables are in columns 5:17

west <- subset(rotif.env, HEMISPHERE == "Western", select = 5:17)
east <- subset(rotif.env, HEMISPHERE == "Eastern", select = 5:17)
east.with.ID <- subset(rotif.env, HEMISPHERE == "Eastern",
select = c(1, 5:17))

head(east)
head(east.with.ID)  # ID is in column 1

mess <- MESS(V = west, P = east)
mess.with.ID <- MESS(V = west, P = east.with.ID, id.col = 1)

head(mess)
head(mess.with.ID)

range(mess[ , "TOTAL"])
}
}
