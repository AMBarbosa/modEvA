\name{poMeasures}
\alias{poMeasures}
\title{Strictly presence-only measures of model evaluation}

\description{
THIS FUNCTION IS EXPERIMENTAL AND IMPLEMENTS METHODS THAT ARE STILL UNDER SCIENTIFIC REVIEW. It computes a set of truly presence-only evaluation metrics for model predictions, including bin-dependent and bin-independent approaches. The function evaluates how the continuous predicted values (at presence sites only) relate to the number or density of observed presences, using correlation-based measures and distributional comparisons. Optional diagnostic plots are also produced.
}

\usage{
  poMeasures(
    model = NULL,
    obs = NULL,
    pred = NULL,
    n.bins = 100,
    bin.width = "default",
    method = "spearman",
    rm.dup.classes = NA,
    rm.dup.points = FALSE,
    simplif = FALSE,
    plot = TRUE,
    plot.lines = TRUE,
    plot.values = TRUE,
    cex.values = 1,
    plot.digits = 3,
    main = c("Presence-only bin-based", "Presence-only density-based"),
    na.rm = TRUE,
    verbosity = 2,
    ...
  )
}

\arguments{
  \item{model}{optional binary-response model object of class "glm", "gam", "gbm", "randomForest" or "bart", producing prediciotns between 0 and 1. If this argument is provided, 'obs' and 'pred' will be extracted with \code{\link{mod2obspred}}. Alternatively, you can input the 'obs' and 'pred' arguments (e.g. for external test data) instead of 'model'.}
  \item{obs}{alternatively to 'model' and (optionally) together with 'pred', a numeric vector of observed presences (1) and absences (0) of a binary response variable. Alternatively (and if 'pred' is a 'SpatRaster'), a two-column matrix or data frame containing, respectively, the x (longitude) and y (latitude) coordinates of the presence points, in which case the 'obs' vector will be extracted with \code{\link{ptsrast2obspred}}. This argument is ignored if 'model' is provided. If used, rows where obs = 0 or pixels with no presence points are removed before calculations.}
  \item{pred}{alternatively to 'model' and (optionally) together with 'obs', a vector with the corresponding predicted values of habitat suitability, environmental favourability or alike (presence probability is not appropriate for these measures, unless model training prevalence equals 0.5). Values should lie between 0 and 1. If 'obs' is provided, 'pred' must be of the same length and in the same order. Alternatively (and if 'obs' is a set of point coordinates), a 'SpatRaster' map of the predicted values (at presence sites only, unless 'obs' coordinates are provided), in which case the 'pred' vector will be extracted with \code{\link{ptsrast2obspred}}. Values should lie between 0 and 1. This argument is ignored if 'model' is provided.}
  \item{n.bins}{argument to pass to \code{\link{getBins}} indicating the number of bins in which to divide the data.}
  \item{bin.width}{argument to pass to \code{\link{getBins}} indicating the width of the moving window. The default is 1/10th of the 'pred' range.}
  \item{method}{argument to pass to \code{\link{cor.test}} specifying the correlation method to use for POGI. The default is "spearman", as in the initial publication on the \code{\link{Boyce}} index on which these measures are inspired, but "kendall" or "pearson" could be more informative in many cases.}
  \item{rm.dup.classes}{(placeholder, as in \code{\link{Boyce}}, but currently not used here).}
  \item{rm.dup.points}{if \code{TRUE} and if 'pred' is a SpatRaster and if there are >1 points within the same pixel, a maximum of one point per pixel is used to compute the presences. See examples in \code{\link{ptsrast2obspred}}. The default is FALSE.}
  \item{simplif}{logical value (default FALSE) indicating whether to return just a simplified output containing only the value estimate for each measure, with no associated statistics.}
  \item{plot}{Logical; if \code{TRUE} (default), plot the values on which the measures are based.}
  \item{plot.lines}{ (if plot=TRUE) logical value (default TRUE) indicating whether or not to add lines connecting the points in the moving bins plot. Missing value (NA) bins are not connected (see \code{\link{lines}}).}
  \item{plot.values}{logical (default TRUE), whether or not to show the measure values on the plots.}
  \item{cex.values}{Character expansion (size) for plot values text.}
  \item{plot.digits}{(if plot=TRUE and plot.values=TRUE) number of digits (default 3) to which the values in the plots should be \code{\link{round}}ed.}
  \item{main}{(if plot=TRUE) main titles for the two plots.}
  \item{na.rm}{Logical (default TRUE) indicating if missing values should be removed from computations.}
  \item{verbosity}{integer specifying the amount of console messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{(if plot=TRUE) additional graphical parameters that can be passed to \code{plot()}.}
}
\value{
  A list with the following components:
    
  \item{bins}{A data frame containing bin statistics from \code{\link{getBins}}.}
  \item{Nbins}{Number of bins with (not missing, non-NA) data.}
  \item{bPOGI}{bin-based Presence-Only Gradient Index.}
  \item{dPOGI}{density-based (bin-independent) Presence-Only Gradient Index.}
  \item{PODS}{Presence-Only Distributional Shift (PODS).}
  \item{N}{Number of predicted values (at presence sites if 'obs' provided).}
  
  If \code{plot = TRUE}, two diagnostic plots are also produced.
}

\seealso{
  \code{\link{getBins}}, \code{\link{Boyce}}, \code{\link{cor.test}}, \code{\link{wilcox.test}}
}

\examples{
# load sample models:
data(rotif.mods)

# choose a particular model to play with:
mod <- rotif.mods$models[[18]]

# compute the PO measures:
# poMeasures(model = mod)


# you can also use vectors of observed and predicted values
# instead of a model object:

obs <- mod$y
pred <- mod$fitted.values

poMeasures(obs = obs, pred = pred)


# 'obs' can also be a table of presence point coordinates
# and 'pred' a SpatRaster of predicted values
}
