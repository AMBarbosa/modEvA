\name{ptsrast2obspred}
\alias{ptsrast2obspred}
\title{
Observed and predicted values from presence points and a raster map.
}
\description{
This function takes presence points or coordinates and a raster map of model predictions, and it returns a data frame with two columns containing, respectively, the observed (presence or no presence) and the predicted value for each pixel. Duplicate points (i.e., points falling in the same pixel, whether or not they have the exact same coordinates) can be kept or removed.
}
\usage{
ptsrast2obspred(pts, rst, rm.dup = FALSE, na.rm = FALSE, verbosity = 2)
}
\arguments{
  \item{pts}{a 'SpatVector' map of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.}
  \item{rst}{a one-layer 'SpatRaster' map of the model predictions, in the same CRS as 'pts'. If you have a raster map in another format, you can try to convert it with 'terra::rast()'}
  \item{rm.dup}{logical, whether repeated points within the same pixel should be removed. See Examples. The default is FALSE.}
  \item{na.rm}{logical, whether presence points with missing or non-finite values of 'rst' should be excluded from the output. The default is FALSE.}
    \item{verbosity}{integer value indicating the amount of messages to display. Defaults to 2, for the maximum amount of messages.}
}
\value{
This function outputs a data frame with one column containing the observed (1 for presence, 0 for absence) and another column containing the corresponding predicted values from 'rst'.
}
\author{
A. Marcia Barbosa
}
\examples{
\dontrun{
# you can run these examples if you have the 'terra' package installed

require(terra)

# get an example raster map:
rst <- terra::rast(system.file("ex/elev.tif", package = "terra"))
rst <- terra::aggregate(rst, 10)

plot(rst)

# generate some random presence points within it:
set.seed(8)
presences <- terra::spatSample(as.polygons(ext(rst)), 10)

plot(presences, add = TRUE)


# use 'ptsrast2obspred' on this points + raster data:

# without removing duplicates (the default):
obspred <- ptsrast2obspred(pts = crds(presences), rst = rst)
obspred
nrow(obspred)  # you get as many 'obs' as pixels + additional points per pixel
sum(obspred$obs)  # as many presences as points that overlay 'pred'

# with removal of duplicates:
obspred_rmdup <- ptsrast2obspred(pts = crds(presences), rst = rst[[1]], 
rm.dup = TRUE)  # you get as many 'obs' as pixels
obspred_rmdup
nrow(obspred_rmdup)  # you get as many 'obs' as pixels
sum(obspred_rmdup$obs)  # as many presences as pixels that contain (one or more) points
}
}
