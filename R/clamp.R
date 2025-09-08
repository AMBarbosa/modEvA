# set each layer to vary within the range of values in the training data

clamp <- function(vars, # a SpatRaster or an object inheritting class data frame containing the variables to be clamped, one in each layer or column
                  train,  #  a SpatRaster, points SpatVector, or object inheritting class data frame containing the localities used in model training (including presences and pseudoabsences or background). If it is a non-spatial data frame, it should have only two columns specifying, respectively, the X and Y (longitude and latitude) coordinates (in this order!), in the same CRS as 'vars'.
                  coord.cols,
                  var.cols)
{
  # version 1.0 (23 Jul 2025)
  
  if (inherits(train, "SpatVector")) {
    if (!terra::same.crs(vars, train)) {
      message("Projecting 'train' to the same CRS as 'vars'.")
      train <- terra::project(train, vars)
    }
  } else {
    if (ncol(train) != 2)
      stop("If not a spatial object, 'train' must have only two columns\nwith the X (longitude) and Y (latitude) coordinates, respectively.")
    message("Assuming 'train' has the same CRS as 'vars'.")
    train <- terra::vect(train, geom = names(train), crs = terra::crs(vars))
  }
  
  if (inherits(vars, "SpatRaster")) {
    vars_train <- terra::mask(vars, train)
    vals_train <- as.data.frame(vars_train)
    n_layers <- terra::nlyr(vars)
    vars_out <- vector("list", n_layers)
    for (v in 1:n_layers) {
      vars_out[[v]] <- terra::clamp(vars[[v]], lower = min(vals_train[ , v], na.rm = TRUE), upper = max(vals_train[ , v], na.rm = TRUE))
    }
    
    return(terra::rast(vars_out))
  } else {  # end if SpatRaster
    
    vars <- as.data.frame(vars)  # accommodates matrices, tibbles, etc.
    
    
  }
}
