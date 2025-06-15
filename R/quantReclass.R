quantReclass <- function(pred, # numeric vector, RasterLayer or SpatRaster
                         by = 0.01, # percentiles by default
                         na.rm = TRUE) {

  # code created in Formoso-Freire et al.
  # version 1.0 (26 Feb 2023)

  # if (min(pred, na.rm = TRUE) < 0 || max(pred, na.rm = TRUE) > 1) stop ("'pred' should be between 0 and 1.")

  reclass <- function(x, reclass_matrix) {
    result <- rep(NA_real_, length(x))
    for (i in 1:length(x)) {
      rcl_row <- which(x[i] >= reclass_matrix[ , 1] & x[i] < reclass_matrix[ , 2])
      if (length(rcl_row) == 0) result[i] <- reclass_matrix[nrow(reclass_matrix), 3]
      else result[i] <- reclass_matrix[rcl_row, 3]
    }
    return(result)
  }

  probs <- seq(0, 1, by = by)
  to <- quantile(pred[pred > 0], probs = probs, na.rm = na.rm)
  from <- c(0, to[-length(to)])
  reclass_matrix <- as.matrix(cbind(from, to, probs))

  if (is(pred, "SpatRaster")) result <- terra::classify(pred, reclass_matrix)
  # else if (is(pred, "RasterLayer")) result <- raster::reclassify(pred, reclass_matrix)  # implied declaring more imports; unnecessary
  else if (is(pred, "numeric")) result <- reclass(pred, reclass_matrix)

  return(result)
}
