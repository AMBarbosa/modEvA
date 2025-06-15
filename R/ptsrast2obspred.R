ptsrast2obspred <- function(pts, rst, rm.dup = FALSE, na.rm = FALSE, verbosity = 2) {
  # version 1.4 (2 Nov 2022)

  error_message <- "'pts' must be either a 'SpatVector' of the presence points, or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively and in this order."

  if (!(inherits(pts, "SpatVector") || inherits(pts, "data.frame") || inherits(pts, "matrix"))) stop (error_message)
  if ((inherits(pts, "data.frame") || inherits(pts, "matrix")) && ncol(pts) != 2) stop (error_message)

  if (!inherits(rst, "SpatRaster")) stop("'rst' must be of class SpatRaster. You can try converting it with terra::rast()")
  if (terra::nlyr(rst) > 1) stop("currently, 'rst' must have only one layer.")

  if (inherits(pts, "data.frame")) pts <- as.matrix(pts)  # as per 'terra::rasterize' input requirements below

  obs_rst <- terra::rasterize(pts, rst, fun = sum)
  obs_rst[is.na(obs_rst)] <- 0L

  obs <- terra::values(obs_rst, mat = FALSE)
  pred <- terra::values(rst, mat = FALSE)

  out <- data.frame(obs = obs, pred = pred)

  # keep absences only in value pixels (i.e. remove absences outside value pixels):
  out <- out[obs > 0 | is.finite(pred), ]

  if (na.rm) {  # remove also presences outside value pixels:
    out <- out[which(is.finite(out$pred)), ]
  }

  if (rm.dup) {
    out$obs[out$obs > 1] <- 1L
  } else {
    nrow_orig <- nrow(out)
    repeats <- out$obs
    repeats[repeats == 0] <- 1L
    out <- out[rep(seq_len(nrow(out)), repeats), ]
    out$obs[out$obs > 1] <- 1L
    nrow_final <- nrow(out)
    n_repeats <- nrow_final - nrow_orig
  }

  if (verbosity > 0) {
    cat(paste("Data include", sum(out$obs, na.rm = TRUE), "presences and", sum(out$obs == 0, na.rm = TRUE), "absences.\n"))
  }

  if (!rm.dup && n_repeats > 0) warning("Data include ", n_repeats, " duplicate(s), i.e. repeated presence points in already occupied pixels; use 'rm.dup=TRUE' (or 'rm.dup.points=TRUE' for 'Boyce' function) if you want them removed.")

  return(out)
}
