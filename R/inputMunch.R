inputMunch <- function(model = NULL, obs = NULL, pred = NULL, rm.dup = FALSE, na.rm = FALSE, pbg = FALSE, verbosity = 2) {

  # version 1.3 (28 Oct 2028)

  if (!is.null(model)) {
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obspred <- mod2obspred(model)
    obs <- obspred$obs
    pred <- obspred$pred

  }  else {  # end if model

    if (inherits(obs, "data.frame") || inherits(obs, "matrix")) {
      if (!inherits(pred, "SpatRaster")) stop ("When 'obs' is a matrix or dataframe (in which case it should contain the x and y presence point coordinates), 'pred' must be of class 'SpatRaster'.")
    }

    if (inherits(pred, "SpatRaster")) {
      error_message <- "When 'pred' is a SpatRaster, 'obs' must be a matrix or data frame with two columns containing, respectively and in this order, the x (longitude) and y (latitude) coordinates of the presence points."
      if (!(is.null(obs) || inherits(obs, "data.frame") || inherits(obs, "matrix"))) stop(error_message)
      if ((inherits(obs, "data.frame") || inherits(obs, "matrix")) && ncol(obs) != 2) stop(error_message)
      if (is.null(obs)) {
        obspred <- data.frame(pred = terra::values(pred))
        names(obspred) <- "pred"  # was naming column "lyr1" otherwise, error downstream
      }
      else obspred <- ptsrast2obspred(pts = obs, rst = pred, rm.dup = rm.dup, verbosity = verbosity)

    } else {  # end if SpatRaster

      #if (!is.null(obs) && length(obs) != length(pred))  stop ("When they are provided as numeric vectors, 'obs' and 'pred' must be of the same length (and in the same order).")
      if (is.null(obs)) obspred <- data.frame(pred = pred)
      else obspred <- data.frame(obs = obs, pred = pred)
    }  # end if vectors
  }  # end of !model

  if (na.rm) {
    dat <- obspred
    n.in <- nrow(dat)
    dat <- na.omit(dat)
    n.out <- nrow(dat)
    if (n.out < n.in)  warning (n.in - n.out, " observation(s) removed due to missing data; ", n.out, " observations actually evaluated.")

    if (is.null(obs)) obspred <- data.frame(pred = dat$pred)
    else obspred <- data.frame(obs = dat$obs, pred = dat$pred)
  }

  if (isTRUE(pbg)) {  # presence/background
    nrow_in <- nrow(obspred)
    obspred <- rbind(obspred, obspred[obspred$obs == 1, ])
    obspred[(nrow_in + 1):nrow(obspred), "obs"] <- 0
  }

  return(obspred)
}
