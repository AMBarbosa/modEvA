applyThreshold <- function(model = NULL, obs = NULL, pred = NULL, thresh, pbg = FALSE, right = FALSE, interval = 0.01, quant = 0, na.rm = TRUE, verbosity = 2) {

  # version 1.4 (28 Oct 2024)

  if(!(length(thresh) %in% 1:2)) stop ("'thresh' must be of length 1 or 2.")
  if (!(is.numeric(thresh) || all(thresh %in% modEvAmethods("getThreshold"))))
    stop("'thresh' must be EITHER numeric OR among the options obtained with modEvAmethods('getThreshold')")

  pred_in <- pred  # in case input is raster, so final reclass is also raster

  obspred <- inputMunch(model, obs, pred, verbosity = verbosity, pbg = pbg)
  if (!is.null(obs) || !is.null(model)) obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  if (any(thresh %in% modEvAmethods("getThreshold"))) {
    for (i in which(thresh %in% modEvAmethods("getThreshold"))) {
      thresh[i] <- getThreshold(obs = obs, pred = pred, threshMethod = thresh[i], interval = interval, quant = quant, na.rm = na.rm, pbg = pbg)
    }
  }

  thresh <- sort(as.numeric(thresh))

  if (inherits(pred_in, "SpatRaster")) reclass <- pred_in
  else reclass <- pred

  reclass[reclass < thresh[1]] <- 0
  reclass[reclass > thresh[length(thresh)]] <- 1
  if (length(thresh) == 2)  reclass[reclass > thresh[1] & reclass < thresh[2]] <- 0.5

  if (right) {
    reclass[reclass == thresh[1]] <- 0
    if (length(thresh) == 2)  reclass[reclass == thresh[2]] <- 0.5
  } else {
    reclass[reclass == thresh[length(thresh)]] <- 1
    if (length(thresh) == 2)  reclass[reclass == thresh[2]] <- 0.5
  }

  return(reclass)
}
