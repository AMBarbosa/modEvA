similarity <- function(model = NULL, obs = NULL, pred = NULL, thresh, measures = modEvAmethods("similarity"), simplif = FALSE, pbg = FALSE, plot = TRUE, plot.type = "lollipop", plot.ordered = FALSE, verbosity = 2, interval = 0.01, quant = 0, na.rm = TRUE, rm.dup = FALSE, ...) {
  # version 1.3 (24 Nov 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, pbg = pbg, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  #if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")
  # if (!(thresh == "preval" | is.numeric(thresh)))
  #   stop("'thresh' must be either 'preval' or a numeric value between 0 and 1")
  # if (thresh == "preval")  thresh <- prevalence(obs)
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm, pbg = pbg)

  if (is.finite(thresh)) {
    pred01 <- pred
    pred01[pred < thresh] <- 0
    pred01[pred >= thresh] <- 1
    N <- length(pred)
  }

  Nmeasures <- length(measures)
  measureValues <- as.vector(rep(NA, Nmeasures), mode = "numeric")
  names(measureValues) <- measures

  A <- sum(obs, na.rm = na.rm)
  B <- sum(pred01, na.rm = na.rm)
  C <- sum(pmin(obs, pred01, na.rm = na.rm), na.rm = na.rm)  # intersection

  for (m in measures) {
    if (m %in% modEvAmethods("similarity") && is.finite(thresh)) {
      if (m == "Jaccard") measureValues[m] <- C / (A + B - C)
      if (m == "Sorensen") measureValues[m] <- 2 * C / (A + B)
    }  # end if m in modEvAmethods("similarity")
    else {
      warning("'", m, "' is not a valid measure;
type modEvAmethods('similarity') for available options.")
      next
    }  # end else
  }  # end for m

  Measures <- matrix(data = measureValues, nrow = Nmeasures, ncol = 1, dimnames = list(measures, "Value"))
  if (simplif) {  # shorter version for use with e.g. optiThresh function
    out <- Measures
  } else {
    out <- list(N = N, Threshold = thresh,
                similarity = Measures)
  }  # end else
  if (plot) {
    measures.plot <- measureValues
    if (plot.ordered) {
      measures.plot <- sort(measures.plot, decreasing = TRUE, na.last = TRUE)
    }
    measures.plot[is.infinite(measures.plot)] <- NA
    if (plot.type == "barplot" && any(is.finite(measures.plot))) 
      barplot(measures.plot[is.finite(measures.plot)], las = 2, ...)
    if (plot.type == "lollipop" && any(is.finite(measures.plot))) 
      lollipop(measures.plot[is.finite(measures.plot)], las = 2, xlab = "", ylab = "", ...)
  }  # end if plot
  return(out)
}
