threshMeasures <- function(model = NULL, obs = NULL, pred = NULL, thresh, measures = modEvAmethods("threshMeasures")[-grep("OddsRatio", modEvAmethods("threshMeasures"))], simplif = FALSE, pbg = FALSE, plot = TRUE, plot.type = "lollipop", ylim = "auto0", plot.ordered = FALSE, standardize = TRUE, verbosity = 2, interval = 0.01, quant = 0, na.rm = TRUE, rm.dup = FALSE, ...) {
  # version 4.0 (19 Feb 2025)
  
  # if (is.null(model)) {
  #   if (is.null(obs) | is.null(pred)) stop ("You must provide either the 'obs' and 'pred' vectors, or a 'model' object.")
  #
  # } else { # end if null model
  #
  #   #if (!all(class(model) %in% c("glm", "lm"))) stop ("'model' must be a model object of class 'glm'")
  #   #if (!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
  #   if (verbosity > 0) {
  #     if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
  #     if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
  #   }
  #   # obs <- model$y
  #   # pred <- model$fitted.values
  #   obspred <- mod2obspred(model)
  #   obs <- obspred[ , "obs"]
  #   pred <- obspred[ , "pred"]
  #
  # }  # end if !null model
  #
  # if (length(obs) != length(pred))  stop ("'obs' and 'pred' must have the same number of values (and in the same order).")
  #
  # dat <- data.frame(obs, pred)
  # n.in <- nrow(dat)
  # dat <- na.omit(dat)
  # n.out <- nrow(dat)
  # if (n.out < n.in)  warning (n.in - n.out, " observation(s) removed due to missing data; ", n.out, " observations actually evaluated.")
  # obs <- dat$obs
  # pred <- dat$pred
  #
  # if (!all(obs %in% c(0, 1)))
  #   stop ("'obs' must consist of binary observations of 0 or 1")
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity, pbg = pbg)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  #if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")
  # if (!(thresh == "preval" | is.numeric(thresh)))
  #   stop("'thresh' must be either 'preval' or a numeric value between 0 and 1")
  # if (thresh == "preval")  thresh <- prevalence(obs)
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm, pbg = pbg)
  
  # next lines moved to new 'confusionMatrix' function:
  # obs0 <- obs == 0
  # obs1 <- obs == 1
  # pred0 <- pred < thresh
  # pred1 <- pred >= thresh
  # a <- sum(obs1 & pred1, na.rm = TRUE)
  # b <- sum(obs0 & pred1, na.rm = TRUE)
  # c <- sum(obs1 & pred0, na.rm = TRUE)
  # d <- sum(obs0 & pred0, na.rm = TRUE)
  
  if (is.finite(thresh)) {
    conf_mat <- confusionMatrix(obs = obs, pred = pred, thresh = thresh, plot = FALSE)
    a <- conf_mat["pred1", "obs1"]
    b <- conf_mat["pred1", "obs0"]
    c <- conf_mat["pred0", "obs1"]
    d <- conf_mat["pred0", "obs0"]
    N <- a + b + c + d
  } else {
    a <- b <- c <- d <- N <- NA
  }
  
  Nmeasures <- length(measures)
  measureValues <- as.vector(rep(NA, Nmeasures), mode = "numeric")
  
  for (i in 1:Nmeasures) {
    if (measures[i] == "AUC") measureValues[i] <- AUC(obs = obs, pred = pred, simplif = TRUE)
    else if (measures[i] %in% modEvAmethods("threshMeasures") && is.finite(thresh)) {
      measureValues[i] <- evaluate(a, b, c, d, measure = measures[i])  # , N
      if (standardize == TRUE && measures[i] %in% c("TSS", "kappa", "ORSS")) {
        measureValues[i] <- standard01(measureValues[i])
        measures[i] <- paste("s", measures[i], sep = "")
        if (verbosity > 0) message("\n", measures[i], " standardized to the 0-1 scale for direct comparability
with other measures (type '?standard01' for more info);
use 'standardize = FALSE' if this is not what you wish")
      }  # end if standardize
    }  # end if measures[i] in modEvAmethods("threshMeasures")
    else {
      warning("'", measures[i], "' is not a valid measure;
type modEvAmethods('threshMeasures') for available options.")
      next
    }  # end else
  }  # end for i
  
  Measures <- matrix(data = measureValues, nrow = Nmeasures, ncol = 1, dimnames = list(measures, "Value"))
  
  if (plot) {
    names(measureValues) <- measures
    measures.plot <- measureValues
    if (plot.ordered) {
      measures.plot <- sort(measures.plot, decreasing = TRUE, na.last = TRUE)
    }
    measures.plot[is.infinite(measures.plot)] <- NA
    
    if (length(grep("auto", ylim)) > 0) {
      ymin <- min(measures.plot, na.rm = TRUE)
      if ("auto0" %in% ylim) ymin <- min(0, ymin)
      ylim <- c(ymin, max(measures.plot))
    }
    
    if (plot.type == "barplot" && any(is.finite(measures.plot)))
      barplot(measures.plot[is.finite(measures.plot)], las = 2, ylim = ylim, ...)
    else if (plot.type == "lollipop" && any(is.finite(measures.plot)))
      lollipop(measures.plot[is.finite(measures.plot)], las = 2, ylim = ylim, xlab = "", ylab = "", ...)
  }  # end if plot
  
  if (simplif) {  # shorter version for use with e.g. optiThresh function
    out <- Measures
    
  } else {
    prev <- prevalence(obs = obs)
    conf.matrix <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("pred1", "pred0"), c("obs1", "obs0")))
    out <- list(N = N, Prevalence = prev, Threshold = thresh, ConfusionMatrix = conf.matrix, ThreshMeasures = Measures)
  }  # end else
  
  return(out)
}
