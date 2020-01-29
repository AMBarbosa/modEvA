threshMeasures <-
function(model = NULL, obs = NULL, pred = NULL, thresh,
         measures = modEvAmethods("threshMeasures"), 
         simplif = FALSE, plot = TRUE, plot.ordered = FALSE, 
         standardize = TRUE, verbosity = 2, ...) {
  # version 2.8 (16 Jan 2020)

  if (is.null(model)) {
    if (is.null(obs) | is.null(pred)) stop ("You must provide either the 'obs' and 'pred' vectors, or a 'model' object of class 'glm'.")

    } else { # end if null model
  
    #if (!all(class(model) %in% c("glm", "lm"))) stop ("'model' must be a model object of class 'glm'")
    if (!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (verbosity > 0) {
      if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
      if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    }
    obs <- model$y
    pred <- model$fitted.values
  }  # end if !null model

  if (length(obs) != length(pred))  stop ("'obs' and 'pred' must have the same number of values (and in the same order).")

  dat <- data.frame(obs, pred)
  n.in <- nrow(dat)
  dat <- na.omit(dat)
  n.out <- nrow(dat)
  if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
  obs <- dat$obs
  pred <- dat$pred
  
  if (!all(obs %in% c(0, 1)))
    stop ("'obs' must consist of binary observations of 0 or 1")
  #if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")
  if (!(thresh == "preval" | is.numeric(thresh)))
    stop("'thresh' must be either 'preval' or a numeric value between 0 and 1")
  if (thresh == "preval")  thresh <- prevalence(obs)

  obs0 <- obs == 0
  obs1 <- obs == 1
  pred0 <- pred < thresh
  pred1 <- pred >= thresh
  a <- sum(obs1 & pred1)
  b <- sum(obs0 & pred1)
  c <- sum(obs1 & pred0)
  d <- sum(obs0 & pred0)
  N <- a + b + c + d

  Nmeasures <- length(measures)
  measureValues <- as.vector(rep(NA, Nmeasures), mode = "numeric")

  for (i in 1:Nmeasures) {
    if (measures[i] == "AUC") measureValues[i] <- AUC(obs = obs, pred = pred, simplif = TRUE)
    else if (measures[i] %in% modEvAmethods("threshMeasures")) {
      measureValues[i] <- evaluate(a, b, c, d, N, measure = measures[i])
      if (standardize == TRUE  &  measures[i] %in% c("TSS", "kappa")) {
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
  if (simplif) {  # shorter version for use with e.g. optiThresh function
    return(Measures)
  } else {
    prev <- prevalence(obs)
    conf.matrix <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("pred1", "pred0"), c("obs1", "obs0")))
    if (plot) {
      names(measureValues) <- measures
      measures.plot <- measureValues
      if (plot.ordered) {
        measures.plot <- sort(measures.plot, decreasing = TRUE)
      }
      barplot(measures.plot, las = 3, ...)
    }  # end if plot
    return(list(N = N, Prevalence = prev, Threshold = thresh,
                ConfusionMatrix = conf.matrix, ThreshMeasures = Measures))
  }  # end else
}
