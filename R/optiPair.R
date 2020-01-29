optiPair <-
function (model = NULL, obs = NULL, pred = NULL,
          measures = c("Sensitivity", "Specificity"), 
          interval = 0.01, plot = TRUE, plot.sum = FALSE, 
          plot.diff = FALSE, ylim = NULL, ...) {
  # version 1.7 (18 Apr 2016)

  if(length(measures) != 2) stop ("'measures' must contain two elements.")

  if(is.null(model)) {
    
    if (is.null(obs) | is.null(pred)) stop ("You must provide either the 'obs' and 'pred' vectors, or a 'model' object of class 'glm'.")
    if (length(obs) != length(pred))  stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    
    dat <- data.frame(obs, pred)
    n.in <- nrow(dat)
    dat <- na.omit(dat)
    n.out <- nrow(dat)
    if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
    obs <- dat$obs
    pred <- dat$pred
    
  } else { # end if null model
  
    if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    model <- NULL  # so the message is not repeated for each threshold
  }  # end if model

  if (NA %in% obs | NA %in% pred) stop("Please remove (rows with) NA from your data")
  if (length(obs) != length(pred)) stop("'obs' and 'pred' must have the same number of values (and in the same order)")
  if (!all(obs %in% c(0, 1))) stop ("'obs' must consist of binary observations of 0 or 1")
  if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")

  measures.values <- optiThresh(obs = obs, pred = pred, interval = interval, measures = measures, optimize = "each", simplif = TRUE)
  measures.values$Difference <- abs(measures.values[, 1] - measures.values[, 2])
  measures.values$Sum <- rowSums(measures.values[ , 1:2])
  measures.values$Mean <- rowMeans(measures.values[ , 1:2])
  measures.values$Threshold <- as.numeric(rownames(measures.values))

  MinDiff <- min(measures.values$Difference)
  MaxSum <- max(measures.values$Sum)
  MaxMean <- max(measures.values$Mean)

  ThreshDiff <- with(measures.values, Threshold[which.min(Difference)])
  ThreshSum <- with(measures.values, Threshold[which.max(Sum)])
  ThreshMean <- with(measures.values, Threshold[which.max(Mean)])

  if (plot) {

    finite <- as.matrix(measures.values[ , 1:2])
    finite <- finite[is.finite(finite)]
    if(is.null(ylim)) {
      if(plot.sum) ylim <- c(min(finite), MaxSum)
      else ylim <- c(min(finite), max(finite))
    }  # end if null ylim

    plot(measures.values[ , 1] ~ measures.values$Threshold, pch = 19, xlab = "", ylab = "", ylim = ylim, ...)
    mtext(side = 1, text = "Threshold", line = 2, col = "black")
    mtext(side = 2, text = measures[1], line = 3, col = "black")
    mtext(side = 2, text = measures[2], line = 2, col = "darkgrey")
    points(measures.values[ , 2] ~ measures.values$Threshold, pch = 20, col = "darkgrey")
    abline(h = measures.values[which.min(measures.values$Difference), 1], col = "lightgrey", lty = 2)
    abline(v = ThreshDiff, col = "lightgrey", lty = 2)

    if (plot.sum) {
      with(measures.values, points(Sum ~ Threshold, pch = '+'))
      abline(v = ThreshSum, col = "lightgrey", lty = 2)
      abline(h = MaxSum, col = "lightgrey", lty = 2)
    }    # end if plot sum

    if (plot.diff) {
      with(measures.values, points(Difference ~ Threshold, pch = '-', col = "grey"))
      abline(v = ThreshDiff, col = "grey", lty = 2)
      abline(h = MinDiff, col = "grey", lty = 2)
    }  # end if plot diff
  }  # end if plot

  return(list(measures.values = measures.values, MinDiff = MinDiff, ThreshDiff = ThreshDiff, MaxSum = MaxSum, ThreshSum = ThreshSum, MaxMean = MaxMean, ThreshMean = ThreshMean))

}
