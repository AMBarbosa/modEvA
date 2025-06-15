optiPair <- function (model = NULL, obs = NULL, pred = NULL, measures = c("Sensitivity", "Specificity"), interval = 0.01, pbg = FALSE, plot = TRUE, plot.sum = FALSE, plot.diff = FALSE, col1 = "darkblue", col2 = "lightblue3", ylim = NULL, na.rm = TRUE, exclude.zeros = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 2.4 (26 Dec 2024)

  if (length(measures) != 2) stop ("'measures' must contain two elements.")

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, pbg = pbg, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  # if (!is.null(model)) {
  #   model <- NULL  # so the message is not repeated for each threshold
  # }  # end if model

  if (!all(obs %in% c(0, 1))) stop ("'obs' must consist of binary observations of 0 or 1")
  if (any(pred < 0 | pred > 1)) stop ("'pred' must range between 0 and 1")

  measures.values <- optiThresh(obs = obs, pred = pred, interval = interval, measures = measures, optimize = "each", simplif = TRUE)

  measures.values$Difference <- abs(measures.values[, 1] - measures.values[, 2])
  measures.values$Sum <- rowSums(measures.values[ , 1:2])
  measures.values$Mean <- rowMeans(measures.values[ , 1:2])
  measures.values$Threshold <- as.numeric(rownames(measures.values))

  measures.values.trimmed <- measures.values
  if (exclude.zeros)  measures.values.trimmed <- measures.values.trimmed[is.finite(rowSums(measures.values.trimmed[ , 1:2])) & rowSums(measures.values.trimmed[ , 1:2] > 0), ]

  MinDiff <- min(measures.values.trimmed$Difference, na.rm = na.rm)
  MaxSum <- max(measures.values.trimmed$Sum, na.rm = na.rm)
  MaxMean <- max(measures.values.trimmed$Mean, na.rm = na.rm)

  ThreshDiff <- with(measures.values.trimmed, Threshold[which.min(Difference)])
  ThreshSum <- with(measures.values.trimmed, Threshold[which.max(Sum)])
  ThreshMean <- with(measures.values.trimmed, Threshold[which.max(Mean)])

  if (plot) {
    par_mgp <- par()$mgp
    on.exit(par(mgp = par_mgp))
    par(mgp = c(3, 0.7, 0))

    finite <- as.matrix(measures.values[ , 1:2])
    finite <- finite[is.finite(finite)]
    if (is.null(ylim)) {
      if (plot.sum) ylim <- c(min(finite), MaxSum)
      else ylim <- c(min(finite), max(finite))
    }  # end if null ylim

    plot(measures.values[ , 1] ~ measures.values$Threshold, pch = 19, xlab = "", ylab = "", ylim = ylim, col = col1, ...)
    mtext(side = 1, text = "Threshold", line = 2)
    mtext(side = 2, text = measures[1], line = 3, col = col1)
    mtext(side = 2, text = measures[2], line = 2, col = col2)
    points(measures.values[ , 2] ~ measures.values$Threshold, pch = 20, col = col2)
    abline(h = measures.values[which.min(measures.values.trimmed$Difference), 1], col = "lightgrey", lty = 2)
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
