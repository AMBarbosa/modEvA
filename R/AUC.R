AUC <- function(model = NULL, obs = NULL, pred = NULL, simplif = FALSE, interval = 0.01, FPR.limits = c(0, 1), curve = "ROC",  method = "rank", plot = TRUE, diag = TRUE, diag.col = "grey", diag.lty = 1, curve.col = "black", curve.lty = 1, curve.lwd = 2, plot.values = TRUE, plot.digits = 3, plot.preds = FALSE, grid = FALSE, xlab = "auto", ylab = "auto", ...) {
  # version 2.2 (17 Jan 2020)
  
  if (all.equal(FPR.limits, c(0, 1)) != TRUE) stop ("Sorry, 'FPR.limits' not yet implemented. Please use default values.")
  
  if (length(obs) != length(pred))  stop ("'obs' and 'pred' must be of the same length (and in the same order).")
  
  if (!is.null(model)) {
    if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
  }  # end if model
  
  dat <- data.frame(obs, pred)
  n.in <- nrow(dat)
  dat <- na.omit(dat)
  n.out <- nrow(dat)
  if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
  obs <- dat$obs
  pred <- dat$pred
  
  stopifnot(
    obs %in% c(0,1),
    #pred >= 0,
    #pred <= 1,
    interval > 0,
    interval < 1,
    curve %in% c("ROC", "PR"),
    method %in% c("rank", "trapezoid", "integrate")
  )
  
  n1 <- sum(obs == 1)
  n0 <- sum(obs == 0)
  
  if (curve != "ROC" && method == "rank") {
    method <- "trapezoid"
    #message("'rank' method not applicable to the specified 'curve'; using 'trapezoid' instead.")
  }
  
  if (method == "rank") {
    # next 3 lines from Wintle et al 2005 supp mat "roc" function
    xy <- c(pred[obs == 0], pred[obs == 1])
    rnk <- rank(xy)
    AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
    if (simplif && !plot) return(AUC)
  }

  if (any(pred < 0) | any(pred > 1)) warning("Some of your predicted values are outside the [0, 1] interval within which thresholds are calculated.")
  
  N <- length(obs)
  preval <- prevalence(obs)
  thresholds <- seq(0, 1, by = interval)
  Nthresh <- length(thresholds)
  true.positives <- true.negatives <- sensitivity <- specificity <- precision <- false.pos.rate <- n.preds <- prop.preds <- numeric(Nthresh)
  
  for (t in 1 : Nthresh) {
    true.positives[t] <- sum(obs == 1 & pred >= thresholds[t])
    true.negatives[t] <- sum(obs == 0 & pred < thresholds[t])
    sensitivity[t] <- true.positives[t] / n1
    specificity[t] <- true.negatives[t] / n0
    #pred.positives[t] <- sum(pred >= thresholds[t])
    precision[t] <- true.positives[t] / sum(pred >= thresholds[t])
    #if (true.positives[t] == 0 && sum(pred >= thresholds[t], na.rm = TRUE) == 0)  precision[t] <- 0  # to avoid NaN?
    false.pos.rate[t] <- 1 - specificity[t]
    n.preds[t] <- sum(round(pred, nchar(Nthresh) - 1) == thresholds[t])
    prop.preds[t] <- n.preds[t] / length(pred)
  }
  
  precision_mean <- mean(precision, na.rm = TRUE)
    
  if (curve == "ROC") {
    xx <- false.pos.rate
    yy <- sensitivity
  } else {
    if (curve == "PR") {
      xx <- sensitivity
      yy <- precision
    } else {
      stop ("'curve' must be either 'ROC' or 'PR'.")
    }
  }
  
  if (method == "trapezoid") {
    xy <- na.omit(data.frame(xx, yy))
    #if (length(xx) != nrow(xy)) warning("Some non-finite values omitted from area calculation.")
    xx <- xy$xx
    yy <- xy$yy
    # next line adapted from https://stackoverflow.com/a/22418496:
    AUC <- sum(diff(xx) * (yy[-1] + yy[-length(yy)]) / 2)
    AUC <- -AUC  # euze
  }
  
    if (method == "integrate") {
    xx.interp <- stats::approx(x = thresholds, y = xx, n = length(thresholds))
    yy.interp <- stats::approx(x = thresholds, y = yy, n = length(thresholds))
    f <- approxfun(x = xx.interp$y, y = yy.interp$y)
    AUC <- integrate(f, lower = min(thresholds), upper = max(thresholds))$value
    }
  
  if (plot) {
    if (curve == "ROC") {
      if (xlab == "auto") xlab <- c("False positive rate", "(1-specificity)")
      if (ylab == "auto") ylab <- c("True positive rate", "(sensitivity)")
    }
    if (curve == "PR") {
      if (xlab == "auto") xlab <- c("Recall", "(sensitivity)")
      if (ylab == "auto") ylab <- c("Precision", "(positive predictive value)")
    }
    
    d <- ifelse(diag, "l", "n")  # to plot the 0.5 diagonal (or not if diag=FALSE)
    if (curve == "ROC") plot(x = c(0, 1), y = c(0, 1), type = d, xlab = xlab, ylab = ylab, col = diag.col, lty = diag.lty, ...)
    if (curve == "PR") plot(x = c(0, 1), y = c(1, 0), type = d, xlab = xlab, ylab = ylab, col = diag.col, lty = diag.lty, ...)
    
    if (grid) abline(h = thresholds, v = thresholds, col = "lightgrey")
    
    lines(x = xx, y = yy, col = curve.col, lty = curve.lty, lwd = curve.lwd)
    
    if (plot.preds == TRUE) plot.preds <- c("curve", "bottom")  # for back-compatibility
    if ("bottom" %in% plot.preds) {
      points(x = thresholds, y = rep(0, Nthresh), cex = 100 * prop.preds, col = "darkgrey")  # 20 * sqrt(prop.preds)
    }
    if ("curve" %in% plot.preds) {
      points(x = xx, y = yy, cex = 100 * prop.preds, col = "darkgrey")
    }
    
    if (plot.values) {
      if (curve == "ROC") text(1, 0.4, adj = 1, substitute(paste(AUC == a), list(a = round(AUC, plot.digits))))
      #if (curve == "PR") text(1, 1, adj = 1, substitute(paste(expression('AUC'['PR']) == a), list(a = round(AUC, plot.digits))))
      if (curve == "PR") text(1, 1, adj = 1, substitute(paste('AUC'['PR'] == a), list(a = round(AUC, plot.digits))))
    }  # end if plot.values
    
  }  # end if plot
  
  if (simplif)  return (AUC)
  
  thresholds.df <- data.frame(thresholds, true.positives, true.negatives, sensitivity, specificity, precision, false.pos.rate, n.preds, prop.preds)
  rownames(thresholds.df) <- thresholds
  
  return (list(thresholds = thresholds.df, N = N, prevalence = preval, AUC = AUC, AUCratio = AUC / 0.5, meanPrecision = precision_mean))
}
