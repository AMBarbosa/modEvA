AUC <- function(model = NULL, obs = NULL, pred = NULL, simplif = FALSE, interval = "auto", FPR.limits = c(0, 1), curve = "ROC", pbg = FALSE, method = NULL, plot = TRUE, diag = TRUE, diag.col = "lightblue3", diag.lty = 2, curve.col = "darkblue", curve.lty = 1, curve.lwd = 2, plot.values = TRUE, plot.digits = 3, plot.preds = FALSE, grid = FALSE, grid.lty = 1, xlab = "auto", ylab = "auto", cex.lab = 0.9, ticks = FALSE, na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  
  # version 3.4 (4 Mar 2025)
  
  if (all.equal(FPR.limits, c(0, 1)) != TRUE) stop ("Sorry, 'FPR.limits' not yet implemented. Please use default values.")
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity, pbg = pbg)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  # if (any(pred < 0, na.rm = TRUE) || any(pred > 1, na.rm = TRUE)) warning("Some of your predicted values are outside the [0, 1] interval within which thresholds are calculated.")  # moved to after NA removal and simplif check
  
  incalculable <- FALSE
  if (all(obs == 0) || all(obs == 1)) {
    incalculable <- TRUE
    warning("AUC can't be computed if there aren't two response states\n(i.e. both ones and zeros) to compare their predictions.")
  }
  
  stopifnot(
    obs %in% c(0,1),
    #pred >= 0,
    #pred <= 1,
    interval == "auto" || (interval > 0 && interval < 1),
    curve %in% c("ROC", "PR"),
    is.null(method) || (length(method) == 1 && method %in% c("rank", "trapezoid", "integrate"))
  )
  
  if (interval == "auto"){
    # interval <- round(0.01 - (abs(mean(pred) - 0.5) * 0.018), 3)  # makes interval range between 0.001 and 0.01, proportionally to how close mean(pred) is to 0.5
    coverage <- table(cut(pred, breaks = seq(0, 1, by = 1/3)))
    if (all(coverage > 0)) interval <- 0.01
    else interval <- 0.001
  }
  
  if (is.null(method)) {
    method <- ifelse(curve == "ROC", "rank", "trapezoid")
  } else {  # if method isn't NULL
    if (method == "integrate") {
      if (!incalculable) warning("'integrate' method no longer supported; using default 'rank' method for ROC or 'trapezoid' for PR curve.")
      method <- ifelse(curve == "ROC", "rank", "trapezoid")
    }
    if (method == "rank" && curve != "ROC") {
      if (!incalculable) warning("'rank' method not applicable to the specified 'curve'; using 'trapezoid' method instead.")
      method <- "trapezoid"
    }
  }
  
  n1 <- sum(obs == 1)
  n0 <- sum(obs == 0)
  
  if (method == "rank") {
    # next 3 lines from Wintle et al 2005 supp mat "roc" function
    xy <- c(pred[obs == 0], pred[obs == 1])
    rnk <- rank(xy)
    AUC <- ((n0 * n1) + ((n0 * (n0 + 1))/2) - sum(rnk[1 : n0])) / (n0 * n1)
    
    # AUC <- as.numeric(wilcox.test(pred[obs == 0], pred[obs == 1]) $ statistic) / (sum(obs == 0) * sum(obs == 1))
    
    if (simplif && !plot) return(AUC)
  }
  
  if (any(pred < 0, na.rm = TRUE) || any(pred > 1, na.rm = TRUE) && (simplif == FALSE || plot == TRUE)) warning("Some of the 'pred' values are outside the [0, 1] interval within which thresholds are calculated and plotted.")
  
  N <- length(obs)
  preval <- suppressWarnings(prevalence(obs = obs))
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
    
    if (curve == "ROC" && !incalculable) warning ("AUC value will be more accurate if method = 'rank', or if 'interval' is decreased -- see 'interval' and 'method' arguments in ?AUC")
    else if (interval >= 0.01 && !incalculable) warning ("AUC value will be more accurate if 'interval' is decreased -- see 'interval' and 'method' arguments in ?AUC")
    
    xy <- data.frame(xx, yy)
    #xy <- na.omit(data.frame(xx, yy))  # this caused inaccurate AUC-PR values, as per bug report by Tessa Chen
    #if (length(xx) != nrow(xy))  warning(paste(abs(length(xx) - nrow(xy)), "non-finite value(s) omitted from area calculation."))
    xx <- xy$xx
    yy <- xy$yy
    # next line adapted from https://stackoverflow.com/a/22418496:
    AUC <- sum(diff(xx) * (yy[-1] + yy[-length(yy)]) / 2)
    AUC <- -AUC  # euze
    
    if (curve == "PR" && any(is.nan(yy))) {  # added Oct 30 2021 following bug report by Ying-Ju Tessa Chen, which caused wrong area calculation when curve extreme was NaN
      if(!incalculable) warning(paste(sum(is.nan(yy)), "point(s) with NaN precision value (see 'precision' column in the $thresholds section of your returned results if simplif=FALSE); coercing to last non-NaN value to interpolate curve for AUC calculation"))
      last_thresh_with_value <- max(which(!is.nan(yy)))
      yy_noNaN <- yy
      yy_noNaN[is.nan(yy)] <- yy_noNaN[last_thresh_with_value]
      # next line adapted from https://stackoverflow.com/a/22418496:
      AUC <- sum(diff(xx) * (yy_noNaN[-1] + yy_noNaN[-length(yy_noNaN)]) / 2)
      AUC <- -AUC  # euze
    }  # end if NaN precision
  }  # end if trapezoid
  
  # if (method == "integrate") {  # deactivated for producing less accurate values (compared to 'rank' method for 'ROC' curve)
  # xx.interp <- stats::approx(x = thresholds, y = xx, n = length(thresholds))
  # yy.interp <- stats::approx(x = thresholds, y = yy, n = length(thresholds))
  # f <- approxfun(x = xx.interp$y, y = yy.interp$y)
  # AUC <- integrate(f, lower = min(thresholds), upper = max(thresholds))$value
  # }
  
  if (plot) {
    
    par_mgp <- par()$mgp
    on.exit(par(mgp = par_mgp))
    par(mgp = c(2.7, 0.7, 0))
    
    if (curve == "ROC") {
      if (xlab == "auto") xlab <- c("False positive rate", "(1-specificity)")
      if (ylab == "auto") ylab <- c("True positive rate", "(sensitivity)")
    }
    if (curve == "PR") {
      if (xlab == "auto") xlab <- c("Recall", "(sensitivity)")
      if (ylab == "auto") ylab <- c("Precision", "(positive predictive value)")
    }
    
    if (curve == "ROC") diag_vals <- c(0, 1)
    if (curve == "PR") diag_vals <- c(1, 0)
    d <- ifelse(diag, "l", "n")  # to plot the 0.5 diagonal (or not if diag=FALSE)
    
    plot(x = c(0, 1), y = diag_vals, type = d, xlab = "", ylab = "", col = diag.col, lty = diag.lty, ...)
    mtext(xlab, side = 1, line = c(2, 3), cex = cex.lab)
    mtext(ylab, side = 2, line = c(3, 2), cex = cex.lab)
    
    # if (grid) abline(h = thresholds, v = thresholds, lty = grid.lty, col = "lightgrey")
    grid.seq <- seq(0, 1, by = 0.1)
    if (grid) abline(h = grid.seq, v = grid.seq, lty = grid.lty, col = "lightgrey")
    
    if (curve == "PR" && any(is.nan(yy))) lines(x = xx, y = yy_noNaN, col = "coral", lty = 3, lwd = min(1, curve.lwd))  # plots the interpolated (noNaN) curve underneath the curve with actual precision values
    
    # draw the curve:
    lines(x = xx, y = yy, col = curve.col, lty = curve.lty, lwd = curve.lwd)
    
    # if (plot.preds == TRUE) plot.preds <- c("curve", "bottom")  # for back-compatibility
    if ("bottom" %in% plot.preds) {
      points(x = xx, y = rep(0, Nthresh), cex = 20 * sqrt(prop.preds), pch = 21, col = "darkgrey", bg = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))  # cex = 100 * prop.preds
    }
    if ("curve" %in% plot.preds || plot.preds == TRUE) {
      points(x = xx, y = yy, cex = 20 * sqrt(prop.preds), pch = 21, col = "darkgrey", bg = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))  # cex = 100 * prop.preds
    }
    if (ticks == TRUE) axis(1, at = xx, labels = NA, tick = TRUE, tck = 0.03, col = NA, col.ticks = "blue")
    
    if (plot.values) {
      if (curve == "ROC") text(0.5, 0.05, substitute(paste(AUC == a), list(a = round(AUC, plot.digits))))
      #if (curve == "PR") text(1, 1, adj = 1, substitute(paste(expression('AUC'['PR']) == a), list(a = round(AUC, plot.digits))))
      if (curve == "PR") text(0.5, 0.1, substitute(paste('AUC'['PR'] == a), list(a = round(AUC, plot.digits))))
    }  # end if plot.values
    
  }  # end if plot
  
  if (simplif)  return (AUC)
  
  thresholds.df <- data.frame(thresholds, true.positives, true.negatives, sensitivity, specificity, precision, false.pos.rate, n.preds, prop.preds)
  rownames(thresholds.df) <- thresholds
  
  output <- list(thresholds = thresholds.df,
                 N = N,
                 prevalence = preval,
                 AUC = AUC,
                 AUCratio = AUC / 0.5,
                 meanPrecision = precision_mean,
                 GiniCoefficient = 2 * AUC - 1)
  
  # if (ci) {  # https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_the_Area_Under_an_ROC_Curve.pdf
  #   tbl <- table(obs)
  #   n1 <- tbl[1]
  #   n2 <- tbl[2]
  #   q1 <- AUC / (2 - AUC)
  #   q2 <- (2 * AUC ^ 2) / (1 + AUC)
  #   numerator <- AUC * (1 - AUC) + (n1 - 1) * (q1 - AUC ^ 2) + (n2 - 1) * (q2 - AUC ^ 2)
  #   se <- sqrt(numerator / n1 * n2)
  #   upper <- AUC + 1.96 * se
  #   lower <- AUC - 1.96 * se
  #   
  #   output <- list(thresholds = thresholds.df,
  #                  N = N,
  #                  prevalence = preval,
  #                  AUC = AUC,
  #                  AUCratio = AUC / 0.5,
  #                  AUCse = se,
  #                  AUCupper = upper,
  #                  AUClower = lower,
  #                  meanPrecision = precision_mean,
  #                  GiniCoefficient = 2 * AUC - 1)
  # }
  
  return(output)
}
