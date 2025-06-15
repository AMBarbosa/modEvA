HLfit <- function (model = NULL, obs = NULL, pred = NULL, bin.method, n.bins = 10, fixed.bin.size = FALSE, min.bin.size = 15, min.prob.interval = 0.1, quantile.type = 7, simplif = FALSE, verbosity = 2, alpha = 0.05, plot = TRUE, plot.values = TRUE, plot.bin.size = TRUE, xlab = "Predicted probability", ylab = "Observed prevalence", na.rm = TRUE, rm.dup = FALSE, ...) {
  # version 2.1 (6 May 2022)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0, 1)#,
    #pred >= 0,
    #pred <= 1
  )
  # new:
  if (any(pred < 0) | any(pred > 1)) {
    warning("Some of your 'pred' values are outside the [0,1] interval; are you sure these represent probabilities? Unexpected or incorrect results may arise. Consider properly rescaling you 'pred' values, or obtaining real probabilities instead.")
  }
  
  bins <- getBins(obs = obs, pred = pred, bin.method = bin.method, n.bins = n.bins, fixed.bin.size = fixed.bin.size, min.bin.size = min.bin.size, min.prob.interval = min.prob.interval, verbosity = verbosity)
  n.bins <- nrow(bins$bins.table)

  # next 4 lines: adapted from hosmerlem function in http://www.stat.sc.edu/~hitchcock/diseaseoutbreakRexample704.txt
  observed <- xtabs(cbind(1 - obs, obs) ~ bins$prob.bin)
  expected <- xtabs(cbind(1 - pred, pred) ~ bins$prob.bin)
  chi.sq <- sum((observed - expected) ^ 2 / expected)
  p.value <- 1 - pchisq(chi.sq, df = n.bins - 2)
  rmse <- sqrt(mean((observed - expected) ^ 2))

  if (simplif) return (list(chi.sq = chi.sq, p.value = p.value, RMSE = rmse))

  # plotting loosely based on calibration.plot function in package PresenceAbsence
  if (plot) {
    N.total <- tapply(obs, bins$prob.bin, length)  # N cases in each bin
    N.presence <- tapply(obs, bins$prob.bin, sum)  # N presences in each bin
    Empty <- is.na(N.total)
    N.total[is.na(N.total)] <- 0
    N.presence[is.na(N.presence)] <- 0
    OBS.proportion <- N.presence / N.total
    OBS.proportion[Empty] <- NA
    df1.low <- 2 * (N.total - N.presence + 1)
    df2.low <- 2 * N.presence
    df1.up <- 2 * (N.presence + 1)
    df2.up <- 2 * (N.total - N.presence)
    N.bins <- length(unique(bins$prob.bin))  # fui eue
    Lower <- rep(0, N.bins)
    Upper <- rep(1, N.bins)
    TF <- N.presence != 0
    Lower[TF] <- N.presence[TF]/(N.presence[TF] + ((N.total[TF] - N.presence[TF] + 1) * qf(1 - alpha/2, df1.low[TF], df2.low[TF])))
    Lower[Empty] <- NA
    TF <- N.presence < N.total
    Upper[TF] <- ((N.presence[TF] + 1) * qf(1 - alpha/2, df1.up[TF], df2.up[TF]))/(N.total[TF] - N.presence[TF] + ((N.presence[TF] + 1) * qf(1 - alpha/2, df1.up[TF], df2.up[TF])))
    Upper[Empty] <- NA

    xymin <- min(-0.05, min(pred))
    xymax <- max(1.05, max(pred))
    plot(c(xymin, xymax), c(xymin, xymax), type = "n", xlab = xlab, ylab = ylab, ...)
    # the above is needed for some bin sizes when plotted as text
    bin.centers <- bins$bins.table$median.prob  # fui eue

    if (plot.bin.size) {
      #mtext("Proportional bin size", side = 4, line = 1, col = "darkgrey")
      #abline(h = min.bin.size/length(pred), col = "red")
      #barplot(height = N.total/length(pred), xlim = c(0, 1), width = 0.05, border = NA, add = TRUE, beside = TRUE)  # still need to place bars at bin centers
      text(bin.centers, Upper + 0.07, labels = N.total)
    }
    
    abline(a = 0, b = 1, lty = 2)
    for (i in 1:N.bins) {
      lines(rep(bin.centers[i], 2), c(Lower[i], Upper[i]))
    }
    points(bin.centers, OBS.proportion, pch = 20)

    if (plot.values) {
      text(1, 0.2, adj = 1, substitute(paste(HL == a), list(a = round(chi.sq, 1))))
      if (p.value < 0.001) {
        text(1, 0.1, adj = 1, substitute(paste(italic(p) < a), list(a = 0.001)))
      } else {
        text(1, 0.1, adj = 1, substitute(paste(italic(p) == a), list(a = round(p.value, 3))))
      }
      text(1, 0.0, adj = 1, substitute(paste(RMSE == a), list(a = round(rmse, 1))))
    }  # end if plot values
  }

  BinPred <- tapply(pred, bins$prob.bin, mean)
  bins.table <- data.frame(BinCenter = bin.centers, NBin = N.total, BinObs = OBS.proportion, BinPred = BinPred, BinObsCIlower = Lower, BinObsCIupper = Upper)

  return(list(bins.table = bins.table, chi.sq = chi.sq, DF = n.bins - 2, p.value = p.value, RMSE = rmse))
}
