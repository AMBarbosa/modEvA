Miller <- function(model = NULL, obs = NULL, pred = NULL, plot = TRUE, line.col = "steelblue4", diag = TRUE, diag.col = "lightblue3", plot.values = TRUE, digits = 2, values.col = c("grey40", "#8B4C4C", "black"), xlab = "", ylab = "", main = "Miller calibration", na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 3.0 (18 May 2026)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  stopifnot(
    obs %in% c(0, 1)
  )

  if (any(pred < 0) | any(pred > 1)) warning("Some of your predicted values are outside the [0, 1] interval, while values should represent probabilities")

  pred[pred == 0] <- 2e-16  # avoid NaN in log below
  pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below

  logit <- log(pred / (1 - pred))
  mod <- suppressWarnings(glm(obs ~ logit, family = binomial))
  intercept <- mod$coef[[1]]
  slope <- mod$coef[[2]]
  slopeDiff <-  abs(slope - 1)
  #std.err <- summary(mod)$coefficients["logit", "Std. Error"]
  #slope.p <- abs((slope - 1) / sqrt(std.err^2 + 0))  # Paternoster 98; http://stats.stackexchange.com/questions/55501/test-a-significant-difference-between-two-slope-values
  #slope.t <- (slope - 1) / std.err
  #slope.p <- pt(slope.t, df = mod$df.residual)  # http://stats.stackexchange.com/questions/111559/test-model-coefficient-regression-slope-against-some-value
  # both values look wrong...
  
  #  f(x) = x/(x+1) if x=>0; f(x) = x/(x-1) if x<=0  # Carlos Ramos
  MCA <- 1 - (slopeDiff / (slopeDiff + 1))  # it's absolute slope diff

  if (plot) {
    ymin <- min(0, intercept)
    ymax <- max(1, intercept + 0.3)
    plot(c(0, 1), c(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main, ...)
    if (diag) abline(0, 1, lty = 2, col = diag.col)
    abline(intercept, slope, lwd = 2, col = line.col)
    if (plot.values) {
      if (length(values.col) == 1)  values.col <- rep(values.col, 3)
      text(x = 1, y = ymin + 0.25 * (ymax - ymin), adj = 1, labels = paste0("slope = " , round(slope, digits)), col = values.col[1], cex = 0.9)
      text(x = 1, y = ymin + 0.135 * (ymax - ymin), adj = 1, labels = paste0("slopeDiff = ", round(slopeDiff, digits)), col = values.col[2], cex = 0.9)
      text(x = 1, y = ymin + 0.02 * (ymax - ymin), adj = 1, labels = paste0("MCA = ", format(round(MCA, digits = 3), nsmall = 3)), col = values.col[3])
    }  # end if plot.values
  }  # end if plot

  # return(list(intercept = intercept, slope = slope, slope.pvalue = slope.p))
  list(intercept = intercept, slope = slope, slopeDiff = slopeDiff, MCA = MCA)
}
