MillerCalib <- function(model = NULL, obs = NULL, pred = NULL, plot = TRUE, line.col = "darkblue", diag = TRUE, diag.col = "lightblue3", plot.values = TRUE, digits = 2, xlab = "", ylab = "", main = "Miller calibration", na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 2.1 (3 Jul 2025)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  stopifnot(
    obs %in% c(0, 1)#,
    #pred >= 0,
    #pred <= 1
  )

  if (any(pred < 0) | any(pred > 1)) warning("Some of your predicted values are outside the [0, 1] interval; are you sure these represent probabilities?")

  pred[pred == 0] <- 2e-16  # avoid NaN in log below
  pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below

  logit <- log(pred / (1 - pred))
  mod <- glm(obs ~ logit, family = binomial)
  intercept <- mod$coef[[1]]
  slope <- mod$coef[[2]]
  slopeDiff <-  abs(slope - 1)
  #std.err <- summary(mod)$coefficients["logit", "Std. Error"]
  #slope.p <- abs((slope - 1) / sqrt(std.err^2 + 0))  # Paternoster 98; http://stats.stackexchange.com/questions/55501/test-a-significant-difference-between-two-slope-values
  #slope.t <- (slope - 1) / std.err
  #slope.p <- pt(slope.t, df = mod$df.residual)  # http://stats.stackexchange.com/questions/111559/test-model-coefficient-regression-slope-against-some-value
  # both values look wrong...

  if (plot) {
    ymin <- min(0, intercept)
    ymax <- max(1, intercept + 0.3)
    plot(c(0, 1), c(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main, ...)
    if (diag) abline(0, 1, lty = 2, col = diag.col)
    abline(intercept, slope, lwd = 2, col = line.col)
    if (plot.values) {
      # plotext <- paste0("slope = " , round(slope, digits), "\n(intercept = ", round(intercept, digits), ")")
      # text(x = 1, y = ymin + 0.15 * (ymax - ymin), adj = 1, labels = plotext)
      text(x = 1, y = ymin + 0.175 * (ymax - ymin), adj = 1, labels = paste0("slope = " , round(slope, digits)))
      text(x = 1, y = ymin + 0.115 * (ymax - ymin), adj = 1, labels = paste0("intercept = ", round(intercept, digits)), col = "darkgrey", cex = 0.9)
      text(x = 1, y = ymin + 0.05 * (ymax - ymin), adj = 1, labels = paste0("slopeDiff = ", round(slopeDiff, digits)), col = "darkred", cex = 0.9)
    }  # end if plot.values
  }  # end if plot

  # return(list(intercept = intercept, slope = slope, slope.pvalue = slope.p))
  list(intercept = intercept, slope = slope, slopeDiff = slopeDiff)
}  # end MillerCalib function
