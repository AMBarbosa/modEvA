MillerCalib <- function(model = NULL, obs = NULL, pred = NULL, plot = TRUE, line.col = "black", diag = TRUE, diag.col = "grey", plot.values = TRUE, digits = 2, xlab = "", ylab = "", main = "Miller calibration", ...) {
  # version 1.4 (7 Oct 2018)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values

  } else { # if model not provided

    if (is.null(obs) | is.null(pred))  stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'.")
    if (length(obs) != length(pred))  stop ("'obs' and 'pred' must have the same number of values (and in the same order).")
    # new (15 Sep 2015):
    dat <- data.frame(obs, pred)
    n.in <- nrow(dat)
    dat <- na.omit(dat)
    n.out <- nrow(dat)
    if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
    obs <- dat$obs
    pred <- dat$pred
  }

  stopifnot(
    length(obs) == length(pred),
    obs %in% c(0,1)#,
    #pred >= 0,
    #pred <= 1
  )
  # new:
  if (any(pred < 0) | any(pred > 1)) warning("Some of your predicted values are outside the [0, 1] interval; are you sure these represent probabilities?")

  pred[pred == 0] <- 2e-16  # avoid NaN in log below
  pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below

  logit <- log(pred / (1 - pred))
  mod <- glm(obs ~ logit, family = binomial)
  intercept <- mod$coef[[1]]
  slope <- mod$coef[[2]]
  #std.err <- summary(mod)$coefficients["logit", "Std. Error"]
  #slope.p <- abs((slope - 1) / sqrt(std.err^2 + 0))  # Paternoster 98; http://stats.stackexchange.com/questions/55501/test-a-significant-difference-between-two-slope-values
  #slope.t <- (slope - 1) / std.err
  #slope.p <- pt(slope.t, df = mod$df.residual)  # http://stats.stackexchange.com/questions/111559/test-model-coefficient-regression-slope-against-some-value
  # both values look wrong...

  if (plot) {
    ymin <- min(0, intercept)
    ymax <- max(1, intercept + 0.3)
    plot(c(0, 1), c(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main, ...)
    if (diag) abline(0, 1, col = diag.col, lty = 2)
    abline(intercept, slope, col = line.col)
    if (plot.values) {
      #plotext <- paste("intercept =" , round(intercept, digits), "\nslope =", round(slope, digits), "\nslope p-value =", round(slope.p, digits))
      plotext <- paste0("intercept = " , round(intercept, digits), "\nslope = ", round(slope, digits))
      text(x = 1, y = ymin + 0.1 * (ymax - ymin), adj = 1, labels = plotext)
    }  # end if plot.values
  }  # end if plot

  #return(list(intercept = intercept, slope = slope, slope.pvalue = slope.p))
  list(intercept = intercept, slope = slope)
}  # end MillerCalib function
