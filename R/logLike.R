logLike <- function(model = NULL,
                    obs = NULL,
                    pred = NULL,
                    na.rm = TRUE,
                    plot = TRUE,
                    verbosity = 2) {
  # version 1.1 (19 Nov 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  tolog <- pred * obs  +  (1 - pred) * (1 - obs)

  if(any(tolog == 0))  # prevent -Inf output
    tolog <- (pred + 2e-16) * obs  +  (1 - pred) * (1 - obs)

  if (plot) {
    plot(sort(log(tolog)), pch = 10, cex = 0.1, ylab = "Sorted log values")
    par(new = TRUE)
    plot(sort(tolog), pch = 10, cex = 0.1, axes = FALSE, bty = "n", xlab = "", ylab = "", col = "darkgrey")
    axis(side = 4, at = pretty(range(tolog)), col.axis = "darkgrey")
    mtext("Sorted values", side = 4, line = 3, col = "darkgrey")
  }

  sum(log(tolog), na.rm = na.rm)
}
