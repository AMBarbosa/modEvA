errorMeasures <- function(model = NULL, obs = NULL, pred = NULL, na.rm = TRUE, rm.dup = FALSE, verbosity = 2) {
  # version 1.0 (2 Oct 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  SE <- (obs - pred) ^ 2
  MSE <- mean(SE)
  Brier_score <- MSE
  RMSE <- sqrt(MSE)
  # SSE <- sum(SE)

  return(list(RMSE = RMSE, MSE = MSE, Brier_score = Brier_score))
}
