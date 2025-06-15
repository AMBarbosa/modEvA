RMSE <- function(model = NULL, obs = NULL, pred = NULL, na.rm = TRUE, rm.dup = FALSE, verbosity = 2) {
  # version 1.1 (19 Nov 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  squared_error <- (obs - pred) ^ 2
  # return(sd(obs - pred))
  return(sqrt(mean(squared_error)))
}
