mod2obspred <- function(model, obs.only = FALSE, x.only = FALSE) {
  # version 2.1 (21 Jun 2025)
  
  if (x.only) {  # new (4 Mar 2025)
    if (inherits(model, "glm"))
      return(model$model[ , -1])
    
    if (inherits(model, "GBMFit"))
      return(model$gbm_data_obj$x)
    
    if (inherits(model, "bart")) {
      if (is.null(model$fit$data)) stop("'$fit$data' section not present in 'model'. Try running 'bart()' with 'keeptrees=TRUE'.")
      return(model$fit$data@x)  # requires model ran with keeptrees=TRUE
    }
    
    stop("\n'data' (x, predictors) cannot be extracted from a 'model' of this class.")
  }  # end if x.only
  

  if (inherits(model, c("glm", "gam"))) {
    obs <- model$y
    if (!obs.only) pred <- model$fitted.values

  } else if (inherits(model, "gbm")) {
    obs <- model$data$y
    if (!obs.only) {
      pred <- suppressMessages(predict(model, type = "response"))  # checked same as:
      # logit <- function(x) exp(x) / (1 + exp(x))
      # pred <- logit(model$fit)  # but this applies only to binary response
    }

  } else if (inherits(model, "GBMFit")) {
    obs <- model$gbm_data_obj$y
    if (!obs.only) {
      pred <- suppressMessages(predict(model, type = "response", newdata = model$gbm_data_obj$original_data, n.trees = length(model$trees)))  # checked same as:
      # logit <- function(x) exp(x) / (1 + exp(x))
      # pred <- logit(model$fit)  # but this applies only to binary response
    }

  } else if (inherits(model, "randomForest")) {
    obs <- as.integer(as.character(model$y))
    if (!obs.only) pred <- predict(model, type = "prob")[ , "1"]

  } else if (inherits(model, "bart")) {
    if (is.null(model$fit$data)) stop("'$fit$data' section not present in 'model'. Try running 'bart()' with 'keeptrees=TRUE'.")
    obs <- model$fit$data@y  # requires model ran with keeptrees=TRUE
    if (!obs.only) pred <- fitted(model, type = "response")

  } else stop("'model' is of a non-implemented class.")

  if (obs.only) return(data.frame(obs = obs))
  return(data.frame(obs = obs, pred = pred))
}
