Dsquared <- function(model = NULL,
                     obs = NULL,
                     pred = NULL,
                     family = NULL,
                     adjust = FALSE,
                     npar = NULL,
                     na.rm = TRUE,
                     rm.dup = FALSE) {
  # version 2.0 (20 Dec 2022)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    # if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (is(model, "glm")) family <- family(model)$family
  }
  
  # OLD VERSION:
  #
  # pred[pred == 0] <- 2e-16  # avoid NaN in log below
  # pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below
  #
  # if (family == "binomial") {
  #   if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("'binomial' family implies that 'obs' should be binary (with values 0 or 1) and 'pred' should be bounded between 0 and 1.")
  #   link <- log(pred / (1 - pred))  # logit
  # }  # end if binomial
  #
  # else if (family == "poisson") {
  #   if (any(obs %%1 != 0)) stop ("'poisson' family implies that 'obs' should be integer.")
  #   link <- log(pred)
  # }  # end if poisson
  #
  # model <- glm(obs ~ link, family = family)
  #
  # Dsq <- (model$null.deviance - model$deviance) / model$null.deviance


  if (is.null(family)) {
    if (all(obs %in% c(0, 1))) family <- "binomial"
    else if (all(obs %% 1 == 0) && all(obs > 0)) family <- "poisson"
    else family <- "gaussian"
    message("'family' not specified; assuming '", family, "' based on the values of the response variable.")
  }  # end if null family

  dev <- function (obs, pred, family = family) {  # based on code from dismo::calc.deviance
    if (family %in% c("binomial", "bernoulli")) {
      d <- -2 * sum((obs * log(pred)) + ((1 - obs) * log(1 - pred)))
    }
    else if (family == "poisson") {
      d <- 2 * sum(ifelse(obs == 0, 0, (obs * log(obs/pred))) - (obs - pred))
    }
    else if (family == "laplace") {
      d <- sum(abs(obs - pred))
    }
    else if (family == "gaussian") {
      d <- sum((obs - pred) * (obs - pred))
    }
    else {
      stop("unknown family, should be one of: \"binomial\", \"bernoulli\", \"poisson\", \"laplace\", \"gaussian\"")
    }
    return(mean(d))
  }  # end 'dev' function

  deviance <- dev(obs = obs, pred = pred, family = family)
  null.deviance <- dev(obs = obs, pred = mean(obs), family = family)
  Dsq <- (null.deviance - deviance) / null.deviance

  if (adjust) {
    if (model.provided && is(model, "glm")) {
      n <- length(model$y)
      #p <- length(model$coefficients)
      p <- attributes(logLik(model))$df
    } else {
      if (is.null(npar)) stop ("'adjust=TRUE' requires either providing a 'model' argument of class 'glm', or specifying 'npar'.")
      n <- length(na.omit(obs))
      p <- npar
    }  # end if model.provided else

    Dsq <- 1 - ((n - 1) / (n - p)) * (1 - Dsq)
  }  # end if adjust

  return(Dsq)
}
