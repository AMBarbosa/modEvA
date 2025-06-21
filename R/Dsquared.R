Dsquared <- function(model = NULL,
                     obs = NULL,
                     pred = NULL,
                     family = NULL,
                     adjust = FALSE,
                     npar = NULL,
                     na.rm = TRUE,
                     rm.dup = FALSE,
                     pbg = FALSE,
                     dismo.version = FALSE,
                     verbosity = 2) {
  # version 2.4 (19 Nov 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, pbg = pbg, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (inherits(model, "glm")) family <- family(model)$family
  }

  if (is.null(family)) {
    if (all(obs %in% c(0, 1))) family <- "binomial"
    else if (all(obs %% 1 == 0) && all(obs > 0)) family <- "poisson"
    # else if (.....) family <- "laplace"
    # else if (.....) family <- "Gamma"
    else family <- "gaussian"
    message("'family' not specified; assuming '", family, "' based on the values of the response variable.\n")
  }  # end if null family


  if (!dismo.version) {

    if (!model.provided) {
      message("'model' object not provided; using glm() for computing the null model and the corresponding null deviance -- result can be somewhat inaccurate (normally from the 3rd decimal digit) if 'pred' comes from a different type of model. You can try instead with dismo.version=TRUE.\n")
    }

    # OLD VERSION (uses GLM for null deviance):

    pred[pred == 0] <- 2e-16  # avoid NaN in log below
    pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below

    if (family == "binomial") {
      if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("'binomial' family implies that 'obs' should be binary (with values 0 or 1) and 'pred' should be bounded between 0 and 1.")
      link <- log(pred / (1 - pred))  # logit
    }  # end if binomial

    else if (family == "poisson") {
      if (any(obs %%1 != 0)) stop ("'poisson' family implies that 'obs' should be integer.")
      link <- log(pred)
    }  # end if poisson

    model <- glm(obs ~ link, family = family)

    Dsq <- (model$null.deviance - model$deviance) / model$null.deviance
    return(Dsq)
  }


  dev <- function (obs, pred, family = family) {  # based on code from dismo::calc.deviance
    if (family %in% c("binomial", "bernoulli")) {
      d <- -2 * sum(obs * log(pred) + (1 - obs) * log(1 - pred))  # NOTE: this only works correctly if 'obs' is binary (cf. Elic Weitzel email bug report)
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
      stop("'family' should be one of: \"binomial\", \"bernoulli\", \"poisson\", \"laplace\", \"gaussian\"")
    }
    return(mean(d))
  }  # end 'dev' function

  if (family %in% c("binomial", "bernoulli") && !all(obs %in% c(0, 1))) {
    warning("family is 'binomial' / 'bernoulli'\nbut response is not binary (with only values of 0 or 1).")

    if (!model.provided) {
      message("Using 'binomial' family with 'logit' link.")
      link <- log(pred / (1 - pred))
      model <- glm(obs ~ link, family = family)
    }

    deviance <- model$deviance
    null.deviance <- model$null.deviance
  } else {
    deviance <- dev(obs = obs, pred = pred, family = family)
    null.deviance <- dev(obs = obs, pred = rep(mean(obs), length(obs)), family = family)
  }

  Dsq <- (null.deviance - deviance) / null.deviance

  if (adjust) {
    if (model.provided && inherits(model, "glm")) {
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

  # Dsq <- round(Dsq, 4)  # result can be slightly inexact beyond 4 decimals
  return(Dsq)
}
