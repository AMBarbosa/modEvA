RsqGLM <- function(model = NULL, obs = NULL, pred = NULL, use = "pairwise.complete.obs") {
  # version 1.6 (18 Oct 2019)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values

  } else { # if model not provided
    
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'.")
    if (length(obs) != length(pred))  stop ("'obs' and 'pred' must have the same number of values (and in the same order).")
    
    pred[pred == 0] <- 2e-16  # avoid NaN in log below
    pred[pred == 1] <- 1 - 2e-16  # avoid NaN in log below
    
    # new (15 Sep 2015):
    dat <- data.frame(obs, pred)
    n.in <- nrow(dat)
    dat <- na.omit(dat)
    n.out <- nrow(dat)
    if (n.out < n.in)  warning (n.in - n.out, " observations removed due to missing data; ", n.out, " observations actually evaluated.")
    obs <- dat$obs
    pred <- dat$pred

    if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("Sorry, 'obs' and 'pred' options only implemented for binomial GLMs (binary observations with values 0 or 1, and probabilistic predictions with values between 0 and 1). For GLMs of other types, you need to provide a 'model' object.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }

  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  #like.M <- exp(loglike.M)
  #like.0 <- exp(loglike.0)
  #like.M <- like.M + 2.2e-16
  #like.0 <- like.0 + 2.2e-16  # zeros were causing problems, but this too
  N <- length(obs)

  # from http://stackoverflow.com/questions/3337044/model-fit-statistics-for-a-logistic-regression/15949904#15949904:
  CoxSnell <- 1 - exp((model$deviance - model$null.deviance) / 2 * N)
  Nagelkerke <- CoxSnell / (1 - exp((- model$null.deviance) / N))
  McFadden <- 1 - ((model$deviance / -2)/(model$null.deviance / -2))

  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))

  # based on Allison 2014:
  #CoxSnell <- 1 - (loglike.0 / loglike.M) ^ (2 / N)
  #Nagelkerke <- CoxSnell / (1 - loglike.0 ^ (2 / N))
  McFadden <- 1 - (loglike.M / loglike.0)
  
  if (family(model)$family == "binomial" & family(model)$link == "logit")
    #Tjur <- abs(diff(unique(ave(pred, obs))))
    #Tjur <- diff(tapply(pred, obs, mean))
    Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  else {
    Tjur <- NA
    message("NOTE: Tjur R-squared applies only to binomial GLMs")
  }
  
  sqPearson <- cor(obs, pred, use = use) ^ 2

  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}
