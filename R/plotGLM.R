plotGLM <- function(model = NULL, obs = NULL, pred = NULL, link = "logit",
         plot.values = TRUE, plot.digits = 3, xlab = "Logit (Y)",
         ylab = "Predicted probability", main = "Model plot",
         na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 2.2 (6 May 2022)

  model.provided <- ifelse(is.null(model), FALSE, TRUE)

  # if (model.provided) {
  #   if(!("glm" %in% class(model) && model$family$family == "binomial" && model$family$link == "logit")) stop ("'model' must be an object of class 'glm' with 'binomial' family and 'logit' link.")
  #   if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
  #   if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
  #   obs <- model$y
  #   pred <- model$fitted.values
  #   
  # } else { # if model not provided
  #   
  #   if (is.null(obs) | is.null(pred)) stop("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
  # }  # end if model
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)  
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  stopifnot(
    #length(obs) == length(pred),
    obs %in% c(0, 1)#,
    #pred >= 0,
    #pred <= 1
    )
  
  if (any(pred < 0) | any(pred > 1)) warning("Some of your 'pred' values are outside the [0,1] interval; are you sure these represent probabilities? Unexpected or incorrect results may arise. Consider properly rescaling you 'pred' values, or obtaining real probabilities instead.")
  
  pred[pred == 0] <- 2e-16  # avoid log 0 below
  pred[pred == 1] <- 1 - 2e-16  # avoid division by 0 below
  
  logit <- log(pred / (1 - pred))
  if (!model.provided) model <- glm(obs ~ logit, family = "binomial")

  plot(pred ~ logit, ylim = c(0, 1), type = "n", xlab = xlab, ylab = ylab,
       main = main, ...)
  points(obs ~ logit, pch = 1, col = "darkgrey")
  abline(v = 0, lty = 5, col = "grey")  # y of P = 0.5
  #abline(h = pred[which.min(abs(logit))], col = "lightgrey", lty = 2)
  points(pred ~ logit, pch = 20, cex = 0.6)

  if (plot.values) {
    Dsq <- round(Dsquared(model = model, adjust = FALSE), plot.digits)
    Rsq <- RsqGLM(model = model, plot = FALSE)
    CoxSnell <- round(Rsq$CoxSnell, plot.digits)
    McFadden <- round(Rsq$McFadden, plot.digits)
    Nagelkerke <- round(Rsq$Nagelkerke, plot.digits)
    Tjur <- round(Rsq$Tjur, plot.digits)

    #minX <- min(log(mod$fitted/(1 - mod$fitted)))
    
    #if (max(logit) > abs(min(logit)))  x.loc <- c(max(logit), 1)
    #else x.loc <- c(min(logit), 0)
    
    if (max(logit) > abs(min(logit))) {
      x.loc <- max(logit)
      adj <- 1
    } else {
      x.loc <- min(logit)
      adj <- 0
    }
    
    text(x.loc, 0.95, substitute(paste(D^2 == a), list(a = Dsq)), adj = adj)
    text(x.loc, 0.8, substitute(paste(R[Cox-Snell]^2 == a), list(a = CoxSnell)), adj = adj)
    text(x.loc, 0.6, substitute(paste(R[McFadden]^2 == a), list(a = McFadden)), adj = adj)
    text(x.loc, 0.4, substitute(paste(R[Nagelkerke]^2 == a), list(a = Nagelkerke)), adj = adj)
    text(x.loc, 0.2, substitute(paste(R[Tjur]^2 == a), list(a = Tjur)), adj = adj)
    
    #text(x = x.loc[1], y = 0.6, adj = x.loc[2], labels = substitute(paste(D^2 == a), list(a = round(Dsq, plot.digits))))
    #if (model.provided) {  # adjDsq needs n parameters in original model, not just our model created from obs~logit
    #  adjDsq <- Dsquared(model = model, adjust = TRUE)
    #  text(x = x.loc[1], y = 0.4, adj = x.loc[2], labels = substitute(paste('D'['adj']^2) == a, list(a = round(adjDsq, plot.digits))))
    #}  # end if model provided
    
  }  # end if plot values
}
