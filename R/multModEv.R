multModEv <-
  function(models = NULL, obs.data = NULL, pred.data = NULL, measures = modEvAmethods("multModEv"), standardize = FALSE, thresh = NULL, bin.method = NULL, verbosity = 0, ...) {
    # version 2.4 (24 Nov 2024)
    
    #  if (Favourability == TRUE & thresh == "preval") {
    #    thresh <- 0.5
    #    message("Threshold automatically set to 0.5, which corresponds to prevalence when Favourability is used.")
    #  }
    
    if (!is.null(models) && !all(class(models) == "list")) stop ("'models' must be an object of class 'list' containing the model object(s).")
    
    if (!is.null(models) && (!all(class(models) == "list") || !all(unique(lapply(models, class))[[1]] == c("glm", "lm"))))
      stop ("'models' must be an object of class 'list' containing only model object(s) of class 'glm'.")
    
    for (m in measures) {
      if (!(m %in% modEvAmethods("multModEv"))) stop(m, " is not a valid measure; type modEvAmethods('multModEv') for available options.")
    }  # end for m
    
    thresh.measures <- measures[measures %in% modEvAmethods("threshMeasures")]
    if (length(thresh.measures) > 0 && is.null(thresh)) stop ("'thresh' must be specified for measures ", paste(thresh.measures, collapse = ", "))
    
    if (!is.null(bin.method) && !(bin.method %in% modEvAmethods("getBins"))) stop("Invalid bin.method; type modEvAmethods('getBins') for available options.")
    
    if (is.null(bin.method) && ("HL" %in% measures | "HL.p" %in% measures))  stop ("bin.method must be specified if HL or HLp are included in 'measures'.")
    
    calib.measures <- measures[measures %in% c("HL", "HL.p", "RMSE", "Miller.int", "Miller.slope")]
    if (verbosity > 1 && length(calib.measures) > 0)  warning(paste0(calib.measures, ", "), "valid only if your input 'pred.data' represent presence probability; otherwise, please ignore these measures, or use probability as input instead.")
    
    if (is.null(models)) {
      if (is.null(obs.data) || is.null(pred.data)) stop("You must provide either a list of model object(s) of class 'glm', or a set of obs.data + pred.data with matching dimensions.")
      #if (Favourability) message("'pred.data' converted with the Fav function; set Favourability to FALSE if this is not what you wish.")
    }  # end if !models
    
    else {  # if !null models
      if (verbosity > 1 && !is.null(obs.data)) message("Argument 'obs.data' ignored in favour of 'models'.")
      if (verbosity > 1 && !is.null(pred.data)) message("Argument 'pred.data' ignored in favour of 'models'.")
      
      n <- sapply(models, function(x) length(resid(x)))
      if(!(all(n == n[1]))) stop("Models must all be based on the same sample size.")
      
      obs.data <- pred.data <- fav.data <- matrix(data = NA, nrow = length(resid(models[[1]])), ncol = length(models), dimnames = list(NULL, names(models)))
      for (m in 1 : length(models)) {
        obs.data[ , m] <- models[[m]]$y
        pred.data[ , m] <- models[[m]]$fitted.values
        #if (Favourability) fav.data[ , m] <- Fav(obs = obs.data[ , m], pred = pred.data[ , m])
      }  # end for m
    }  # end if models
    
    # if (Favourability) pred.data <- fav.data
    
    #  stopifnot(
    #    is.null(models) | is.list(models),
    #    dim(obs.data) == dim(pred.data),
    #    as.vector(as.matrix(obs.data)) %in% c(0,1),
    #    as.vector(as.matrix(pred.data)) >= 0,
    #    as.vector(as.matrix(pred.data)) <= 1,
    #    is.logical(Favourability),
    #  )
    
    n.models <- ncol(obs.data)
    n.measures <- length(measures)
    results <- matrix(NA, nrow = n.models, ncol = n.measures)
    rownames(results) <- colnames(obs.data)
    colnames(results) <- measures
    
    if (verbosity > 1) message(n.models, " models to evaluate; ", n.measures,  " measures to calculate for each.")
    
    #  if (any(measures %in% modEvAmethods("threshMeasures"))) {
    #    thresh.measures <- measures[measures %in% modEvAmethods("threshMeasures")]
    #    cat("- using '", thresh, "' as the threshold value to calculate ", paste(thresh.measures, collapse = ", "), "\n", sep = "")  # doesn't quite work with 'message' or 'paste' or with a 'sep'
    #  }  # this needs to be oustide the 'for' loop
    
    #  bin.measures <- c("HL", "HL.p", "RMSE")  # ABC, unityRsq
    #  if (any(measures %in% bin.measures)) {
    #    bin.measures <- measures[measures %in% bin.measures]
    #    cat("- using '", bin.method, "' as the bin method to calculate ", paste(bin.measures, collapse = ", "), "\n", sep = "")  # doesn't quite work with 'message' or 'paste' or with a 'sep'
    #  }  # this needs to be oustide the 'for' loop
    
    for (m in 1:n.models) {
      
      if (verbosity > 0) message("Evaluating model ", m, "...")
      
      if ("Prevalence" %in% measures)
        results[m, "Prevalence"] <- prevalence(obs.data[ , m])
      
      if ("Evenness" %in% measures)
        results[m, "Evenness"] <- evenness(obs.data[ , m])
      
      if (any(measures %in% c("AUC", "MeanPrecision"))) {
        auc <- AUC(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE)
        if ("AUC" %in% measures)
          results[m, "AUC"] <- auc$AUC
        if ("MeanPrecision" %in% measures)
          results[m, "MeanPrecision"] <- auc$meanPrecision
      }
      
      if ("AUCPR" %in% measures)
        results[m, "AUCPR"] <- AUC(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE, curve = "PR")$AUC
      
      if (any(measures %in% modEvAmethods("threshMeasures"))) {
        for (m in 1:n.models)  for (tm in thresh.measures) {
          results[m, tm] <- threshMeasures(obs = obs.data[ , m], pred = pred.data[ , m], measures = tm, thresh = thresh, standardize = standardize, simplif = TRUE, plot = FALSE, verbosity = verbosity)
        }; rm(m, tm)
      }  # end if measures in modEvAmethods("threshMeasures")
      # thresh.measures <- optiThresh(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE, optimize = "each")
      
      if (any(measures %in% c("HL", "HL.p"))) {
        for (m in 1:n.models) {
          HL <- HLfit(obs = obs.data[ , m], pred = pred.data[ , m], bin.method = bin.method, simplif = TRUE, verbosity = verbosity, ...)
          if ("HL" %in% measures)  results[m, "HL"] <- HL$chi.sq
          if ("HL.p" %in% measures)  results[m, "HL.p"] <- HL$p.value
          if ("RMSE" %in% measures)  results[m, "RMSE"] <- HL$RMSE
        }; rm(m)
      }  # end if HL
      
      if (any(measures %in% c("Miller.int", "Miller.slope"))) {
        for (m in 1:n.models) {
          Miller <- MillerCalib(obs = obs.data[ , m], pred = pred.data[ , m], plot = FALSE)
          if ("Miller.int" %in% measures)
            results[m, "Miller.int"] <- Miller$intercept
          if ("Miller.slope" %in% measures)
            results[m, "Miller.slope"] <- Miller$slope
          #if ("Miller.p" %in% measures)
          #  results[m, "Miller.p"] <- Miller$slope.pvalue
        }; rm(m)
      } # end if Miller
      
      #if (any(measures %in% c("ABCc", "rABCc", "unityRsq")))
      #  ABC <- ABCmodev(obs.data[ , m], pred.data[ , m], bin.method = bin.method, plot = FALSE, simplif = TRUE)
      #if ("ABCc" %in% measures)
      #  results[m, "ABCc"] <- ABC $ ABCc
      #if ("rABCc" %in% measures)
      #  results[m, "rABCc"] <- ABC $ rABCc
      #if ("unityRsq" %in% measures)
      #  results[m, "unityRsq"] <- ABC $ unityR2
      
    }  # end for m
    
    if (standardize) {
      #    colnames(results) <- sub(pattern = "kappa", replacement = "skappa", x = colnames(results))
      #    colnames(results) <- sub(pattern = "TSS", replacement = "sTSS", x = colnames(results))
      colnames(results)[colnames(results) == "kappa"] <- "skappa"
      colnames(results)[colnames(results) == "TSS"] <- "sTSS"
    }  # end if standardize
    
    if (!is.null(rownames(results))) results <- data.frame(Model = rownames(results), results, row.names = NULL)
    if (verbosity > 0) message("Finished!")
    return(results)
  }

