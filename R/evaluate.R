evaluate <-
  function(a, b, c, d, N = NULL, measure = "CCR"){
    # version 1.3 (25 Mar 2024)
    # internal function to be used by others in the package
    # a, b, c, d: elements of the confusion matrix (TP, FP, FN, TN)
    # N: sample size (total number of observations); calculated automatically if NULL
    # measure: evaluation measure to use
    # note: TSS and NMI are not symmetrical ("obs" vs "pred" != "pred" vs "obs")
    # note that some measures (e.g. NMI, odds ratio) don't work with zeros in (some parts of) the confusion matrix
    if (is.null(N))  N <- a + b + c + d
    stopifnot(N == a + b + c + d)
    if (measure == "CCR") { value <- (a+d)/N
    } else if(measure %in% c("Sensitivity", "Recall")) { value <- a/(a+c)
    } else if(measure == "Specificity") { value <- d/(b+d)
    } else if(measure == "Omission") { value <- c/(a+c)
    } else if(measure == "Commission") { value <- b/(b+d)
    } else if(measure %in% c("PPP", "Precision")) { value <- a/(a+b)
    } else if(measure == "NPP") { value <- d/(c+d)
    } else if(measure == "Misclass") { value <- (b+c)/N
    } else if(measure == "UPR") { value <- c/(c+d)  # this and next 3: Barbosa et al. 2013 Diversity and Distributions
    } else if(measure == "OPR") { value <- b/(a+b)
    } else if(measure == "PPI") { value <- ((a+b)/(a+c))-1
    } else if(measure == "PAI") { value <- ((c+d)/(b+d))-1
    } else if(measure == "kappa") { value <- ((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))/(N-(((a+c)*(a+b)+(b+d)*(c+d))/N))
    } else if(measure == "TSS") { value <- (a*d - b*c) / ((a+c) * (b+d))
    } else if(measure == "NMI") { value <- 1-((-a*log(a)-b*log(b)-c*log(c)-d*log(d)+(a+b)*log(a+b)+(c+d)*log(c+d))/(N*log(N)-((a+c)*log(a+c)+(b+d)*log(b+d))))  # NMI by Forbes (1995); the "1-" was missing in Fielding & Bell 1997 (and Manel et al 2001) but Fielding confirmed it was a typo
    } else if (measure == "F1score") {
      precision <- a/(a+b)
      recall <- a/(a+c)
      numerator <- precision * recall
      denominator <- precision + recall
      value <- 2 * (numerator / denominator)
      value[numerator == 0] <- 0  # euze
    } else if(measure == "OddsRatio") { value <- (a*d)/(c*b)
    # (c*b)/(a*d)  # inverse, would give a (more expectable) unimodal plot of odds against thresholds
    } else if(measure == "ORSS") { value <- (a*d - b*c) / (a*d + b*c)
    } else if(measure == "SEDI") {
      H_ = a / (a + c)
      F_ = b / (b + d)
      value <- (log(F_) - log(H_) - log(1 - F_) - log(1 - H_)) / (log(F_) + log(H_) + log(1 + F_) + log(1 + H_))
    } else stop("Invalid measure; available options are ",
                paste(.modEvAmethods("threshMeasures"), collapse = ", "))
    return(value)
  }
