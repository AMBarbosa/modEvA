varImp <- function(model, imp.type = "each", relative = TRUE, reorder = TRUE, group.cats = FALSE, n.per = 10, data = NULL, n.trees = 100, plot = TRUE, plot.type = "lollipop", error.bars = "sd", ylim = "auto0", col = c("#4477aa", "#ee6677"), plot.points = TRUE, legend = TRUE, grid = TRUE, verbosity = 2, ...) {
  
  # version 2.7 (6 Mar 2025)
  
  # if 'col' has length 2 and varImp has negative values (e.g. for z-value), those will get the second colour
  
  imp.type <- match.arg(imp.type, c("each", "permutation"))
  plot.type <- match.arg(plot.type, c("lollipop", "barplot", "boxplot"))
  
  stopifnot(# is.numeric(error.bars) || error.bars %in% c("sd", "range"),
    is.logical(TRUE),
    is.logical(plot),
    is.logical(plot.points),
    is.logical(legend),
    is.logical(grid),
    length(col) %in% 1:2
  )
  
  is_bart <- methods::is(model, "bart") || methods::is(model, "pbart") || methods::is(model, "lbart")
  is_flexbart <- methods::is(model, "list") && c("varcounts", "trees") %in% names(model)
  
  if ((is_bart || is_flexbart) && imp.type == "permutation")
    stop("imp.type='permutation' not yet implemented for BART models")
    
  
  if (!is_bart && !is_flexbart)  error.bars <- NA
  
  if (reorder && is_flexbart && is.null(colnames(model[["varcounts"]]))) {
    reorder <- FALSE
    if (verbosity > 0) message ("'reorder' set to FALSE, as this version of 'flexBART' does not carry variable names, which would make it impossible to match variables with importance values. Variables are in the order in which they were provided to the 'flexBART' model, with the continuous preceding the categorical ones. Update 'flexBART' if you want the variables named and reordered.")
  }
  
  
  if (imp.type == "each") {
    
    if (methods::is(model, "glm")) {  #  && !methods::is(model, "Gam")
      
      if (family(model)$family != "binomial")  stop ("This function is currently only implemented for binary-response models of family 'binomial'.")
      
      # if (measure == "z") {
      metric <- ifelse(isTRUE(relative), "Relative z value", "Absolute z value")
      if (verbosity > 1) cat("\nMetric:", metric, "\n\n")
      varimp <- summary(model)$coefficients[-1, "z value"]
      if (isTRUE(relative)) varimp <- varimp / sum(abs(varimp))
      ylab <- metric
      # }
      
      # if (measure == "Wald") {  # requires 'fuzzySim' and 'aod'
      #   ylab <- "Wald"
      #   legend <- FALSE
      #   smry <- summaryWald(model, interceptLast = FALSE)[-1, ]
      #   varimp <- smry[ , "Wald"]
      #   names(varimp) <- rownames(smry)
      #   varimp <- varimp[order(varimp, decreasing = TRUE)]
      # }
    }  # end if glm
    
    else if (methods::is(model, "gbm")) {
      # requireNamespace("gbm")  # would require a suggest/depend
      if ("gbm" %in% .packages()) {
        metric <- "Relative influence"
        if (verbosity > 1) cat("\nMetric:", metric, "\n\n")
        ylab <- metric
        smry <- summary(model, plotit = FALSE)
        varimp <- smry[ , "rel.inf"] / 100
        names(varimp) <- smry[ , "var"]
      } else {
        stop("package 'gbm' needs to be loaded first.")
      }
    }
    
    else if (methods::is(model, "GBMFit")) {
      if ("gbm3" %in% .packages()) {
        metric <- "Relative influence"
        if (verbosity > 1) cat("\nMetric:", metric, "\n\n")
        ylab <- metric
        smry <- summary(model, plot_it = FALSE)
        varimp <- smry[ , "rel_inf"] / 100
        names(varimp) <- smry[ , "var"]
      } else {
        stop("package 'gbm3' needs to be loaded first.")
      }
    }
    
    else if (methods::is(model, "randomForest")) {
      metric <- colnames(model$importance)
      if (verbosity > 1) cat("\nMetric:", metric, "\n\n")
      varimp <- model$importance  # / nrow(model$importance) / 100  # doesn't work well for mean accuracy decrease
      names(varimp) <- rownames(model$importance)
      ylab <- metric
    }
    
    
    else if (is_bart || is_flexbart) {
      
      # metric <- "Proportion of splits used"
      metric <- ifelse(isTRUE(relative), "Proportion of splits used", "Number of splits used")
      if (verbosity > 1) cat("\nMetric:", metric, "\n\n")
      ylab <- metric
      
      if ("varcounts" %in% names(model)) { # in flexBART models
        names(model)[grep("varcounts", names(model))] <- "varcount"  # to homogenize
      }
      
      # varimps <- model[["varcount"]] / rowSums(model[["varcount"]])
      varimps <- model[["varcount"]]
      if (isTRUE(relative)) varimps <- varimps / rowSums(model[["varcount"]])
      varimp <- colMeans(varimps)
      
      if (group.cats) {
        if (methods::is(model, "bart")) {
          cat.vars <- names(which(lapply(attr(model$fit$data@x, "drop"), length) > 1))
          names.nosuffix <- names(varimp)
          for (v in cat.vars) {
            v.inds <- grep(v, colnames(model$fit$data@x))
            names.nosuffix[v.inds] <- v
          }
        }
        
        else if (methods::is(model, "pbart") || methods::is(model, "lbart")) {
          names.nosuffix <- gsub("[0-9]+$", "", colnames(model$varcount))
        }  # but WATCH OUT: other variables with numeric suffix (e.g. "o2" and "o3") will be grouped too! also cat vars with same name but different numeric suffix, e.g. "var" and "var2"
        
        if (!is_flexbart) {
          
          varimp.df <- data.frame(names = names.nosuffix, varimp, row.names = NULL)
          varimp.df <- aggregate(varimp.df$varimp, by = list(varimp.df$names), FUN = sum)
          varimp <- varimp.df$x
          names(varimp) <- varimp.df$Group.1
          
          colnames(varimps) <- names.nosuffix
          varimps.agg <- apply(varimps, 1, aggregate, sum, by = list(colnames(varimps)))
          varimps.agg <- lapply(varimps.agg, getElement, "x")
          varimps <- do.call(rbind.data.frame, varimps.agg)
          colnames(varimps) <- names(varimp)
        }
      }  # end if group.cats
      
      if (methods::is(model, "bart")) {  #  || methods::is(model, "pbart") || methods::is(model, "lbart")
        if (is.null(model$fit)) {
          stop("'model' does not contain the required info; please compute it with keeptrees=TRUE")
        }
        
        if (is.null(colnames(model$fit$data@x))) {
          # colnames(model$fit$data@x) <- paste0("X", 1:ncol(model$fit$data@x))  # didn't work, pbb because other attributes were missing
          stop("'model' does not have predictor attributes; please compute it with column names in the predictor variables")
        }
        
        # if (methods::is(model, "bart"))
        dropped.vars <- names(which(unlist(attr(model$fit$data@x, "drop")) == 1))
        # else dropped.vars <- colnames(model$varcount)[model$rm.const]  # no, because these names already have the cat vars divided and renamed according to their factor levels, so the 'rm.const' indices do not correctly match the original var names
        
        n.dropped <- length(dropped.vars)
        if (n.dropped > 0) {
          if (verbosity > 0) message("The following variables had been automatically dropped by the model (e.g. for having no variability):  ", paste(dropped.vars, collapse = ", "))
          dropped.varimp <- rep(0, n.dropped)
          names(dropped.varimp) <- dropped.vars
          varimp <- c(varimp, dropped.varimp)
          varimps[ , dropped.vars] <- 0
        }
      }
      
    }  # end if bart
    
    else if (methods::is(model, "maxnet")) {
      
      if (imp.type == "each") {
        if (verbosity > 0) message("'imp.type' changed to 'permutation', as 'each' is not implemented for this class of 'model'.")
        imp.type <- "permutation"
      }
      
    }
      
    
    else stop ("'model' is of a non-implemented class.")
    
  }  # end if imp.type = "each"
  
  
  if (imp.type == "permutation") {  # do not add 'else', to allow imp.type changes above
    
    metric <- "Permutation importance"
    
    
    if (methods::is(model, "glm") || methods::is(model, "Gam") || methods::is(model, "gbm") || methods::is(model, "GBMFit"))
      pred.type <- "response"
    
    else if (methods::is(model, "randomForest"))
      pred.type <- "prob"
    
    else if (is_bart)
      pred.type <- "ev"
    
    else if (methods::is(model, "maxnet"))
      pred.type <- "cloglog"
    
    
    if (is.null(data)) {
      data <- as.data.frame(mod2obspred(model, x.only = TRUE))
      names(data) <- gsub("s\\(|\\)", "", names(data))  # for gam models
    }
    
    
    if (inherits(model, "bart"))
      original_predictions <- as.vector(predict(model, as.data.frame(data), type = pred.type))
    else
      original_predictions <- as.vector(predict(model, as.data.frame(data), type = pred.type, n.trees = n.trees))
    
    varimp <- numeric(ncol(data))
    
    for (v in seq_along(varimp)) {
      permuted_scores <- numeric(n.per)
      
      for (p in 1:n.per) {
        permuted_data <- data
        permuted_data[ , v] <- sample(permuted_data[ , v])
        if (inherits(model, "bart"))
          permuted_predictions <- as.vector(predict(model, as.data.frame(data)))
        else
          permuted_predictions <- as.vector(predict(model, as.data.frame(permuted_data), type = pred.type))
        permuted_scores[p] <- sqrt(mean((original_predictions - permuted_predictions) ^ 2))  # RMSE
      }
      
      varimp[v] <- mean(permuted_scores)
    }  # end for v
    
    if (relative) {
      divisor <- sum(varimp)
      if (isTRUE(all.equal(divisor, 0))) divisor <- divisor + 0.00001
      varimp <- varimp / divisor
    }
    
    names(varimp) <- colnames(data)
    
  }  # end if imp.type="perm"
  
  
  if (reorder) {
    varimp <- varimp[order(abs(varimp), decreasing = TRUE)]
    if (is_bart)  varimps <- varimps[ , names(varimp)]
  }
  
  
  if (!is.na(error.bars)) {
    if (error.bars == "sd") {
      vsd <- sapply(as.data.frame(varimps), sd)
      eb_lower <- varimp - vsd
      eb_upper <- varimp + vsd
    } else if (error.bars == "range") {
      eb_lower <- apply(varimps, 2, min)
      eb_upper <- apply(varimps, 2, max)
    } else if (is.numeric(error.bars) && length(error.bars) == 1 && error.bars >= 0 && error.bars <= 1) {
      quants <- sapply(data.frame(varimps), quantile, probs = c(1 - error.bars, error.bars))
      eb_lower <- quants[1, ]
      eb_upper <- quants[2, ]
    } else stop ("Invalid 'error.bars' argument; see help for valid options.")
  }  # end if error.bars
  
  
  if (plot) {
    plot.args <- list(...)  # https://www.r-bloggers.com/2020/11/some-notes-when-using-dot-dot-dot-in-r/
    if (!("horiz" %in% names(plot.args)))
      plot.args$horiz <- FALSE
    if (!("xlab" %in% names(plot.args)))
      plot.args$xlab <- ifelse(isTRUE(plot.args$horiz), metric, "")
    if (!("ylab" %in% names(plot.args)))
      plot.args$ylab <- ifelse(isTRUE(plot.args$horiz), "", metric)
    
    if (length(col) == 1) col <- rep(col, 2)
    colrs <- ifelse(varimp >= 0, col[1], col[2])
    
    if (length(grep("auto", ylim)) > 0) {
      if ("auto0" %in% ylim) ymin <- 0
      else ymin <- ifelse(!is.na(error.bars), min(varimps), min(abs(varimp)))
      ymax <- ifelse(!is.na(error.bars), max(varimps), max(abs(varimp)))
      ylim <- c(ymin, ymax)
    }
    
    if (plot.type == "lollipop") {
      sticks <- ifelse(is.na(error.bars), TRUE, FALSE)
      
      do.call(lollipop,
              c(list(abs(varimp),
                     col = colrs,
                     names = names(varimp),
                     ylim = ylim,
                     las = 2,
                     sticks = sticks,
                     grid = grid),
                plot.args))
      
      if (!is.na(error.bars)) {
        arrows(x0 = 1:length(varimp), x1 = 1:length(varimp), y0 = eb_lower, y1 = eb_upper, code = 3, angle = 90, length = 0.03, col = colrs)
      }
    }  # end if lollipop
    
    
    if (plot.type == "barplot") {
      
      space <- 0.25  # re-used if plot.points & error.bars
      
      do.call(barplot,
              c(list(height = abs(varimp),
                     col = colrs,
                     border = NA,
                     space = space,
                     names = names(varimp),
                     ylim = ylim,
                     xpd = FALSE,
                     las = 2),
                plot.args
              )
      )
      
      if (grid) grid()
      
      if (plot.points || !is.na(error.bars)) {
        nbars <- length(names(varimp))
        xbars <- ((1 : nbars) - space) + space * (0 : (nbars - 1))
      }
      
      if (!is.na(error.bars)) {
        arrows(x0 = xbars, x1 = xbars, y0 = eb_lower, y1 = eb_upper, angle = 90, code = 3, length = 0.03, col = "#10133a")
      }
    }  # end if barplot
    
    
    if (plot.type == "boxplot") {
      if(is.na(error.bars)) vi <- t(as.data.frame(abs(varimp))) else vi <- varimps  # ifelse makes single boxplot for all vars
      
      boxplot(vi,
              col = adjustcolor(colrs, alpha.f = 0.2),
              border = colrs,
              ylab = ylab,
              ylim = ylim,
              las = 2,
              ...)
      
      if (grid) grid()
    }  # end if boxplot
    
    if (plot.points && (is_bart || is_flexbart)) {
      if (plot.type == "barplot") xx <- rep(xbars, each = nrow(varimps))
      else xx <- rep(1:ncol(varimps), each = nrow(varimps))
      jj <- sapply(xx, jitter, amount = 0.1)
      points(x = jj, y = as.matrix(varimps), pch = 20, cex = 0.1, col = adjustcolor("#ffaabb", alpha.f = 0.3))
      if (plot.type == "lollipop") {
        points(abs(varimp), pch = 20, col = colrs)
        arrows(x0 = 1:length(varimp), x1 = 1:length(varimp), y0 = eb_lower, y1 = eb_upper, code = 3, angle = 90, length = 0.03, col = colrs)
      }  # re-plot on top of points for better visibility
      if (plot.type == "barplot") {
        arrows(x0 = xbars, x1 = xbars, y0 = eb_lower, y1 = eb_upper, angle = 90, code = 3, length = 0.03, col = "#10133a")
      }  # re-plot on top of points for better visibility
    }  # end if plot.points
    
    signs <- unique(sign(varimp)[sign(varimp) != 0])  # check for both negative and positive varimps
    if (legend && length(signs) > 1) legend("topright", legend = c("positive", "negative"), fill = col, border = NA, bty = "n")
  }  # end if plot
  
  if (is.na(error.bars)) return(abs(varimp))
  
  return (data.frame(Mean = varimp, Lower = eb_lower, Upper = eb_upper))
}
