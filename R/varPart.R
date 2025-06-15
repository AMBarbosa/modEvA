varPart <-
function(A, B, C = NA, AB, AC = NA, BC = NA, ABC = NA, model.type = NULL, 
         A.name = "Factor A", B.name = "Factor B", C.name = "Factor C", 
         model = NULL, groups = NULL, pred.type = "Y", cor.method = "pearson", 
         return.models = FALSE, plot = TRUE, plot.digits = 3, cex.names = 1.5, 
         cex.values = 1.2, main = "", cex.main = 2, plot.unexpl = TRUE, 
         colr = FALSE) {
  
  # version 2.1 (21 Jul 2022)
  
  if (!is.null(model.type)) message ("NOTE: Argument 'model.type' is no longer used.")
  
  if (!is.null(model)) {
    if (!missing(A) || !missing(B)) warning("Arguments 'A', 'B', etc. are ignored as argument 'model' is provided.")
    if (!(inherits(model, "glm"))) stop("Argument 'model' is currently only implemented for class 'glm'.")
    if (!(pred.type %in% c("Y", "P", "F"))) stop ("Invalid 'pred.type' argument.")
    if (pred.type == "F" && family(model)$family != "binomial") stop ("pred.type='F' is only applicable to binomial models.")
    
    vars <- names(model$coefficients)[-1]
    
    if (is.null(groups)) stop ("If you provide a 'model' argument, you must also provide 'groups'.")
    if (!all(vars %in% groups[ , 1])) stop ("All variables in 'model' must exist (with the same exact name) in the first column of 'groups'.")
    if (length(unique(groups[ , 2])) != length(unique(trimws(groups[ , 2])))) warning ("Some values in 'groups' have leading or trailing spaces and are treated separately; consider using trimws() first.")
    
    groups <- groups[groups[ , 1] %in% vars, ]
    if (length(unique(groups[ , 2])) > 3) stop ("All variables in 'model' must belong to a maximum of three different 'groups'.")
    
    factors <- unique(groups[ , 2])
    factors_ABC <- LETTERS[1:length(factors)]
    groups[ , 3] <- factors_ABC[match(groups[ , 2], factors)]
    
    c1 <- factors_ABC  # each individual factor
    if (length(factors) > 1) c2 <- as.data.frame(combn(factors_ABC, 2)) else c2 <- NA  # each combination of 2 factors
    if (length(factors) > 2) c3 <- as.data.frame(combn(factors_ABC, 3)) else c3 <- NA  # each combination of 3 factors
    
    combinations <- unname(c(c1, c2, c3))
    combinations <- Filter(Negate(anyNA), combinations)  # like na.omit for list
    
    mods <- vector("list", length(combinations))
    names(mods) <- sapply(combinations, paste, collapse = "")
    
    response <- names(model$model)[1]
    for (fac in 1:length(combinations)) {
      vars_fac <- groups[groups[ , 3] %in% unlist(combinations[fac]), 1]
      form <- as.formula(paste(response, "~", paste(vars_fac, collapse = "+")))
      mods[[fac]] <- glm(form, data = model$model, family = family(model)$family)
    }
    
    preds <- data.frame(matrix(nrow = nrow(model$model), ncol = 0))
    if (pred.type == "Y") type <- "link"  else type <- "response"
    for (m in names(mods)) {
      preds[ , m] <- predict(mods[[m]], type = type)
    }
    if (pred.type == "F") {
      n1 <- sum(model$y == 1)
      n0 <- sum(model$y == 0)
      preds <- as.data.frame(sapply(preds, function(p) (p/(1-p)) / ((n1/n0) + (p/(1-p)))))  # can't import from fuzzySim because fuzzySim imports modEvA
    }
    
    rsq <- rep(NA_real_, length(mods))
    names(rsq) <- names(mods)
    for (f in names(preds)) {
      # if (family(model)$family == "gaussian") linmod <- lm(model$model[ , 1] ~ preds[ , f])
      # else linmod <- lm(preds[ , ncol(preds)] ~ preds[ , f])
      # rsq[f] <- summary(linmod)$r.squared
      if (family(model)$family == "gaussian") corr <- cor(model$model[ , 1], preds[ , f], method = cor.method)
      else corr <- cor(preds[ , ncol(preds)], preds[ , f], method = cor.method)
      rsq[f] <- corr ^ 2
    }
  }
  
  if (is.null(model)) {
    partials <- c(A, B, C, AB, BC, AC, ABC) 
    if (!all(na.omit(partials) >= 0 & na.omit(partials) <= 1)) stop ("Values of A, B, etc. must be between 0 and 1.")
    if (all(is.finite(partials[c(1:2, 4)])) && all(is.na(partials[c(3, 5:7)])))  
      twofactors <- TRUE
    else if (all(is.finite(partials)))  
      twofactors <- FALSE
    else stop ("You must provide numeric values for either A, B and AB (for variation partitioning among two factors) or A, B, C, AB, BC, AC and ABC (for variation partitioning among three factors). See Details.")
  } else {
    if (length(rsq) <= 4) twofactors <- TRUE else twofactors <- FALSE
    A <- rsq["A"]
    B <- rsq["B"]
    C <- rsq["C"]
    AB <- rsq["AB"]
    AC <- rsq["AC"]
    BC <- rsq["BC"]
    ABC <- rsq["ABC"]
    A.name <- unique(groups[groups[ , 3] == "A", 2])
    B.name <- unique(groups[groups[ , 3] == "B", 2])
    C.name <- unique(groups[groups[ , 3] == "C", 2])
  }

  totalexpl <- ifelse(twofactors, AB, ABC)
  unexpl <- 1 - totalexpl

  if (twofactors) {
    Apure <- totalexpl - B
    Bpure <- totalexpl - A
    ABoverlap <- totalexpl - Apure - Bpure
    output.names <- c(A.name, B.name, paste(A.name, B.name, sep = "_"),
                      "Unexplained")
    results <- data.frame(c(Apure, Bpure, ABoverlap, unexpl),
                          row.names = output.names)

  } else { # end if 2 factors
  
    Apure <- totalexpl - BC
    Bpure <- totalexpl - AC
    Cpure <- totalexpl - AB
    ABoverlap <- totalexpl - Apure - Bpure - C
    BCoverlap <- totalexpl - Bpure - Cpure - A
    ACoverlap <- totalexpl - Apure - Cpure - B
    ABCoverlap <- totalexpl - Apure - Bpure - Cpure - ABoverlap - BCoverlap - ACoverlap
    output.names <- c(paste(A.name),
                      paste(B.name),
                      paste(C.name),
                      paste(A.name, B.name, sep = "_"),
                      paste(B.name, C.name, sep = "_"),
                      paste(A.name, C.name, sep = "_"),
                      paste(A.name, B.name, C.name, sep = "_"),
                      "Unexplained")
    results <- data.frame(c(Apure, Bpure, Cpure, ABoverlap, BCoverlap,
                            ACoverlap, ABCoverlap, unexpl),
                          row.names = output.names)
  }  # end else

  colnames(results) <- "Proportion"
  #n <- nrow(results)
  #if(model.type == "GLM")  results <- results[1:(n-1),]  # deletes "unexplained" line (data unavailable for GLM)

  if (plot) {  # adapted from Daniel's http://stackoverflow.com/questions/1428946/venn-diagrams-with-r

    circle <- function(x, y, r, col = NA) {
      ang <- seq(0, 2 * pi, length = 100)
      xx <- x + r * cos(ang)
      yy <- y + r * sin(ang)
      polygon(xx, yy, col = col)
    }  # end circle funtion (by Daniel)

    Apure <- round(Apure, plot.digits)  # shorten values for plotting
    Bpure <- round(Bpure, plot.digits)
    ABoverlap <- round(ABoverlap, plot.digits)
    if(!twofactors) {
      Cpure <- round(Cpure, plot.digits)
      BCoverlap <- round(BCoverlap, plot.digits)
      ACoverlap <- round(ACoverlap, plot.digits)
      ABCoverlap <- round(ABCoverlap, plot.digits)
    }

    if (twofactors) {
      plot(0, 0, ylim = c(-1, 10), xlim = c(-1, 10), type = "n", axes = FALSE,
           ylab = "", xlab = "", main = main, cex.main = cex.main)
      circle(4.5, 3, 3, col = ifelse(colr, rgb(1, 0, 0, 0.5), NA))
      circle(4.5, 6, 3, col = ifelse(colr, rgb(0, 1, 0, 0.5), NA))
      text(x = c(4.5, 4.5), y = c(9.5, -0.5), labels = c(A.name, B.name),
           cex = cex.names)
      text(x = c(4.5, 4.5, 4.5), y = c(7, 4.75, 2), c(Apure, ABoverlap, Bpure),
           cex = cex.values)
      
    } else { # end if 2 factors
    
      plot(0, 0, ylim = c(-1, 10), xlim = c(-1, 10), type = "n", axes = FALSE,
           ylab = "", xlab = "", main = main, cex.main = cex.main)
      circle(3, 6, 3, col = ifelse(colr, rgb(1, 0, 0, 0.5), NA))
      circle(6, 6, 3, col = ifelse(colr, rgb(0, 1, 0, 0.5), NA))
      circle(4.5, 3, 3, col = ifelse(colr, rgb(0, 0, 1, 0.5), NA))
      #Cname.loc = ifelse((plot.unexpl), 6, 4.5)
      text(x = c(2.5, 6.5, 4.5), y = c(9.5, 9.5, -0.5),
           labels = c(A.name, B.name, C.name), cex = cex.names, adj = c(0.5, 0.5, 0))
      text(x = c(1.8, 7.2, 4.5, 4.5, 2.8, 6.2, 4.5), y = c(6.6, 6.6, 2, 7, 4, 4, 5), labels = c(Apure, Bpure, Cpure, ABoverlap, ACoverlap, BCoverlap, ABCoverlap), cex = cex.values)
    } # end if 2 factors else
    
    if (plot.unexpl)  {
      rect(-1, -1, 10, 10)
      text(x = -0.9, y = -0.2, label = paste0("Unexplained\n", round(unexpl, plot.digits)), adj = 0, cex = cex.values)
    }
    
  }  # end if plot

  if (all.equal(sum(results, na.rm = TRUE), 1)) cat("")
  else warning ("Results don't sum up to 1; are you sure your input data are correct?")  # but this doesn't work because results always sum to 1 anyway
  
  if (!return.models) return(results)
  return(list(models = mods, varpart = results))
}
