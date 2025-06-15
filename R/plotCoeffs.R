plotCoeffs <- function(model, labels = NULL, plot = TRUE, ...) {
  
  smry <- summary(model)$coefficients
  intercept <- grep("intercept", tolower(rownames(smry)))
  if (length(intercept) > 0)
    smry <- smry[-intercept, , drop = FALSE]
  
  if (nrow(smry) == 0) {
    args <- as.list(match.call(expand.dots = TRUE))  # [-1] removes the function name from the list
    plot.args <- names(formals(plot.default))
    args <- args[names(args) %in% plot.args]  # to exclude arguments for lollipop not base plot
    args$xlab <- args$ylab <- ""
    args$axes <- FALSE
    # plot(0:1, 0:1, type = "n", axes = FALSE, ...)
    do.call(plot, c(list(0:1, 0:1), type = "n", args))
    text(x = 0.5, y = 0.5, "No variables\nin model.")
    box()
    # message(deparse(substitute(model)), " has no coefficients apart from the intercept;\nno plot produced.")
    return(NULL)
  }
  
  # the following may not work correctly for all model summaries!
  # tested with classes glm, lm, phylolm (as per help file)
  coef_column <- grep("estimate", tolower(colnames(smry)))
  se_column <- grep("err", tolower(colnames(smry)))
  pval_column <- which(startsWith(tolower(colnames(smry)), "p"))
  
  Coefficients <- smry[ , coef_column, drop = FALSE]
  se <- smry[ , se_column, drop = FALSE]
  lowerci <- Coefficients - 1.96 * se
  upperci <- Coefficients + 1.96 * se
  pvals <- smry[ , pval_column, drop = FALSE]
  
  
  if (is.null(labels)) {
    lollipop_names <- rownames(Coefficients)
  } else {
    matches <- match(rownames(Coefficients), labels[ , 1])
    lollipop_names <- labels[matches, 2]
    lollipop_names[is.na(lollipop_names)] <- rownames(Coefficients)[is.na(lollipop_names)]
  }  # end if labels
  
  lollipop(Coefficients, 
           names = lollipop_names, 
           sticks = cbind(lowerci, upperci), 
           bold = ifelse(pvals <= 0.05, TRUE, FALSE), 
           ...)
  box()
  
  out <- data.frame(Variable = rownames(Coefficients),
                    Coefficient = Coefficients[,1], 
                    StdError = se[,1], 
                    LowerCI = lowerci[,1], 
                    UpperCI = upperci[,1], 
                    Pvalue = pvals[,1],
                    row.names = NULL)
  
  if(!is.null(labels)) {
    out <- data.frame(out[ , 1, drop = FALSE], 
                      Label = lollipop_names, 
                      out[ , 2:ncol(out)])
  }
  
  return(out)
}
