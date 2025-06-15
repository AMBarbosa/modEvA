getModEqn <-
function(model, type = "Y", digits = NULL, prefix = NULL, suffix = NULL) {
  # version 1.7 (4 Apr 2022)

  stopifnot(class(model) %in% c("lm", "glm"))
  
  if (length(type) != 1 | !(type %in% c("Y", "P", "F"))) stop("'type' must be either 'Y', 'P', or 'F'")
  if(!("glm" %in% class(model)) && type != "Y") {
    message("types 'P' and 'F' are only applicable to models of class 'glm', so type was reset to 'Y'")
    type <- "Y"
  }
  coeffs <- summary(model)$coefficients[ , 1]
  if (type == "F" & !("(Intercept)") %in% names(coeffs)) {
    message("'F' is currently only applicable to models with an intercept, so type was set to 'P'")
    type <- "P"
  }
  if (type == "F") {
    n1 <- sum(model$y == 1)
    n0 <- sum(model$y == 0)
    coeffs["(Intercept)"] <- coeffs["(Intercept)"] - log(n1/n0)
  }
  names(coeffs) <- paste(prefix, names(coeffs), suffix, sep = "")
  if (!is.null(digits)) coeffs <- signif(coeffs, digits)  # was previously 'round'
  coeffs <- ifelse(coeffs < 0, coeffs, paste("+", coeffs, sep = ""))
  multips <- paste(coeffs, names(coeffs), sep = "*")
  multips <- sub(x = multips, pattern = paste(prefix, "*(Intercept)", suffix, sep = ""), replacement = "", fixed = TRUE)
  eqn <- apply(X = as.matrix(multips), MARGIN = 2, FUN = paste, collapse = "")
  if (substring(eqn, 1, 1) == "+")  eqn <- substring(eqn, 2)
  if (type == "Y")  eqn <- paste("Y=", eqn, sep = "")
  if (type == "P")  eqn <- paste("P=1-(1/(1+exp(", eqn, ")))", sep = "")
  if (type == "F")  eqn <- paste("F=1-(1/(1+exp(", eqn, ")))", sep = "")
  cat(eqn)
  return(invisible(eqn))
}
