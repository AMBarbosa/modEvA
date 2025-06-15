MESS <- function(V, P, id.col = NULL, verbosity = 2){
  # version 1.6, 8 Sep 2021

  # if (inherits(V, "SpatRaster")) V <- terra::values(V)
  # if (inherits(P, "SpatRaster")) P <- terra::values(P)

  index.V <- 1:nrow(V)
  index.P <- 1:nrow(P)

  if (is.null(id.col)) {
    if (ncol(V) != ncol(P))  stop ("The number of variables in V and P does not match.")
  } else {  # if id.col
    if (ncol(V) != ncol(P) - 1)  stop ("'id.col' should refer to a column in P; P should therefore have one more column than V.")
    P.input <- P
    P <- P[ , -id.col]
  }

  n.vars <- ncol(V)
  n.varP <- ncol(P)
  nrow.P <- nrow(P)
  results <- matrix(nrow = nrow.P, ncol = n.vars, dimnames = list(NULL, colnames(P)))

  for (i in 1:n.vars){
    if (verbosity > 0) message("Comparing variable ", i, " of ", n.vars, "...")
    min.Vi <- min(V[, i], na.rm = TRUE)
    max.Vi <- max(V[, i], na.rm = TRUE)
    SIM <- vector("numeric", nrow.P)
    for (j in 1:nrow.P){
      VV <- V[, i]
      #VV[VV < P[j, i]] <- 1
      #VV[VV >= P[j, i]] <- 0
      VV <- ifelse(VV < P[j, i], 1, 0)  # bug fix by Huijie Qiao
      Fj <- sum(VV, na.rm = TRUE) * 100 / length(VV)
      if (Fj == 0)  SIM[j] <- (P[j, i] - min.Vi) / (max.Vi - min.Vi) * 100
      else if (Fj > 0 && Fj <= 50)  SIM[j] <- 2 * Fj
      else if (Fj > 50 && Fj < 100)  SIM[j] <- 2 * (100 - Fj)
      else if (Fj == 100)  SIM[j] <- (max.Vi - P[j, i]) / (max.Vi - min.Vi) * 100
    }
    results[, i] <- SIM
  }

  if (verbosity > 0) message("Calculating MESS and MoD...")
  results <- data.frame(results)
  results$TOTAL <- apply(results[ , 1:n.vars], 1, min)
  results$MoD <- as.factor(colnames(results)[apply(results[ , 1:n.vars], 1, which.min)])
  if (!is.null(id.col)) {
    results <- data.frame(P.input[ , id.col], results)
    colnames(results)[1] <- colnames(P.input)[id.col]
  }
  if (verbosity > 0) message("Finished!")
  return(results)
}
