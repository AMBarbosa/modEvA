evenness <-
function (obs) {
  # version 1.3 (18 June 2013)
  values <- unique(obs)
  nvalues <- length(values)
  if (!(nvalues %in% c(1, 2))) stop("Input vector includes ", nvalues, " different values; 'evenness' is only implemented for binary observations (with 1 or 2 different values).")
  proportion <- (sum(obs == values[1])) / length(obs)
  if (proportion > 0.5)  balance <- 1 - proportion  else  balance <- proportion
  return(2 * balance)  # so evenness ranges between 0 and 1
}
