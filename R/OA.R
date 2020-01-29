OA <-
function(data, sp.cols, var.cols) {
  # version 2.0 (29 Apr 2014)

  if (length(sp.cols) > 1) stop ("Sorry, OA is currently implemented for only one response variable at a time, so 'sp.cols' must indicate only one column")

  input.data <- data
  
  #data <- data.frame(data, 1:nrow(data))
  #data.noNA <- na.omit(data[ , c(sp.cols, var.cols, ncol(data))])
  #noNA <- data.noNA[ , ncol(data.noNA)]
  #data <- data[data[ , ncol(data)] %in% noNA, ]
  
  na.rm = TRUE
  if(na.rm) {
    mod.data <- data[ , c(sp.cols, var.cols)]
    data <- data[complete.cases(mod.data), ]
  }
  
  predictors <- data[ , var.cols]
  nvar <- ncol(predictors)
  
  response <- data[ , sp.cols]
  predictors.presence <- predictors[response > 0, ]
  var.presmin <- var.presmax <- vector("numeric", nvar)
  predictors.overlap <- matrix(data = NA, nrow = nrow(predictors), ncol = nvar)
  for (v in 1:nvar) {
    var.presmin[v] <- min(predictors.presence[ , v])
    var.presmax[v] <- max(predictors.presence[ , v])
    predictors.overlap[ , v] <- ifelse((predictors[ , v] >= var.presmin[v] & predictors[ , v] <= var.presmax[v]), 1, 0)
  }  # end for v
  overlap.noNA <- as.integer(rowSums(predictors.overlap) == nvar)
  return(overlap.noNA)
}
