arrangePlots <-
function(n.plots, landscape = FALSE) {
  # version 1.0 (16 Sep 2013)
  # used internally by optiThresh
  root <- sqrt(n.plots)
  large <- ceiling(root)
  small <- round(root)
  if (landscape) plots.rc <- c(small, large)
  else plots.rc <- c(large, small)
  return(plots.rc)
}
