arrangePlots <-
function(n.plots, landscape = FALSE) {
  # version 1.1 (14 Dec 2023)
  # used internally by optiThresh
  if (n.plots <= 1) return(c(1, 1))
  root <- sqrt(n.plots)
  large <- ceiling(root)
  small <- round(root)
  if (landscape) plots.rc <- c(small, large)
  else plots.rc <- c(large, small)
  return(plots.rc)
}
