RsqGLM <- function(model = NULL, obs = NULL, pred = NULL, use = "pairwise.complete.obs", plot = TRUE, plot.type = "lollipop", na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 3.0 (15 May 2025)

  warning("this function is DEPRECATED; using pseudoRsq() instead.\n")
  
  pseudoRsq(model = model, obs = obs, pred = pred, use = use, plot = plot, plot.type = plot.type, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity, ...)
}
