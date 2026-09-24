MillerCalib <- function(model = NULL, obs = NULL, pred = NULL, plot = TRUE, line.col = "darkblue", diag = TRUE, diag.col = "lightblue3", plot.values = TRUE, digits = 2, xlab = "", ylab = "", main = "Miller calibration", na.rm = TRUE, rm.dup = FALSE, verbosity = 2, ...) {
  # version 2.1 (3 Jul 2025)

  warning("this function is DEPRECATED; using Miller() instead.\nMind that newer Miller() arguments may be ignored here.")
  
  Miller(model = model, obs = obs, pred = pred, plot = plot, line.col = line.col, diag = diag, diag.col = diag.col, plot.values = plot.values, digits = digits, xlab = xlab, ylab = ylab, main = main, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity, ...)
}
