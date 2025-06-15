confusionMatrix <- function(model = NULL, obs = NULL, pred = NULL, thresh, interval = 0.01, quant = 0, verbosity = 2, na.rm = TRUE, rm.dup = FALSE, plot = FALSE, classes = FALSE, ...) {
  # version 1.3 (19 Nov 2024)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))
    stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)

  obs0 <- obs == 0
  obs1 <- obs == 1
  pred0 <- pred < thresh
  pred1 <- pred >= thresh
  a <- sum(obs1 & pred1, na.rm = na.rm)
  b <- sum(obs0 & pred1, na.rm = na.rm)
  c <- sum(obs1 & pred0, na.rm = na.rm)
  d <- sum(obs0 & pred0, na.rm = na.rm)

  out <- data.frame(obs1 = c(a, c), obs0 = c(b, d))
  rownames(out) <- c("pred1", "pred0")

  if (plot == TRUE) {

    rotate <- function(x) t(apply(x, 2, rev))  # because image() rotates the matrix, as per its help file; 'rotate' function obtained from https://stackoverflow.com/a/16497058/3447652

    if (classes) {
      graphics::image(x = 1:2, y = 1:2,
                      # z = matrix(1:4, ncol = 2, byrow = TRUE),
                      # col = c("orange", "royalblue", "red", "lightblue"),
                      z = rotate(matrix(1:4, ncol = 2, byrow = TRUE)),
                      col = c("royalblue", "lightblue", "orange", "red"),
                      axes = FALSE, xlab = "", ylab = "", ...)
    } else {
      graphics::image(x = 1:2, y = 1:2,
                      z = rotate(as.matrix(out)),
                      axes = FALSE, xlab = "", ylab = "", ...)
    }  # end if classes else

    text(x = c(1, 1, 2, 2), y = c(2, 1, 2, 1), labels = as.matrix(out))
    text(0.25, 1, "pred0", xpd = NA)
    text(0.25, 2, "pred1", xpd = NA)
    text(1, 0.25, "obs1", xpd = NA)
    text(2, 0.25, "obs0", xpd = NA)
  }  # end if plot

  out
}
