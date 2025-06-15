lollipop <- function(x, names = NULL, ymin = 0, ylim = "auto0", sticks = TRUE, col = "royalblue4", grid = TRUE, cex = 1, cex.axis = 1, las = 2, horiz = FALSE, bold = FALSE, ...) {
  # version 2.2 (10 Feb 2025)
  
  if (is.matrix(sticks))
    sticks <- as.data.frame(sticks)
  if (!(is.logical(sticks) || inherits(sticks, "data.frame")))
    stop ("Invalid 'sticks'.")
  
  if (is.na(ymin))
    ymin <- min(x, na.rm = TRUE)
  if (length(ylim) == 1 && is.na(ylim)) {
    ylim <- range(x, na.rm = TRUE)
  }
  if (length(grep("auto", ylim)) > 0) {
    ymin <- min(x, na.rm = TRUE)
    if ("auto0" %in% ylim)
      ymin <- min(0, ymin)
    ylim <- c(ymin, max(x, na.rm = TRUE))
  }
  if (inherits(sticks, "data.frame")) {
    ylim <- range(c(ylim, sticks), na.rm = TRUE)
  }
  
  par_mgp <- par()$mgp
  on.exit(par(mgp = par_mgp))
  par(mgp = c(1.9, 0.7, 0))  # values and labels closer to axis

  # args <- as.list(match.call(expand.dots = TRUE))
  # axis.lab <- c(args$xlab, args$ylab)
  # if (is.null(axis.lab)) axis.lab <- deparse(substitute(x))
  # args$xlab <- args$ylab <- NULL
  
  plot.args <- list(...)  # https://www.r-bloggers.com/2020/11/some-notes-when-using-dot-dot-dot-in-r/
  if (!("xlab" %in% names(plot.args))) {
    plot.args$xlab <- ifelse(horiz, deparse(substitute(x)), "")
  }
  if (!("ylab" %in% names(plot.args))) {
    plot.args$ylab <- ifelse(horiz, "", deparse(substitute(x)))
  }
  
  if (isTRUE(horiz)) {
    do.call(plot, c(list(x = c(0, max(x, na.rm = TRUE)), 
                         y = if (length(x) == 2) c(0.5, 2.5) else c(1, length(x)), 
                         axes = FALSE, type = "n", xlim = ylim),
            plot.args))
    if (is.null(names)) names <- names(x)
    if (grid) grid()  # draw grid before lollipops
    axis(2, at = 1:length(x), labels = FALSE, las = las, cex.axis = cex.axis)
    axis(1, at = pretty(ylim), labels = pretty(ylim), cex.axis = cex.axis)
    points(x, 1:length(x), pch = 20, col = col, cex = cex)
    if (is.logical(sticks) && sticks) {
      arrows(x0 = 0, x1 = x, y0 = 1:length(x), y1 = 1:length(x), length = 0, col = col, lwd = cex)
    } else if (is.matrix(sticks) || is.data.frame(sticks)) {
      arrows(x0 = sticks[, 1], x1 = sticks[, 2], y0 = 1:length(x), y1 = 1:length(x), code = 3, length = 0, col = col, lwd = cex)
    }
    mtext(side = 2, at = 1:length(x), text = names, las = las, font = ifelse(bold, 2, 1), cex = cex.axis, line = 1)
    
  } else {  # if !horiz
    do.call(plot, c(list(x = if (length(x) == 2) c(0.5, 2.5) else c(1, length(x)),
                        y = ylim, axes = FALSE, type = "n", ylim = ylim), 
                 plot.args))
    if (is.null(names)) names <- names(x)
    if (grid) grid()  # draw grid before lollipops
    axis(1, at = 1:length(x), labels = FALSE, las = las, cex.axis = cex.axis)
    axis(2, at = pretty(ylim), labels = pretty(ylim), cex.axis = cex.axis)
    points(1:length(x), x, pch = 20, col = col, cex = cex)
    if (isTRUE(sticks)) {
      arrows(x0 = 1:length(x), x1 = 1:length(x), y0 = 0, y1 = x, length = 0, col = col, lwd = cex)
    } else if (inherits(sticks, "data.frame")) {
      arrows(x0 = 1:length(x), x1 = 1:length(x), y0 = sticks[, 1], y1 = sticks[, 2], code = 3, length = 0, col = col, lwd = cex)
    }
    mtext(side = 1, at = 1:length(x), text = names, las = las, font = ifelse(bold, 2, 1), cex = cex.axis, line = 1)
  }  # end if !horiz
}
