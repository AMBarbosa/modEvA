Boyce <- function(model = NULL, obs = NULL, pred = NULL, n.bins = NA, bin.width = "default", res = 100, method = "spearman", rm.dup.classes = FALSE, rm.dup.points = FALSE, pbg = FALSE, plot = TRUE, plot.lines = TRUE, plot.values = TRUE, plot.digits = 3, na.rm = TRUE, verbosity = 2, ...) {

  # version 1.7 (26 Dec 2024)
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup.points, pbg = pbg, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  if (all(obs == 0)) warning("No presences available, so there are no observed proportions of presences to compare with expected values.")
  if (all(obs == 1)) warning("All observations are presences, so the proportion of presences is constant across bins.")

  # to match the original 'ecospat::ecospat.boyce' arguments:
  fit <- pred
  obs <- pred[which(obs == 1)]

  # if (class(fit) == "RasterLayer") {
  #   if (class(obs) == "data.frame" || class(obs) == "matrix") {
  #     obs <- extract(fit, obs)
  #   }
  #   fit <- getValues(fit)
  #   fit <- fit[!is.na(fit)]
  # }

  if (inherits(fit, "SpatRaster")) {
    if (inherits(obs, "data.frame") || inherits(obs, "matrix") || inherits(obs, "SpatVector")) {
      obs <- terra::extract(fit, obs)
    } else {
      stop("When 'pred' is a 'SpatRaster', 'obs' must be either a 'SpatVector' of the presence points or a two-column matrix or data frame containing their x (longitude) and y (latitude) coordinates, respectively.")
    }
    fit <- na.omit(terra::values(fit))
  }

  # the remainder of the function is slightly modified from 'ecospat::ecospat.boyce':

  boycei <- function(interval, obs, fit) {
    pi <- sum(as.numeric(obs >= interval[1] & obs <= interval[2])) / length(obs)
    ni <- sum(as.numeric(fit >= interval[1] & fit <= interval[2]))  # my add
    ei <- ni / length(fit)
    #return(round(pi / ei, 10))  # my removal
    #return(rbind(boycei = round(pi / ei, 10), bin.N = ni))  # my add
    return(rbind(bin.N = ni, predicted = pi, expected = ei, boycei = round(pi / ei, 10)))  # my add
  }

  mini <- min(fit, obs)
  maxi <- max(fit, obs)
  if (length(n.bins) == 1) {
    if (is.na(n.bins)) {
      if (bin.width == "default") {
        bin.width <- (max(fit) - min(fit)) / 10
      }
      vec.mov <- seq(from = mini, to = maxi - bin.width, by = (maxi - mini - bin.width) / res)
      vec.mov[res + 1] <- vec.mov[res + 1] + 1
      interval <- cbind(vec.mov, vec.mov + bin.width)
    } else {
      vec.mov <- seq(from = mini, to = maxi, by = (maxi - mini) / n.bins)
      interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
    }
  } else {
    vec.mov <- c(mini, sort(n.bins[!n.bins > maxi | n.bins < mini]))
    interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
  }

  boycei.result <- t(apply(interval, 1, boycei, obs, fit))  # my add

  f <- boycei.result[ , 4]  # added '[,4]' as per my 'boycei' return modification

  to.keep <- which(!is.nan(f))  # changed from f!="NaN"
  f <- f[to.keep]

  if (length(f) < 2) {
    b <- NA
  } else {
    r <- 1:length(f)
    if (rm.dup.classes) {
      r <- c(1:length(f))[f != c(f[-1], TRUE)]
    }
    b <- cor(f[r], vec.mov[to.keep][r], method = method)
  }

  HS <- apply(interval, 1, sum) / 2
  if (length(n.bins) == 1 & is.na(n.bins)) {
    HS[length(HS)] <- HS[length(HS)] - 1
  }
  HS <- HS[to.keep]

  if (plot && length(f) > 0) {
    par_mgp <- par()$mgp
    on.exit(par(mgp = par_mgp))
    par(mgp = c(1.8, 0.7, 0))  # values and labels closer to axis

    plot(HS, f, ylim = c(0, max(f, na.rm = TRUE)), xlab = "Prediction class", ylab = "Predicted/expected ratio", col = "grey", cex = 0.5, ...)  # includes duplicate P/E values; 'ylim' was my add
    if (plot.lines) {  # my add
      lines(HS, f, col = "grey")
      lines(HS[r], f[r])
    }  # my add
    points(HS[r], f[r], pch = 19, cex = 0.4, col = "darkblue")  # without duplicate P/E values
    #abline(h = 1, lty = 5, col = "grey")  # my add

    bin.N <- boycei.result[to.keep, 1]  # my add
    small_bins <- which(bin.N < 30)  # my add
    if (length(small_bins) > 0) warning ("Some bins (plotted in red) have less than 30 values, so their result may not be meaningful (see 'bin.N' column in console output). Consider increasing 'bin.width'.")
    points(HS[r][small_bins], f[r][small_bins], pch = 17, cex = 0.7, col = "red")  # my add

    # if (plot.values) text(x = max(HS), y = diff(range(f)) / 10, paste("B =", round(b, plot.digits)), adj = 1)  # my add
    if (plot.values) text(x = max(HS), y = min(range(f)), paste("B =", round(b, plot.digits)), adj = c(1, 0))  # my add
    # if (plot.values) text(x = mean(HS), y = diff(range(f)) / 10, paste("B =", round(b, plot.digits)), adj = 0.5)  # my add
  }

  # the following is different from 'ecospat.boyce':
  return(list(bins = data.frame(bin.N = boycei.result[to.keep, 1],
                                bin.min = interval[to.keep, 1],
                                bin.max = interval[to.keep, 2],
                                bin.median = HS,
                                predicted = boycei.result[to.keep, 2],
                                expected = boycei.result[to.keep, 3],
                                PE.ratio = f),
              Boyce = b))
}
