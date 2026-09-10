getBins <- function (model = NULL, obs = NULL, pred = NULL, id = NULL,
                     bin.method, 
                     n.bins = ifelse(bin.method != "mov.bins", 10, 100), 
                     fixed.bin.size = FALSE, min.bin.size = 15, 
                     min.prob.interval = 0.1, bin.width = "default", 
                     quantile.type = 7, simplif = FALSE,
                     verbosity = 2, na.rm = TRUE, rm.dup = FALSE)  {
  
  # version 3.1 (8 Apr 2026)
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)  
  if ("obs" %in% names(obspred)) obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  stopifnot(is.null(obs) || length(obs) == length(pred), 
            # !(NA %in% obs), 
            # !(NA %in% pred), 
            obs %in% c(0, 1), 
            #pred >= 0, 
            #pred <= 1, 
            is.null(id) | length(id) == length(pred), 
            # n.bins >= 2, 
            min.bin.size >= 0, 
            min.prob.interval > 0, 
            min.prob.interval < 1)
  
  if (!(bin.method %in% modEvAmethods("getBins")))
    stop("Invalid bin.method; type modEvAmethods('getBins') for available options.")
  
  N <- length(obs)
  bin.method0 <- bin.method
  
  
  if (bin.method == "mov.bins") {
    # using some code from 'ecospat::ecospat.boyce', with corrections after https://github.com/ecospat/ecospat/issues/99 and https://github.com/plantarum/ecospat/commit/0c542d51e074d570cb7fb14e4b6dac8b6c2dc432:
    min.pred <- min(pred)
    max.pred <- max(pred)
    if (length(n.bins) == 1) {
      if (is.na(n.bins)) {
        if (bin.width == "default") {
          bin.width <- (max.pred - min.pred) / 10
        }
        vec.mov <- seq(from = min.pred, to = max.pred - bin.width, length = n.bins)
        bins <- cbind(vec.mov, vec.mov + bin.width)
      } else {
        vec.mov <- seq(from = min.pred, to = max.pred, length.out = n.bins)
        # bins <- cbind(vec.mov, c(vec.mov[-1], max.pred))
        # vec.mov <- seq(from = min.pred, to = max.pred, length.out = n.bins + 1)
        bins <- data.frame(vec.mov[-length(vec.mov)], vec.mov[-1])
      }
    } else {
      vec.mov <- c(min.pred, sort(n.bins[!n.bins > max.pred | n.bins < min.pred]))
      bins <- cbind(vec.mov, c(vec.mov[-1], max.pred))
    }
    bins <- as.data.frame(bins)
    names(bins) <- c("min", "max")
    
    in_bin <- lapply(seq_len(nrow(bins)), function(i) {
      pred >= bins$min[i] & pred <= bins$max[i]
    })  # index of pred values falling within each bin
    
    bins$N <- sapply(in_bin, function(idx) sum(idx))
    if ("obs" %in% names(obspred)) {
      bins$N.pres <- sapply(in_bin, function(idx) sum(obs[idx] == 1))
      bins$N.abs <- sapply(in_bin, function(idx) sum(obs[idx] == 0))
      bins$prevalence <- bins$N.pres / bins$N
    } else {
      bins$N.pres <- bins$N.abs <- bins$prevalence <- NA
    }
    bins$mean.prob <- sapply(in_bin, function(idx) mean(pred[idx]))
    bins$median.prob <- sapply(in_bin, function(idx) median(pred[idx]))
    if ("obs" %in% names(obspred)) {
      bins$difference <- bins$mean.prob - bins$prevalence
    }
    bins$max.poss.diff <- ifelse(bins$mean.prob < 0.5, 1 - bins$mean.prob, bins$mean.prob)
    if ("obs" %in% names(obspred)) {
      bins$adj.diff <- abs(bins$difference - bins$max.poss.diff)
      bins$overpred <- apply(bins[, c("prevalence", "mean.prob")], MARGIN = 1, max)
      bins$underpred <- apply(bins[, c("prevalence", "mean.prob")], MARGIN = 1, min)
    }
    
    breaks <- c(bins$min[1], bins$max)
    prob.bin <- cut(pred, breaks = breaks)
    
    return(list(prob.bin = prob.bin, bins.table = bins, N = N, n.bins = nrow(bins)))
  }  # end if mov.bins
  
  else  if (bin.method == "round.prob" ) {
    if (verbosity > 1) message("NOTE: Arguments n.bins, fixed.bin.size and min.bin.size are ignored by this bin.method.")
    prob.bin <- round(pred, digits = nchar(min.prob.interval) - 2)
  }
  
  else if (bin.method == "prob.bins") {
    if (verbosity > 1) message("NOTE: Arguments n.bins, fixed.bin.size and min.bin.size are ignored by this bin.method.")
    bin.cuts <- seq(from = min(0, min(pred, na.rm = na.rm), na.rm = na.rm), to = max(1, max(pred, na.rm = na.rm), na.rm = na.rm), by = min.prob.interval)
    prob.bin <- findInterval(pred, bin.cuts)
  }
  
  else if (bin.method == "size.bins") {
    if (verbosity > 1) message("NOTE: Arguments n.bins and min.prob.interval are ignored by this bin.method.")
    bin.method <- "n.bins"
    n.bins <- floor(N / min.bin.size)
    fixed.bin.size <- TRUE
  }
  
  if (bin.method == "n.bins") {  # can't have 'else' here because of previous 'if'
    if (verbosity > 1 && bin.method0 != "size.bins") message("NOTE: Arguments min.bin.size and min.prob.interval are ignored by this bin.method.")
    if (fixed.bin.size) {
      #prob.bin <- findInterval(pred, quantile(pred, probs = seq(from = 0, to = 1, by = 1 / (n.bins - 1))))
      prob.bin <- cut(seq_along(pred), n.bins)  # same if sort(pred)
    } else {
      prob.bin <- cut(pred, n.bins)
    }
  }  # end if n.bins
  
  else if (bin.method == "quantiles") {
    #cutpoints <- quantile(pred, probs = seq(0, 1, by = 1/n.bins))
    #prob.bin <- findInterval(pred, cutpoints)
    if (verbosity > 1) message("NOTE: Arguments fixed.bin.size, min.bin.size and min.prob.interval are ignored by this bin.method.")
    #cutpoints <- quantile(pred, probs = seq(min.prob.interval, 1, by = min.prob.interval))
    #prob.bin <- cut(pred, breaks = length(cutpoints), include.lowest = TRUE, dig.lab = nchar(min.prob.interval) - 2)
    #cutpoints <- quantile(pred, probs = seq(0, 1, length = n.bins), type = quantile.type)
    cutpoints <- quantile(pred, probs = (0:n.bins)/n.bins, type = quantile.type, na.rm = na.rm)
    #prob.bin <- cut(pred, cutpoints, include.lowest = TRUE)
    prob.bin <- findInterval(pred, cutpoints, rightmost.closed = TRUE)
  }
  
  if (is.factor(prob.bin))  prob.bins <- levels(prob.bin) else prob.bins <- sort(unique(prob.bin))  # avoid missing empty bins
  bins.table <- t(as.data.frame(unclass(table(obs, prob.bin))))
  bins.table <- data.frame(rowSums(bins.table), bins.table[, c(2, 1)])
  colnames(bins.table) <- c("N", "N.pres", "N.abs")
  
  bins.table$prevalence <- with(bins.table, N.pres/N)
  bins.table$mean.prob <- tapply(pred, prob.bin, mean)
  bins.table$median.prob <- tapply(pred, prob.bin, median)
  bins.table$difference <- with(bins.table, mean.prob - prevalence)
  bins.table$max.poss.diff <- ifelse(bins.table$mean.prob < 0.5,
                                     1 - bins.table$mean.prob,
                                     bins.table$mean.prob)
  bins.table$adj.diff <- with(bins.table, abs(difference - max.poss.diff))
  bins.table$overpred <- apply(bins.table[, c("prevalence", "mean.prob")], 
                               MARGIN = 1, max)
  bins.table$underpred <- apply(bins.table[, c("prevalence", "mean.prob")], 
                                MARGIN = 1, min)
  # bins.table <- bins.table[bins.table$N > 0, ]  # could cause errors
  
  if (min(as.data.frame(bins.table)$N) < 15)
    if (verbosity > 0) warning("There is at least one bin with fewer than 15 values, where proportion comparisons may not be meaningful; consider using another 'bin.method', e.g. one that allows defining a minimum bin size")
  
  n.bins <- nrow(bins.table)
  list(prob.bin = prob.bin, bins.table = bins.table, N = N, n.bins = n.bins)
}
