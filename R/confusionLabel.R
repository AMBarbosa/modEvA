confusionLabel <- function(model = NULL, obs = NULL, pred = NULL, thresh, interval = 0.01, quant = 0, verbosity = 2, na.rm = FALSE, rm.dup = FALSE, plot = TRUE, ...) {
  # version 2.0 (29 Apr 2025)
  
  pred_in <- pred  # in case input is raster, so final reclass is also raster
  
  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity)
  obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]
  
  if (!(is.numeric(thresh) || thresh %in% modEvAmethods("getThreshold")))  stop("'thresh' must be either a numeric value between 0 and 1, or one of the options obtained with modEvAmethods('getThreshold')")
  if (thresh %in% modEvAmethods("getThreshold"))  thresh <- getThreshold(obs = obs, pred = pred, threshMethod = thresh, interval = interval, quant = quant, na.rm = na.rm)
  
  out_chr <- rep("", length(obs))
  out_chr[pred >= thresh & obs == 1] <- "TruePos"
  out_chr[pred < thresh & obs == 0] <- "TrueNeg"
  out_chr[pred >= thresh & obs == 0] <- "FalsePos"
  out_chr[pred < thresh & obs == 1] <- "FalseNeg"
  
  out_fac <- factor(out_chr, levels = c("TrueNeg", "FalseNeg", "FalsePos", "TruePos"))  # ordered levels, low to high
  
  if (!inherits(pred_in, "SpatRaster"))
    return(out_fac)
  
  
  finite_pixels <- which(is.finite(terra::values(pred_in)))
  out_rast <- pred_in
  # levs <- as.factor(out_chr)
  levs <- factor(out_chr, levels = c("TrueNeg", "FalseNeg", "FalsePos", "TruePos"))  # for ordered levels, low to high
  terra::values(out_rast)[finite_pixels] <- levs  # only 'finite_pixels' because out_chr vector is shorter when input has NAs
  
  # convert to categorical raster:
  levels(out_rast) <- data.frame(id = as.integer(unique(levs)), cat = unique(out_chr))
  
  # set colours for raster categories:
  # colr_table <- data.frame(lev = as.factor(c("TruePos", "FalsePos", "TrueNeg", "FalseNeg")), col = c("royalblue", "lightblue", "red", "orange"))
  # colr_table <- data.frame(lev = as.factor(c("TrueNeg", "FalseNeg", "FalsePos", "TruePos")), col = c("red", "orange", "lightblue", "royalblue"))
  # existing_levs <- colr_table$lev %in% levels(out_rast)[[1]]$cat
  # terra::coltab(out_rast) <- data.frame(
  #   values = droplevels(colr_table$lev[existing_levs]),
  #   cols = colr_table$col[existing_levs])
  
  if (plot) terra::plot(out_rast, ...)
  
  return(out_rast)
}
