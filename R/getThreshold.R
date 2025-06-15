getThreshold <- function(model = NULL, obs = NULL, pred = NULL, threshMethod, interval = 0.01, quant = 0, na.rm = TRUE, verbosity = 2, pbg = FALSE) {

  # version 1.4 (19 Nov 2024)

  stopifnot(length(threshMethod) == 1)

  obspred <- inputMunch(model, obs, pred, na.rm = na.rm, verbosity = verbosity, pbg = pbg)
  if (!is.null(obs) || !is.null(model)) obs <- obspred[ , "obs"]
  pred <- obspred[ , "pred"]

  if (is.null(obs) && !(threshMethod %in% c("meanPred", "midPoint"))) stop ("'obs' must be provided for the specified threshold method.")


  # thresholds in Liu et al. (2005, 2013):

  if (threshMethod == "preval" || threshMethod == "trainPrev")  thresh <- prevalence(obs, na.rm = na.rm)
  else if (threshMethod == "meanPred")  thresh <- mean(pred, na.rm = na.rm)
  else if (threshMethod == "midPoint")  thresh <- median(pred, na.rm = na.rm)

  else if (threshMethod == "maxKappa")  thresh <- optiThresh(obs = obs, pred = pred, measures = "kappa", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold
  else if (threshMethod == "maxCCR" || threshMethod == "maxOA" || threshMethod == "maxOPS")  thresh <- optiThresh(obs = obs, pred = pred, measures = "CCR", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold
  else if (threshMethod == "maxF")  thresh <- optiThresh(obs = obs, pred = pred, measures = "F1score", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold
  else if (threshMethod == "maxSSS")  thresh <- optiPair(obs = obs, pred = pred, measures = c("Sensitivity", "Specificity"), interval = interval, plot = FALSE, na.rm = na.rm, exclude.zeros = TRUE) $ ThreshSum
  else if (threshMethod == "minDSS")  thresh <- optiPair(obs = obs, pred = pred, measures = c("Sensitivity", "Specificity"), interval = interval, plot = FALSE, na.rm = na.rm, exclude.zeros = TRUE) $ ThreshDiff
  else if (threshMethod == "minDPR")  thresh <- optiPair(obs = obs, pred = pred, measures = c("Precision", "Recall"), interval = interval, plot = FALSE, na.rm = na.rm, exclude.zeros = TRUE) $ ThreshDiff
  else if (threshMethod == "minD01")  stop("Sorry, ", threshMethod, " criterion is not yet fully implemented... Please choose another option.")  # m ROC Dis = (Sp-1)^2+(1-Se)^2; https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/html/ROC.html?revision=2&root=diagnosismed&pathrev=2
  else if (threshMethod == "minD11")  stop("Sorry, ", threshMethod, " criterion is not yet fully implemented... Please choose another option.")
  else if (threshMethod == "equalPrev")  stop("Sorry, ", threshMethod, " criterion is not yet fully implemented... Please choose another option.")


  # additional thresholds:

  else if (threshMethod == "maxTSS")  thresh <- optiThresh(obs = obs, pred = pred, measures = "TSS", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold
  else if (threshMethod == "maxSPR")  thresh <- optiPair(obs = obs, pred = pred, measures = c("Precision", "Recall"), interval = interval, plot = FALSE, na.rm = na.rm, exclude.zeros = TRUE) $ ThreshSum
  else if (threshMethod == "MTP")  thresh <- quantile(pred[obs == 1], probs = quant, na.rm = na.rm)
  else if (threshMethod == "maxJaccard")  thresh <- optiThresh(obs = obs, pred = pred, measures = "Jaccard", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold
  else if (threshMethod == "maxSorensen")  thresh <- optiThresh(obs = obs, pred = pred, measures = "Sorensen", optimize = "each", interval = interval, simplif = FALSE, plot = FALSE) $ optimals.each $ threshold


  else stop ("Invalid 'threshMethod'. Run modEvAmethods('getThreshold') for available (case-sensitive) options.")

  return(thresh)
}
