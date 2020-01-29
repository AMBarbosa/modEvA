.modEvAmethods <-
function(arg) {
  if (arg %in% c("threshMeasures", "multModEv")) {
    thresh.measures <- c("CCR", "Misclass", "Sensitivity", "Specificity", "Omission", "Commission", "Precision", "Recall", "PPP", "NPP", "UPR", "OPR", "PPI", "PAI", "kappa", "TSS", "NMI", "F1score", "OddsRatio")
    if (arg == "threshMeasures") return(thresh.measures)
    else if (arg == "multModEv") return (c("Preval", "AUC", "MeanPrecision", "AUCPR", thresh.measures))
  }
  else if (arg == "bin") return(c("round.prob", "prob.bins", "size.bins", "n.bins", "quantiles"))
  else if (arg == "threshold") return(c("each", "preval", "0.5", "maxKappa", "minSensSpecDiff", "maxSensSpecSum", "maxTSS"))
}
