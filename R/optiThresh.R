optiThresh <- function(model = NULL, obs = NULL, pred = NULL, interval = 0.01, measures = c(modEvAmethods("threshMeasures"), modEvAmethods("similarity")), optimize = modEvAmethods("optiThresh"), simplif = FALSE, pbg = FALSE, plot = TRUE, sep.plots = FALSE, reset.par = TRUE, xlab = "Threshold", na.rm = TRUE, rm.dup = FALSE, pch = 20, cex = 0.2, col = "darkblue", verbosity = 2, ...) {
    # version 3.7 (26 Dec 2024)

    wrong.measures <- measures[which(!(measures %in% c(modEvAmethods("threshMeasures"), modEvAmethods("similarity"))))]
    wrong.optimizers <- optimize[which(!(optimize %in% modEvAmethods("optiThresh")))]
    if (length(wrong.measures) > 0) {
      warning("'", paste(wrong.measures, collapse = ", "), "'", " invalid under 'measures'; see modEvAmethods('threshMeasures') for available options.")
      measures <- measures[!(measures %in% wrong.measures)]
    }
    if (length(wrong.optimizers) > 0) {
      warning("'", paste(wrong.optimizers, collapse = ", "), "'", " invalid under 'optimize'; see modEvAmethods('optiThresh') for available options.")
      optimize <- optimize[!(optimize %in% wrong.optimizers)]
    }

    obspred <- inputMunch(model, obs, pred, na.rm = na.rm, rm.dup = rm.dup, verbosity = verbosity, pbg = pbg)
    obs <- obspred[ , "obs"]
    pred <- obspred[ , "pred"]

    if (all(obs == 0)) message ("No presences available, so can't compute measures that require presences.")
    if (all(obs == 1)) message ("All observations are presences, so can't compute measures that require absences.")

    # if (!is.null(model)) {
    #   model <- NULL  # so the message is not repeated for each threshold
    # }  # end if model

    input.measures <- measures
    similarity.measures <- modEvAmethods("similarity")
    thresh.measures <- measures[!(measures %in% similarity.measures)]

    if ("minSensSpecDiff" %in% optimize || "maxSensSpecSum" %in% optimize) {
      if (!("Sensitivity" %in% measures)) {
        measures <- c(measures, "Sensitivity")
      }  # end if !Sensitivity
      if (!("Specificity" %in% measures)) {
        measures <- c(measures, "Specificity")
      }  # end if !Specificity
    }  # end if minSensSpecDiff

    if("maxKappa" %in% optimize && !("kappa" %in% measures)) {
      measures <- c(measures, "kappa")
    }

    if("maxTSS" %in% optimize && !("TSS" %in% measures)) {
      measures <- c(measures, "TSS")
    }

    if("maxJaccard" %in% optimize && !("Jaccard" %in% measures)) {
      measures <- c(measures, "Jaccard")
    }

    if("maxSorensen" %in% optimize && !("Sorensen" %in% measures)) {
      measures <- c(measures, "Sorensen")
    }

    thresholds <- seq(from = 0, to = 1, by = interval)
    Nthresholds <- length(thresholds)
    Nmeasures <- length(measures)
    all.thresholds <- data.frame(matrix(data = NA,
                                        nrow = Nthresholds,
                                        ncol = Nmeasures),
                                 row.names = thresholds)
    colnames(all.thresholds) <- measures

    for (t in 1 : Nthresholds) for (m in 1 : Nmeasures) {
      if (measures[m] %in% similarity.measures) {
        all.thresholds[t, m] <- similarity(obs = obs, pred = pred,
                                           thresh = thresholds[t],
                                           measures = measures[m],
                                           simplif = TRUE,
                                           plot = FALSE,
                                           verbosity = 0)
      } else {
        all.thresholds[t, m] <- threshMeasures(obs = obs, pred = pred,
                                               thresh = thresholds[t],
                                               measures = measures[m],
                                               standardize = FALSE,
                                               simplif = TRUE,
                                               plot = FALSE,
                                               verbosity = 0)
      }
    }  # end for t for m

    if (isTRUE(simplif)) {  # shorter version for use with e.g. the optiPair function
      return(all.thresholds)

    } else {  # if !simplif
      results <- list(all.thresholds = all.thresholds)  # start a list of results

      input.optimize <- optimize

      if (plot == TRUE && !("each" %in% optimize)) optimize <- c("each", optimize)

      if ("each" %in% optimize) {
        optimals.each <- data.frame(matrix(data = NA, nrow = Nmeasures, ncol = 4))
        colnames(optimals.each) <- c("measure", "threshold", "value", "type")
        optimals.each[1] <- measures
        goodness.measures <- c("CCR", "Sensitivity", "Specificity", "PPP", "NPP", "kappa", "TSS", "NMI", "OddsRatio", "F1score", "Precision", "Recall", similarity.measures)
        badness.measures <- c("Omission", "Commission", "Misclass", "UPR", "OPR")
        change.measures <- c("PPI", "PAI")

        for (m in 1 : Nmeasures) {
          if (measures[m] %in% (goodness.measures)) {  # optimal is maximum
            th <- as.numeric(rownames(all.thresholds)[which.max(all.thresholds[ , m])])
            if (length(th) > 0) optimals.each[m, "threshold"] <- th

            optimals.each[m, "value"] <- max(all.thresholds[ , m], na.rm = TRUE)
            optimals.each[m, "type"] <- "maximum"
          }  # end if measure in goodness
          else {
            if (measures[m] %in% (badness.measures)) {  # optimal is minimum
              th <- as.numeric(rownames(all.thresholds)[which.min(all.thresholds[, m])])
              if (length(th) > 0) optimals.each[m, "threshold"] <- th
              optimals.each[m, "value"] <- min(all.thresholds[ , m], na.rm = TRUE)
              optimals.each[m, "type"] <- "minimum"
            }  # end if measure in badness
            else {
              if (measures[m] %in% (change.measures)) {  # optimal is closest to zero
                th <- as.numeric(rownames(all.thresholds)[which.min(abs(all.thresholds[ , m]))])
                if (length(th) > 0) optimals.each[m, "threshold"] <- th
                optimals.each[m, "value"] <- min(abs(all.thresholds[ , m]), na.rm = TRUE)
                optimals.each[m, "type"] <- "closest to zero"
              }  # end if measure in change
            }  # end 2nd else
          }  # end 1st else
        }  # end for m
        if ("each" %in% input.optimize)  results <- c(results, optimals.each = list(optimals.each))  # add this to results
      }  # end if each

      criteria <- optimize[optimize != "each"]
      Ncriteria <- length(criteria)

      if (Ncriteria > 0) {

        optimals.criteria <- data.frame(matrix(data = NA, nrow = Nmeasures,
                                               ncol = Ncriteria))
        rownames(optimals.criteria) <- measures
        colnames(optimals.criteria) <- criteria

        if ("preval" %in% criteria) {
          for (m in 1 : Nmeasures) {
            if (measures[m] %in% similarity.measures) {
              suppressWarnings(optimals.criteria[m, "preval"] <- similarity(obs = obs, pred = pred, thresh = "preval", measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
            } else {
              suppressWarnings(optimals.criteria[m, "preval"] <- threshMeasures(obs = obs, pred = pred, thresh = "preval", measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0))
            }
          }
        }  # end if preval

        if ("minSensSpecDiff" %in% criteria) {
          all.thresholds$SensSpecDiff <- with(all.thresholds, abs(Sensitivity - Specificity))
          minSensSpecDiff <- thresholds[which.min(all.thresholds$SensSpecDiff)]
          if (length(minSensSpecDiff) > 0 && is.finite(minSensSpecDiff)) {
            for (m in 1:Nmeasures) {
              if (measures[m] %in% similarity.measures) {
                suppressWarnings(optimals.criteria[m, "minSensSpecDiff"] <- similarity(obs = obs, pred = pred, thresh = minSensSpecDiff, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
              } else {
                optimals.criteria[m, "minSensSpecDiff"] <- threshMeasures(obs = obs, pred = pred, thresh = minSensSpecDiff, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
              }
            }
          }
        }

        if ("maxSensSpecSum" %in% criteria) {
          all.thresholds$SensSpecSum <- with(all.thresholds, Sensitivity + Specificity)
          maxSensSpecSum <- thresholds[which.max(all.thresholds$SensSpecSum)]
          if (length(maxSensSpecSum) > 0 && is.finite(maxSensSpecSum)) {
            for (m in 1 : Nmeasures) {
              if (measures[m] %in% similarity.measures) {
                suppressWarnings(optimals.criteria[m, "maxSensSpecSum"] <- similarity(obs = obs, pred = pred, thresh = maxSensSpecSum, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
              } else {
                optimals.criteria[m, "maxSensSpecSum"] <- threshMeasures(obs = obs, pred = pred, thresh = maxSensSpecSum, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
              }
            }
          }
        }

        if ("maxKappa" %in% criteria) {
          if (!("kappa" %in% measures)) {
            for (t in 1 : Nthresholds) {
              all.thresholds$kappa <- threshMeasures(obs = obs, pred = pred, thresh = thresholds[t], measures = "kappa", standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
            }
          }
          maxKappa <- thresholds[which.max(all.thresholds$kappa)]
          for (m in 1 : Nmeasures) {
            if (measures[m] %in% similarity.measures) {
              suppressWarnings(optimals.criteria[m, "maxKappa"] <- similarity(obs = obs, pred = pred, thresh = maxKappa, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
            } else {
              optimals.criteria[m, "maxKappa"] <- threshMeasures(obs = obs, pred = pred, thresh = maxKappa, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
            }
          }
        }

        if ("maxTSS" %in% criteria) {
          if (!("TSS" %in% measures)) {
            for (t in 1 : Nthresholds) {
              all.thresholds$TSS <- threshMeasures(obs = obs, pred = pred, thresh = thresholds[t], measures = "TSS", standardize = FALSE, simplif = TRUE, plot = FALSE)
            }
          }
          maxTSS <- thresholds[which.max(all.thresholds$TSS)]
          if (length(maxTSS) > 0 && is.finite(maxTSS)) {
            for (m in 1 : Nmeasures) {
              if (measures[m] %in% similarity.measures) {
                suppressWarnings(optimals.criteria[m, "maxTSS"] <- similarity(obs = obs, pred = pred, thresh = maxTSS, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
              } else {
                optimals.criteria[m, "maxTSS"] <- threshMeasures(obs = obs, pred = pred, thresh = maxTSS, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
              }
            }  # end for m
          }  # end if finite maxTSS
        }  # end if maxTSS

        if ("0.5" %in% criteria) {
          for (m in 1 : Nmeasures) {
            optimals.criteria[m,"0.5"] <- all.thresholds[rownames(
              all.thresholds) == 0.5, m]
          }
        }  # end if 0.5

        if ("maxJaccard" %in% criteria) {
          if (!("Jaccard" %in% measures)) {
            for (t in 1 : Nthresholds) {
              all.thresholds$Jaccard <- similarity(obs = obs, pred = pred, thresh = thresholds[t], simplif = TRUE, plot = FALSE, verbosity = 0)["Jaccard", "Value"]
            }
          }
          maxJaccard <- thresholds[which.max(all.thresholds$Jaccard)]
          if (length(maxJaccard) > 0 && is.finite(maxJaccard)) {
            for (m in 1 : Nmeasures) {
              if (measures[m] %in% similarity.measures) {
                suppressWarnings(optimals.criteria[m, "maxJaccard"] <- similarity(obs = obs, pred = pred, thresh = maxJaccard, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
              } else {
                optimals.criteria[m, "maxJaccard"] <- threshMeasures(obs = obs, pred = pred, thresh = maxJaccard, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
              }
            }
          }
        }  # end if maxJaccard

        if ("maxSorensen" %in% criteria) {
          if (!("Sorensen" %in% measures)) {
            for (t in 1 : Nthresholds) {
              all.thresholds$Sorensen <- similarity(obs = obs, pred = pred, thresh = thresholds[t], simplif = TRUE, plot = FALSE, verbosity = 0)["Sorensen", "Value"]
            }
          }
          maxSorensen <- thresholds[which.max(all.thresholds$Sorensen)]
          if (length(maxSorensen) > 0 && is.finite(maxSorensen)) {
            for (m in 1 : Nmeasures) {
              if (measures[m] %in% similarity.measures) {
                suppressWarnings(optimals.criteria[m, "maxSorensen"] <- similarity(obs = obs, pred = pred, thresh = maxSorensen, measures = measures[m], simplif = TRUE, plot = FALSE, verbosity = 0))
              } else {
                optimals.criteria[m, "maxSorensen"] <- threshMeasures(obs = obs, pred = pred, thresh = maxSorensen, measures = measures[m], standardize = FALSE, simplif = TRUE, plot = FALSE, verbosity = 0)
              }
            }  # end for m
          }
        }  # end if maxSorensen

        results <- c(results, optimals.criteria = list(optimals.criteria))  # add this to results
      }  # end if Ncriteria > 0

      if (plot) {
        opar <- par(no.readonly = TRUE)
        if (reset.par) on.exit(par(opar))
        par(mgp = c(1.8, 0.7, 0))
        
        n.input.measures <- length(input.measures)

        if (!is.na(sep.plots) && isTRUE(sep.plots)) {
          par(mfrow = c(1, 1))

        } else if (!is.na(sep.plots) && isFALSE(sep.plots)) {
          # if (n.input.measures > 4)  
            par(mar = c(3, 2.8, 0.5, 0.5))
          par(mfrow = arrangePlots(n.input.measures))
        }  # end if sep.plots else

        for (m in 1 : n.input.measures) {
          if(any(is.finite(all.thresholds[ , m]))) {
            plot(all.thresholds[ , m] ~ thresholds, ylab = input.measures[m], pch = pch, cex = cex, col = col, xlab = xlab, ...)

            opt <- gsub("min|max", "", input.optimize)
            if ("each" %in% input.optimize || isTRUE(all.equal(opt, measures))) {
              abline(v = optimals.each[m, "threshold"], col = "darkgrey", lty = 2)  # vertical line on optimal threshold
              abline(h = optimals.each[m, "value"], col = "darkgrey", lty = 2)  # horiz line on optimal value
            }

          } else {
            plot(thresholds, thresholds, ylab = input.measures[m], type = "n", ...)
          }
        }  # end for m
      }  # end if plot

      return(results)

    }  # end if simplif else
  }
