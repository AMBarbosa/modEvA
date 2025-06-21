# Version 3.37
## (Committed 2025-06-21)

### Modified functions:

* varImp
    - replaced instances of methods::is() with inherits()
    - check if class 'Gam' or 'gam', in which case 'imp.type' automatically set to 'permutation' (as 'maxnet' already did)
    - warning (not just message if verbosity) when 'imp.type' automatically changed
    - changed 'col' argument defaults to named colours
    - help file mentions 'Gam' and 'gam' classes also implemented

* mod2obspred
    - replaced instances of methods::is() with inherits()

* prevalence
    - replaced  if (!is(obs, "vector") && !is(obs, "factor"))  with  if (!is.vector(obs) && !is.factor(obs))  to remove dependency on {methods}


### Other modified files:

* NAMESPACE
    - removed importFrom("methods", "is")

* DESCRIPTION
    - removed "methods" from Imports 


# Version 3.36
## (Committed 2025-06-09)

### Modified functions:

* RsqGLM
    - renamed to pseudoRsq

* MillerCalib
    - changed colour of slope-1 value on plot, from red (too dramatic) to dark red
    - moved slope-1 value on plot to just after slope, before intercept

* optiThresh
    - added '...' which was missing in 'plot(all.thresholds, )'


# Version 3.35
## (Committed 2025-05-12)

### Modified functions:

* confusionLabel
    - vector output now also ordered factor rather than character (as for raster inputs)


### Other modified files:

* AUC.Rd
- mention Swets AUC-ROC interpretation thresholds

* RsqGLM.Rd
- mention McFadden R-squared interpretation thresholds

* threshMeasures.Rd
- mention prevalence dependence and add reference on TSS

* VarImp.Rd
- mention {vip} and other packages in See Also and References



# Version 3.34
## (Committed 2025-04-29) -> CRAN

### Modified functions:

* confusionLabel
    - reordered output categories, low to high rather than alphabetical
    - removed colour table for rasters
    
* RsqGLM
    - implemented for raster inputs

* MillerCalib
    - plot text now grey for intercept
    - plot text now includes 1-slope (in red)
    


# Version 3.33 
## (Committed 2025-03-19) -> CRAN


### Modified functions:

* AUC
    - interval="auto" now 0.01 or 0.001 depending on 'pred' coverage
    - added cex.lab argument
    - improved axis labels

* MillerCalib, lollipop
    - slightly changed default colours
    
    
## Other modified files:

* NEWS
    - reformatted and converted to .md to work with news()
    
    

# Version 3.32 
## (Committed 2025-03-18)


### Modified functions:

* plotCoeffs
    - return also data frame with the results 
    - improve Details section



# Version 3.31 
## (Committed 2025-03-05)


### Modified functions:

* varImp
    - added option "permutation" for argument 'imp.type' (pending implementation for BART models)
    - added partial argument matching
    - added arguments 'n.per', 'data' and 'n.trees' for some model classes when imp.type="permutation"
    - implemented also for 'maxnet' models if imp.type = "permutation"

* mod2obspred
    - added option x.only = FALSE; if TRUE, the values of the predictors are returned instead (for varImp if imp.type = "permutation")

* AUC
    - 'interval' new default "auto", to make it smaller as 'pred' mean departs from 0.5



# Version 3.30 
## (Committed 2025-02-21) -> CRAN


### Modified functions:

* AUC, Boyce, lollipop (affecting threshMeasures), optiPair
    - fixed opar() reset bug: reset only the changed parameter 'mgp'



# Version 3.29 
## (Committed 2025-02-14) -> CRAN


### Modified functions:

* lollipop (-> varImp, ploCoeffs, threshMeasures, similarity, RsqGLM)
    - removed 'axis.lab' argument for back-compatibility issues
    - improved handling of plot arguments provided under \dots, namely xlab and ylab when horiz is either TRUE or FALSE



# Version 3.28 
## (Committed 2025-02-10)


### Modified functions:

* lollipop
    - better distribution between plot limits when two lollipops
    - renamed argument "horizontal" to "horiz", to match barplot()
    - added 'axis.lab' argument, to use as xlab or ylab depending on 'horiz'

* plotCoeffs
    - added 'labels' argument

* varImp
    - fixed when horizontal = TRUE

* threshMeasures, similarity
    - changed 'las' from 3 to 2



# Version 3.27 
## (Committed 2025-01-28)


### Modified functions:

* lollipop
    - 'bold' default FALSE, not NULL

* plotCoeffs
    - empty plot if no variables in model


## Other modified files:

* lollipop.Rd, plotCoeffs.Rd, varImp.Rd
    - improved Description, Examples, and See also



# Version 3.26 
## (Committed 2025-01-27)


### Modified functions:

* lollipop
    - new argument "horizontal"
    - "sticks" can also be a matrix of confidence limits


### New functions:

* plotCoeffs
    - uses lollipop() with confidence intervals


## Other modified files:

* evaluate.Rd
    - improved definitions of a, b, c, and d, relating with TP, FP, TN, FN



# Version 3.25 
## (Committed 2024-12-27)


### Modified functions:

* AUC
    - new default line colour and diagonal line type
    - xlab closer to axis, to match 2nd line of ylab and fit in default margins
    - values also closer to axes

* Boyce
    - labels and values closer to axes
    - new colours for points and for plot text
    - new position for plot text
    - new symbol (triangle) for red points

* optiPair
    - plot values closer to axes
    - new arguments 'col1' and 'col2' for plot point colours

* optiThresh
    - plot values and axis labels closer to axes
    - new plot arguments 'pch', 'cex' and 'col', with new default colours and symbols
    - plot margins adjusted also when fewer than 4 measures
    - fixed 'xlab' argument, which wasn't being considered



# Version 3.24 
## (Committed 2024-12-18)


### Modified functions:

* MillerCalib
    - changed default line colours to blues
    - lwd = 2 for calibration line
    - intercept in parentheses in plot text
    - help file now mentions Baquero et al.'s thresholds for a good slope



# Version 3.23 
## (Committed 2024-11-24) -> CRAN


### Modified functions:

* lollipop
    - new argument ylim [to replace ymin, futurely deprecated], default "auto0"

* threshMeasures, similarity
    - change default ymin to 0 (default ylim "auto0") when min value is >0, to avoid deceiving plots
    - allow plot=TRUE with simplif=FALSE

* optiThresh, multModEv
    - added plot=FALSE to some threshMeasures() calls



# Version 3.22 
## (Committed 2024-11-19)


### Modified functions:

* varImp
    - new default "auto0" for 'ylim', to avoid deceiving plots



# Version 3.21 
## (Committed 2024-11-19)


### Modified functions:

* predDensity
    - in plot legend, changed "absences" to "background" if pbg = TRUE

* optiThresh
    - added 'reset.par' argument
    - 'sep.plots' argument can be set to NA

* Boyce, Dsquared, errorMeasures, getThreshold, logLike, MillerCalib, optiPair, plotGLM, predPlot, RMSE
    - added 'verbosity' argument and passed it to inputMunch()

* confusionMatrix, getBins, HLfit, predDensity, similarity
    - passed 'verbosity' argument to inputMunch()



# Version 3.20 
## (Committed 2024-10-28) -> CRAN


### Modified functions:

* inputMunch, applyThreshold, AUC, Boyce, Dsquared, getThreshold, optiPair, optiThresh, predPlot, similarity, threshMeasures (not confusionMatrix, confusionLabel, errorMeasures, getBins, HLfit, logLike, MillerCalib, plotGLM, RMSE)
    - added 'pbg' argument

* predDensity
    - added also 'verbosity' argument
    - message when 'separate' automatically set to FALSE because 'ci' not NA

* Boyce
    - changed colour of plot text to grey50, to increase visibility when overlapping points



# Version 3.19 
## (Committed 2024-10-02)


### New functions:

* errorMeasures (incl. RMSE, MSE, Brier score)


### Modified functions:

* RMSE
    - warning: function deprecated in favour of errorMeasures


## Other modified files:

* quantReclass.Rd
    - update Formoso-Freire ref with publication date

* DESCRIPTION
    - added contributors



# Version 3.18.2 
## (Committed 2024-08-28) -> CRAN


### Modified functions:

* predDensity
    - slightly improved plot colours for better contrast
    - added legend when CI not NA


## Other modified files:

* predDensity.Rd
    - slight improvements to increase clarity



# Version 3.18.1 
## (Committed 2024-08-09)


### Modified functions:

* mod2obspred
    - implemented class 'GBMFit' (pkg 'gbm3') when obs.only=FALSE

* predDensity
    - adapted x-axis values for predictors outside range [0, 1] when type="histogram"
    - changed plot colours


## Other modified files:

* inputMuch.Rd
    - added mention to class 'GBMFit' for 'model'

* predDensity.Rd
    - added mention to 'predictor' values



# Version 3.18 
## (Committed 2024-08-08)


### Modified functions:

* mod2obspred, varImp
    - implemented class 'GBMFit' from pkg 'gbm3'



# Version 3.17.1 
## (Committed 2024-06-07)


### Modified files:

* varImp.Rd
    - second example now uses new argument 'relative'
    - improved descriptions and Details



# Version 3.17 
## (Committed 2024-06-06) -> CRAN


### Modified functions:

* varImp
    - 'relative' argument now applies also to BART and flexBART models
    - adapted to latest version of flexBART, which does carry variable names
    - informative error messages when model is BART and doesn't have the required info
    - added 'verbosity' argument



# Version 3.16 
## (Committed 2024-06-05)


### Modified functions:

* optiThresh
    - added 'verbosity' argument to pass to inputMunch()
    - abline() now plotted also if 'optimize' is a specific measure name, not necessarily "each"

* AUC
    - added 'verbosity' argument to pass to inputMunch()

* varImp
    - added 'relative' argument for z value (suggested by Alba Estrada)



# Version 3.15 
## (Committed 2024-03-25)


### Modified functions:

* threshMeasures, evaluate, modEvAmethods
    - added 'ORSS' and 'SEDI' metrics (Wunderlich et al. 2019) (suggested by Jose Carlos Guerrero)



# Version 3.14 
## (Committed 2024-02-01)


### New functions:

* logLike


### Modified functions:

* predDensity
    - added 'xlim' argument



# Version 3.13.3 
## (Committed 2024-01-19) -> CRAN


### Modified functions:

* threshMeasures
    - set plot=FALSE on confusionMatrix() call

* confusionMatrix
    - set default 'plot' to FALSE (for back-compatibility)



# Version 3.13.2 
## (Committed 2024-01-18)


### Modified functions:

* confusionMatrix
    - add arguments plot=TRUE, classes=FALSE and ...



# Version 3.13.1 
## (Committed 2024-01-17)


### Modified functions:

* confusionLabel
    - assign consistent colours to output raster categories
    - add plot=TRUE and ... arguments



# Version 3.13 
## (Committed 2024-01-16)


### Modified functions:

* confusionLabel
    - output SpatRaster (if input 'pred' is SpatRaster) now properly categorical


## Other modified files:

* AUC.Rd
    - unimplemented argument 'FPR.limits' now suggests using 'pROC::roc'



# Version 3.12 
## (Committed 2023-12-14)


### Modified functions:

* applyThreshold, inputMunch
    - added 'verbosity' argument to pass to ptsrast2obspred()

* AUC
    - added 'grid.lty' argument (used e.g. in modevapp)
    - 'grid' now at 0.1 (not threshold) intervals

* getThreshold
    - finished implementing 'maxF'
    - implemented maxJaccard, maxSorensen (= maxF)

* optiThresh
    - replaced single with double ||, &&
    - added modEvAmethods("similarity") to default 'measures'
    - added maxJaccard and maxSorensen criteria

* modEvAmethods
    - added Jaccard, Sorensen and their maximizations

* arrangePlots
    - return c(1, 1) if n.plots <= 1


## Other modified files:

* Boyce.Rd
    - 'Note' now mentions that Boyce is designed for suitability or favourability, not probability

* similarity.Rd
    - 'Description' and 'Details' now mention equivalence to Li & Guo's F-measures



# Version 3.11 
## (Committed 2023-11-25) -> CRAN


### Modified functions:

* Dsquared
    - added dismo.version argument: if FALSE, uses the previous version of Dsquared, as the dismo version sometimes produces negative or NaN results



# Version 3.10 
## (Committed 2023-11-10) -> CRAN


### New functions:

* quantReclass

* similarity


### Modified functions:

* modEvAmethods
    - added methods for similarity()



# Version 3.9.6 
## (Committed 2023-10-04)


### Modified functions:

* Dsquared
    - warning emitted and Dsquared calculated correctly even when family binomial and response not binary (following bug report by Elic M. Weitzel)

* varImp
    - metric reported in console result
    - absolute (rather than original) values now returned for z value (if model is glm)

* AUC
    - added 'GiniCoefficient' to outputs when simplif=FALSE



# Version 3.9.5 
## (Committed 2023-01-02)


### Modified functions:

* varImp
    - barplot now also gets error bars replotted on top of points if plot.points=TRUE
    - implemented for models of class "lbart" and "pbart" of pkg 'BART' (though not ideal for group.cats = TRUE)
    - group.cats default now FALSE
    - started implementation for flexBART models (still imperfect because object has no var names; issue submitted at https://github.com/skdeshpande91/flexBART/issues/8)



# Version 3.9.4 
## (Committed 2023-04-26)


### Modified functions:

* varImp
    - new argument group.cats=TRUE, for BART models, to sum up the contributions of (one-hot encoded) categorical variable levels



# Version 3.9.3 
## (Committed 2023-04-14) -> CRAN


### Modified functions:

* AUC
    - 'method' default now NULL, to avoid unsolicited warning when curve="PR" because default method was "rank"


## Other modified files:

* varImp.Rd
    - improved info on parameters for different plot types (e.g. cex.axis for lollipop, cex.names for barplot)



# Version 3.9.2 
## (Committed 2023-03-19)


### Modified functions:

* Boyce
    - explicit warning instead of obscure error if 'obs' has no presences
    - warning also when 'obs' has only presences (Boyce = NA)

* optiThresh
    - explicit warning (and results NA) instead of obscure error if 'obs' has either no presences or no absences
    - empty plot instead of error for incalculable metrics

* threshMeasures
    - explicit warning instead of obscure error if 'obs' has either no presences or no absences



# Version 3.9.1 
## (Committed 2023-02-26)


### Modified functions:

* varImp
    - error if family is not 'binomial', to avoid obscure error otherwise because no 'z-value' column in summary

* confusionMatrix, confusionLabel
    - changed defaults 'interval = interval, quant = quant' to 'interval = 0.01, quant = 0'


## Other modified files:

* CITATION
    - replaced old-style personList() with c(), and citEntry() with bibentry(), as per new CRAN requirements



# Version 3.9 
## (Committed 2023-01-12) -> CRAN


### Modified functions:

* varImp
    - corrected point locations (accounting for space between bars) for barplot
    - error.bars in lollipop chart re-plotted on top to improve visibility
    - help file cites Weissgerber et al.

* predDensity
    - 'ci' now plotted as semi-transparent rectangle (rather than just vertical ablines)
    - added 'ci' example to help file



# Version 3.8.9 
## (Committed 2023-01-11)


### Modified functions:

* varImp
    - implemented plot.points argument
    - arguments 'ci' and 'ci.type' replaced by argument 'error.bars'
    - 'error.bars' can also be a numeric for a x% confidence interval
    - added 'grid' argument
    - added "imp.type" argument (placeholder, so far only option "each")
    - set 'border' to NA in legend and barplot
    - fixed legend colour bug
    - added check for loaded 'gbm' when needed

* predDensity
    - added 'ci' argument
    - added '...' for arguments to pass to 'hist'



# Version 3.8.8 
## (Committed 2023-01-05)


### New functions:

* RMSE

* lollipop


### Modified functions:

* varImp
    - added "ylim" argument (default "auto"), and xpd=FALSE to 'barplot' call
    - changed default positive colour for better contrast with CI lines in barplot
    - new default plot.type = "lollipop"

* threshMeasures
    - "OddsRatio" removed from default 'measures'
    - 'ylim' argument removed (default 0,1 not needed after OddsRation removal; 'ylim' can still be used under '...')
    - new argument 'plot.type'
    - new default plot.type = "lollipop"

* RsqGLM
    - new argument 'plot.type'
    - new default plot.type = "lollipop"



# Version 3.8.7 
## (Committed 2022-12-22)


### New functions:

* varImp


### Modified functions:

* mod2obspred
    - added supressMessages() when predicting with GBM



# Version 3.8.6 
## (Committed 2022-12-20)


### Modified functions:

* Dsquared
    - 'family' is now guessed from data (with message) if null
    - extended to models other than GLM



# Version 3.8.5 
## (Committed 2022-11-06)


### Modified functions:

* ptsrast2obspred
    - added (or 'rm.dup.points=TRUE' for 'Boyce' function) to warning, otherwise Boyce would say "argument X matches multiple formal arguments"

* MilerCalib
    - added final empty line



# Version 3.8.4 
## (Committed 2022-10-28) -> CRAN


### Modified functions:

* applyThreshold
    - added "as.numeric" to 'sort(tresh)', otherwise it was character if one of the input 'thresh' criteria was character, which generated errors downstream



# Version 3.8.3 
## (Committed 2022-10-27)


### Modified functions:

* predPlot
    - 'thresh' can now be set to NA or NULL
    - legend is also changed accordingly

* optiThresh
    - warning emitted if obs are all 0 or all 1 (which may originate errorr)     - propagates to optiPair


## Other modified files:

* MillerCalib.Rd
    - added explanations and examples on applicability to non-logit link models and non-probability predictions



# Version 3.8.2 
## (Committed 2022-07-21)


### Modified functions:

* varPart
    - added argument 'cor.method'
    - changed previous argument name 'method' to 'pred.type', to avoid confusion with the above



# Version 3.8.1 
## (Committed 2022-07-20)


### Modified functions:

* varPart
    - argument 'model' now accepts glms of any family (but properly tested only for binomial!)



# Version 3.8 
## (Committed 2022-07-20)


### Modified functions:

* varPart
    - new arguments 'model', 'groups', 'method', 'return.models'
    - output table now doesn't say "pure" or "overlap", just the factor names separated by "_"
    - argument 'coloured' changed to 'colr' to avoid English/American spelling issues



# Version 3.7 
## (Committed 2022-07-10)


### Modified functions:

* applyThreshold
    - 'thresh' can now be of length 2, for high, intermediate and low predictions


## Other modified files:

* DESCRIPTION
    - added reference about the method, as suggested by Uwe Ligges after previous CRAN submission



# Version 3.6 
## (Committed 2022-06-14)


### New functions:

* confusionMatrix


### Modified functions:

* confusionLabel
    - added 'getThreshold' to include all 'thresh' options
    - added 'interval' and 'quant' arguments for the above
    - 'rm.dup' now defaults to FALSE to assume less

* threshMeasures
    - moved part of the code to new 'confusionMatrix' function, which is now called from here



# Version 3.5 
## (Committed 2022-05-16) -> CRAN


### New functions:

* applyThreshold


### Modified functions:

* Boyce
    - warning emitted and points coloured red for bins with less than 30 values

* standard01 (benefitting threshMeasures)
    - added is.finite(score) to avoid error when NAs produced by kappa integer overflow

* getThreshold, inputMunch
    - 'obs' can be null if only 'pred' is needed

* predPlot
    - added call to 'getThreshold' to include more threshold options
    - added 'cex' argument (default 0.5), plus 'interval' and 'quant' to pass to 'getThreshold'

* confusionLabel
    - output is SpatRaster if input is too (as also implemented in 'applyThreshold')

* optiThresh
    - fixed bug: Precision and Recall were missing from 'goodness.measures' and didn't get optimals



# Version 3.4 
## (Committed 2022-05-06)


### Modified functions:

* AUC, confusionLabel, Dsquared, getBins, HLfit, MillerCalib, optiPair, optiThresh, plotGLM, predDensity, predPlot, RsqGLM, threshMeasures
    - added arguments 'rm.dup' and 'na.rm' to pass to 'inputMunch' and cascade to 'ptsrast2obspred'

* Boyce
    - renamed arguments 'rm.dupl.classes' and 'rm.dupl.points' to 'rm.dup.classes' and 'rm.dup.points', for better coherence with 'rm.dup' in other functions
    - added 'na.rm' argument



# Version 3.3 
## (Committed 2022-05-03)


### New functions:

* inputMunch
    - converts from model or point & raster inputs to obs & pred vectors

* getThreshold
    - computes threshold based on any of a range of published criteria


### Modified functions:

* prevalence
    - added 'unlist' to avoid obscure error when input is one-column tibble instead of vector

* confusionLabel, Dsquared, getBins, HLfit, MillerCalib, optiPair, optiThresh, plotGLM, predDensity, predPlot, RsqGLM, threshMeasures
    - 'obs' and 'pred' can also be presence coordinates and a SpatRaster, respectively

* confusionLabel, getBins
    - added 'na.rm' argument

* threshMeasures
    - 'thresh' argument now calls new 'getThreshold' function to include more options

* ptsrast2obspred
    - checks for duplicates and shows message if there are (if rm.dup = FALSE)
    - 'rm.dup' now defaults to FALSE, to assume less and to match 'ecospat.boyce'

* Boyce
    - 'rm.dupl.points' now defaults to FALSE, to assume less and to match 'ecospat.boyce'

* modEvAmethods
    - added error message when 'fun' not correctly specified (instead of silent empty result)
    - help file now provides all available values under 'fun' argument

* getModEqn
    - 'round' replaced with 'signif' to avoid small coeffs becoming zeros (bug report by Jose Carlos Guerrero)

* MESS
    - added 'verbosity' argument



# Version 3.2 
## (Committed 2022-03-13)


### Modified functions:

* AUC
    - 'obs' and 'pred' can be presence coordinates and a SpatRaster, respectively

* Boyce
    - added argument '...', e.g. for plot 'main' or 'xlim'
    - argument name 'nclass' replaced with 'n.bins' to match other modEvA functions
    - argument name 'window.w' replaced with 'bin.width' to accomodate other modEvA functions
    - default 'nclass = 0' replaced with 'n.bins = NA', and all else accordingly
    - changed default 'rm.dupl.classes' to FALSE after some checks (e.g. Galpyr RF in SDMB course)

* ptsrast2obspred
    - added argument 'na.rm' and set to TRUE by default
    - help file now uses 'elev' raster for more clarity



# Version 3.1 
## (Committed 2022-02-22)


### New functions:

    - Boyce

    - ptsrast2obspred


### Modified functions:

* AUC
    - moved 'pred' range check to after NAs are removed to avoid error

* predPlot
    - on exit, restores only 'par(mar)' (the only one changed) rather than all 'par'

* RsqGLM
    - added las = 2 to 'barplot'



# Version 3.0 
## (Committed 2021-12-20) -> CRAN


### Modified functions:

* AUC
    - warning if 'obs' contains only zeros or only ones (to explain result NaN and no curve on plot)
    - if the previous occurs, other warnings suppressed (PR curve interpolated past NaN precision, method changed to 'trapezoid', etc.)

* MillerCalib
    - plot text elevated by 0.05 y-axis units, to avoid it being cropped in small plots


## Other changes:

    - uniformized description of arguments "model", "obs" and "pred" across functions' help files



# Version 2.8 
## (Committed 2021-12-03)


### Modified functions:

* optiPair
    - message "'pred' must range between 0 and 1" changed from error to warning
    - added argument 'na.rm' (default TRUE) to avoid NaNs in result MinDiff, MaxSum, etc.
    - added argument 'exclude.zeros' (default TRUE) to avoid precision-recall "optimal" difference when both 0 or NaN



# Version 2.7 
## (Committed 2021-11-26)


### New functions:

* confusionLabel (to map the confusion matrix)

* mod2obspred (to extract 'obs' and 'pred' from model objects)


### Modified functions:

* prevalence
    - message "'event' is not among of the values of 'obs'" changed from error to warning
    - 'mod2obspred' now used for extracting 'obs' if 'model' is provided

* AUC, getBins, HLfit, MillerCalib, optiPair, optiThresh, predDensity, predPlot, threshMeasures
    - 'model' can now be of class "glm", "gam", "gbm", "randomForest" or "bart"
    - 'mod2obspred' now used for extracting 'obs' and 'pred' if 'model' is provided



# Version 2.6 
## (Committed 2021-11-01)


### Modified functions:

* predDensity
    - added 'main' argument for plot title

* predPlot
    - added 'pch' and 'col' arguments for the plot
    - legend.pos is now "n" by default (legend was ugly and needed box to distinguish from actual plot points)

* RsqGLM
    - added "plot" argument (default TRUE)
    - added '...' for additional parameters for 'plot'

* plotGLM
    - added 'plot=FALSE' to 'RsqGLM' (used if 'plot.values')

* optiThresh
    - slightly changed plot margins so that bottom axis values are visible


## Other modified files:

* www/modEvA-tutorial.html
    - updated with these recent options



# Version 2.5 
## (Committed 2021-10-31)


### Modified functions:

* AUC
    - plot.preds=TRUE now plots filled circles with transparency
    - plot.preds=TRUE now defaults to plotting circles on the curve (rather than both curve and bottom)
    - size of circles if plot.preds=TRUE changed from 100*prop.preds to 20*sqrt(prop.preds)
    - x coordinates of circles if plot.preds=TRUE changed from 'thresholds' to 'xx'
    - if plot.values=TRUE, value is now plotted on the middle bottom for ROC and middle top for PR curve
    - NaN precision values now coerced to the last non-NaN value (rather than 1), and warning emitted
    - interpolated (NaN precision) part of the PR curve now plotted besides the curve with actual precision values



# Version 2.4 
## (Committed 2021-10-30)


### Modified functions:

* AUC
    - fixed wrong PR area when last precision value(s) NaN, by coercing NaN precisions to 1 (thanks to bug report by Ying-Ju Tessa Chen)
    - deactivated option method = "integrate", which was providing somewhat inaccurate values
    - added / improved warning messages when 'method' changed to match chosen 'curve'
    - added warning when method = "trapezoid" and interval >= 0.01, noting result is more accurate with smaller intervals

* threshMeasures
    - added 'ylim' argument with default c(0, 1) to avoid error when no selected measure has finite values


## Other modified files:

* inst/CITATION
    - corrected publication year of 'New measures' paper under 'textVersion' of citation (thanks to bug report by Alba Estrada)



# Version 2.3 
## (Committed 2021-09-30)


### Modified functions:

* prevalence
    - added 'model' argument, which can now be provided instead of 'obs'
    - help file includes examples with 'model' and with character 'obs'
    - error message when 'obs' not a vector and when 'event' not in 'obs'
    - warning when 'obs' ignored in favour of 'model'



# Version 2.2 
## (Committed 2021-09-08)


### Modified functions:

* MillerCalib
    - slope value now plotted before (above) intercept (if plot.values = TRUE)

* varPart
    - fixed right margin in 2-factor plot and added option for colouring the circles, following suggestions by Oswald van Ginkel

* MESS
    - bug fix e-mailed by Huijie Qiao



# Version 2.1 
## (Committed 2021-01-15)


### New functions:

* predPlot


### Modified functions:

* predDensity
    - like 'predPlot', 'legend.pos' now allows NA (for no legend) instead of throwing obscure error

* optiThresh
    - added warning when any of 'measures' or 'optimize' not within implemented values


## Other modified files:

* MESS.Rd
    - removed mention that we will implement ExDet for dataframes; added 'ecospat.climan' to 'See also'


## Other changes:

* minor corrections to some 'man' files



# Version 2.0 
## (Committed 2020-01-21) -> CRAN


### Modified functions:

* modEvA-internal
    - removed .Random.seed

* multModEv, modEvAmethods, modEvA-internal
    - added "MeanPrecision", "AUCPR" and "F1score"


## Other modified files:

* multModEv.Rd
    - added "AUCPR" to one of the examples



# Version 2.0 
## (Committed 2020-01-20)


### Modified functions:

* AUC
    - added 'meanPrecision' to returned list (if simplif = FALSE)
    - "AUC[PR]" mentioned on plot when curve = "PR"

* optiThresh
    - added "F1score" to goodness.measures in optimals.each
    - added Liu et al. 2005 reference to threshold criteria

* threshMeasures, modEvAmethods
    - added "Precision" and "Recall" (even though synonyms with "PPP" and "Sensitivity")

* optiPair
    - changed pch of measure 1 to 19 (slightly larger than pch 20) so it's still visible when overlapped by measure 2

* multModEv
    - added 'plot = FALSE' to internal AUC call


## Other modified files:

* threshMeasures.Rd, optiThresh.Rd, optiPair.Rd
    - added "Note" saying sens=recall and PPP=precision, and defining F1score



# Version 1.8 
## (Committed 2020-01-17)


### Modified functions:

* AUC
    - removed artificial zeros (instead of NaN) when curve = "PR"
    - reversed the reference diagonal if curve = "PR"
    - value now placed higher on the plot if curve = "PR" to avoid superposition


## Other changes:

    - added 'inst' folder with article citation information



# Version 1.7 
## (Committed 2020-01-16)


### Modified functions:

* AUC
    - implemented methods "trapezoid" and "integrate"
    - added area calculation for "PR" curve


* threshMeasures, evaluate, modEvAmethods, (optiThresh, optiPair)
    - added "F1score" to available 'measures'



# Version 1.5 
## (Committed 2020-01-15)


### New functions:

* predDensity


### Modified functions:

* AUC
    - new 'method' argument, which in the future can be "rank", "trapezoid" or "integrate" (currently only "rank" is implemented)
    - new 'curve' argument which can be "ROC" or "PR" (precision-recall)
    - arguments 'roc.col', 'roc.lty' and 'roc.lwd' renamed to 'curve.col', 'curve.lty' and 'curve.lwd'
    - 'xlab' and 'ylab' now default to "auto", to be generated according to 'curve'
    - removed 'main' argument (can be provided via '...')
    - 'plot.preds' can now be either a logical value as before, or a character "curve" and/or "bottom"
    - 'plot.preds' circles now plotted in darkgrey
    - title in .Rd file changed to "Area Under the Curve" (removed "ROC")


## Other changes:

    - corrected some problems in .Rd files, to reduce LaTeX errors when creating PDF version of manual.



# Version 1.4.2 
## (Committed 2020-01-03)


### Modified functions:

* plotGLM
    - corrected to adj=1 when plotting values on the right of the plot



# Version 1.4.1 
## (Committed 2019-10-18)


### Modified functions:

* RsqGLM
    - added NA handling for 'cor'



# Version 1.4 
## (Committed 2018-11-28)


### Modified functions:

* AUC
    - added arguments diag, diag.col, diag.lty, roc.col, roc.lty, roc.lwd
    - 'simplif = TRUE' no longer overrides 'plot = TRUE'

* MillerCalib
    - added arguments diag, line.col


## Other modified files:

    - edited .Rd files (AUC, HLfit, evaluate, optiPair, threshMeasures, modEvA-package) to reflect difference between discrimination and classification
    - updated index.php



# Version 1.3.3 
## (Committed 2017-03-24)


### Modified functions:

* varPart:
    - bug fixed (with na.omit) when only two factors (A, B, AB)



# Version 1.3.2 
## (Committed 2016-07-12)


### Modified functions:

* getBins (affecting HLfit):
    - bug fixed in "bin.method = size.bins", which was giving two different messages regarding ignored arguments

* HLfit:
    - help file slightly clarified



# Version 1.3.1 
## (Committed 2016-06-27)


### Modified functions:

* getBins (affecting HLfit):
    - bug fixed in "bin.method = quantiles", now providing even-sized bins (=SPSS)
    - added argument 'quantile.type', to pass to 'quantile' function

* HLfit:
    - added argument 'quantile.type', to pass to 'quantile' function
    - added argument 'verbosity'

* multModEv:
    - removed default 'thresh' (must be user-specified)
    - replaced argument 'quiet' with 'verbosity' for coherence with other functions

* threshMeasures:
    - removed default 'thresh' (must be user-specified)
    - replaced argument 'messages' with 'verbosity' for coherence with other functions



# Version 1.3 
## (Committed 2016-06-16)


### Modified functions:

* getBins (affecting HLfit):
    - bug fixed in "bin.method = quantiles", removing last additional bin of size 1
    - help file improved to better explain what each bin.method does and what arguments it ignores
    - added argument 'verbosity'

* HLfit:
    - added examples to help file to illustrate differences in bin.methods



# Version 1.2.9 
## (Committed 2016-06-06)


### Modified functions:

* getBins (affecting HLfit):
    - bug fixed in "bin.method = n.bins, fixed.bin.size = TRUE"
    - help file updated to explain what each bin.method does
    - removed default bin.method (must be specified by user)

* multModEv:
    - included '...' for additional arguments to pass to HLfit
    - removed default bin.method (must be specified if HL in measures)

* modEvAmethods:
    - excluded "Miller.p" (previously eliminated from MillerCalib)



# Version 1.2.8 
## (Committed 2016-04-18)


### Modified functions:

* varPart:
    - 'plot.unexpl' now also rounded to 'plot.digits'
    - 'model.type' deprecated (message emitted)
    - AB or ABC now also mandatory, and unexplained var 0, for GLMs
    - help file updated

* plotGLM:
    - warning on preds outside [0,1] now only emitted when appropriate

* AUC, getBins, HLfit, MillerCalib, multModEv, optiPair, optiThresh, plotGLM, threshMeasures:
    - error when model(s) provided but not binomial logit glm



# Version 1.2.7 
## (Committed 2016-04-15)


### Modified functions:

* varPart:
    - added argument 'plot.unexpl = TRUE'
    - explicit error message for 3-factor LM with no ABC
    - help file now with both LM and GLM examples
    - help file now notes that wrong input = wrong result



# Version 1.2.6 
## (Committed 2016-04-13)


### Modified functions:

* AUC:
    - error replaced with warning when pred values outside the [0,1] interval
    - added argument plot.digits = 3 (for plot.values)

* varPart:
    - added arguments 'main' and 'cex.main'
    - slightly increased default cex values
    - updated help file to make GLM use a bit clearer

* plotGLM:
    - error replaced with warning when pred values outside the [0,1] interval
    - added pseudo-R-squared measures to plot.values
    - changed plot.values default to TRUE
    - added argument plot.digits = 3 (for plot.values)



# Version 1.2.5 
## (Committed 2016-03-30)


### Modified functions:

* getBins:
    - eliminated error when pred values outside the [0,1] interval

* HLfit:
    - error replaced with warning on pred values outside the [0,1] interval
    - plot now stretches if pred values exceed [0,1] interval

* MillerCalib:
    - error replaced with warning on pred values outside the [0,1] interval
    - default digits (for plot.values) lowered to 2
    - slope p-value removed (values looked wrong)
    - NaNs avoided by converting 0 and 1 in 'pred' to (1 -) 2e-16
    - increased plot ymax by 0.2
    - changed plot text location to bottom right



# Version 1.2.4 
## (Committed 2016-03-22)


### Modified functions:

* multModEv:
    - warning now emitted, and help file updated, about calibration measures being valid only for probability (when input is obs.data + pred.data)



# Version 1.2.3 
## (Committed 2015-12-10)


### Modified functions:

* prevalence:
    - 'na.rm = TRUE' added



# Version 1.2.2 
## (Committed 2015-11-27)


### Modified functions:

* Dsquared, RsqGLM, plotGLM:
    - NaN-caused errors avoided by converting 0 and 1 in 'pred' to (1 -) 2e-16



# Version 1.2.1 
## (Committed 2015-11-26)


### Modified functions:

* Dsquared, RsqGLM:
    - NaN-caused errors avoided by converting 0 in 'pred' to smallest computable positive number



# Version 1.2


### Modified functions:

* AUC, Dsquared, RsqGLM, threshMeasures, optiPair, HLfit & MillerCalib now omit NAs

* Dsquared based on obs&pred now available also for Poisson GLMs
