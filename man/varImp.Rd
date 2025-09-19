\name{varImp}
\alias{varImp}
\title{Variable importance.}
\description{
This function computes, and optionally plots, variable importance for an input model object of an implemented class. Note that "importance" is a vague concept which can be measured in different ways, and not all importance type options are available for all model classes (see Arguments and Details).
}
\usage{
  varImp(model, imp.type = "each", relative = TRUE, reorder = TRUE,
  group.cats = FALSE, n.per = 10, data = NULL, n.trees = 100, plot = TRUE, 
  plot.type = "lollipop", error.bars = "sd", ylim = "auto0", 
  col = c("steelblue4", "coral2"), plot.points = TRUE, legend = TRUE, 
  grid = TRUE, verbosity = 2, ...)
}
\arguments{
  \item{model}{a (binary-response) model object of class "glm" (of package \pkg{stats}), "Gam" (of package \pkg{gam}), "gam" (of package \pkg{mgcv}), "gbm" (of package \pkg{gbm}), "GBMFit" (of package \pkg{gbm3}), "randomForest" (of package \pkg{randomForest}), "maxnet" (of package \pkg{maxnet}), "bart" (of package \pkg{dbarts}), "pbart" or "lbart" (of package \pkg{BART}), or a list produced by function "flexBART" or "probit_flexBART" of package \pkg{flexBART}.}
  \item{imp.type}{character value indicating the type of variable importance to compute, i.e. the metric with which importance is measured. Partial argument matching is used. Implemented options are:
  \itemize{
    \item "each" (the default), to extract the measure provided by each model object or summary (note that this is different across model classes -- see Details)
    \item "permutation", to randomly shuffle each variable in turn (a given number of times) and average the root mean squared difference between the actual model predictions and those obtained with the shuffled variable. Note this can be considerably slower, especially for computationally intensive models. Use \code{set.seed()} first if you want exactly reproducible results.
    }
  }
  \item{relative}{logical value (default TRUE) indicating whether to divide the absolute importance values by their total sum, to get a measure of relative variable importance ranging from 0 to 1. Applies when imp.type="permutation", or to GLM and BART models when imp.type="each".}
  \item{reorder}{logical value indicating whether to sort the variables in decreasing order of importance. The default is TRUE. If set to FALSE, the variables retain their input order.}
  \item{group.cats}{logical value indicating whether to aggregate all factor levels of each (one-hot encoded) categorical variable into a single variable, by summing up their proportions of branches used. Used if 'model' is of class 'bart', 'pbart' or 'lbart', in whose outputs the contributions of categorical variables are split by their factor levels. The default is FALSE. NOTE that this may incorrectly group variables that have the same name with a different numeric suffix (e.g. "o2" and "o3", or "soil_type_1", "soil_type_2"), so revise your results if you set this to TRUE!}
  \item{n.per}{(if imp.type="permutation") number of permutations for each variable. The default is 10.}
  \item{data}{(if imp.type="permutation") matrix or data frame with the predictor variables for which to compute permutation importance. The default is to extract data from the model object if it contains this information (e.g. for models of class "glm", "GBMFit", or "bart" computed with keeptrees=TRUE), or an error message otherwise, in which case the user needs to provide this argument. Note that only the variables actually included in the model should be provided in 'data'.}
  \item{n.trees}{(if imp.type="permutation") argument required by \code{predict.GBMFit()} if 'model' is of class "GBMFit". The default is 100.}
  \item{plot}{logical value indicating whether to produce a plot with the results. The default is TRUE.}
  \item{plot.type}{(if plot=TRUE) character value indicating the type of plot to produce. Can be "\code{\link{lollipop}}" (the default), "\code{\link{barplot}}", or "\code{\link{boxplot}}". Note that the latter is only useful when 'model' contains several importance values per variable (e.g. for models of class "bart"). Partial argument matching is used.}
  \item{error.bars}{character value indicating the type of error metric to compute (and plot, if plot=TRUE) if the input contains the necessary information (i.e., for Bayesian models like BART) and if the 'plot.type' is appropriate (i.e. "lollipop" or "barplot"). Can be "sd" (the default) for the standard deviation; "range" for the minimum and maximum value across the ones available; a numeric value between 0 and 1 for the corresponding confidence interval (e.g. 0.95 for 95\%), computed with \code{\link{quantile}}; or NA for no error bars.}
  \item{ylim}{(if plot=TRUE) either a numeric vector of length 2 specifying the limits (minimum, maximum) for the y axis; or "auto" to fit the y axis to the existing minimum and maximum values; or "auto0" (the new default) to fit the top of the y axis to the maximum existing values, and the bottom to zero.}
  \item{col}{(if plot=TRUE) character or integer vector of length 1 or 2 specifying the plotting colours for the variables with positive and negative effect on the response, when this info is available (e.g. for models of class "glm" when imp.type="each").}
  \item{plot.points}{(if plot=TRUE) logical, whether or not to add to the plot the individual importance points (rather than just the mean importance value, and the error bar if error.bars=TRUE) for each variable. By default it is TRUE (following Weissgerber et al. 2015), but it only holds for model objects that include several possible importance values per variable (i.e. BART models).}
  \item{legend}{logical, whether or not to draw a legend. Used only if plot=TRUE and if the output includes negative values (i.e., if 'model' is of class 'glm' and imp.type="each" and there are variables with positive and negative coefficients).}
  \item{grid}{(if plot=TRUE) logical, whether or not to add a grid to the plot. The default is TRUE.}
  \item{verbosity}{integer specifying the amount of messages to display. Defaults to the maximum implemented; lower numbers (down to 0) decrease the number of messages.}
  \item{\dots}{(if plot=TRUE) additional arguments that can be used for the plot (depending on 'plot.type'), e.g. 'main', 'cex.axis' (for \code{\link{lollipop}} or \code{\link{boxplot}}) or 'cex.names' (for \code{\link{barplot}}).}
}
\details{
  Variable importance is a non-objective characteristic which can be measured in a variety of ways -- e.g., the weight of a variable in a model (e.g. how strong its coefficient is, or how many times it is used); how much worse the model would be without it (according to a given performance metric); or how different the predictions would be if the variable were shuffled or not used. If you compute variable importance with different methods (e.g. with the functions suggested in the "See also" section), you are likely to get varied results.

  In this function, when imp.type="each" (the default), variable importance in a model of class "glm" (obtained with the \code{\link{glm}} function) can be measured by the magnitude of the absolute z-value test statistic, which is provided with \code{summary(model)}. The 'varImp' function outputs the absolute z value of each variable (or, if relative=TRUE - the default, the relative z value, obtained by dividing the absolute z value by the sum of z absolute values in the model). In the plot (by default), different colours are used for variables with positive and negative relationships with the response.

  If the input model is of class "gbm" of the \pkg{gbm} package, variable importance is obtained from \code{summary.gbm(model)} and divided by 100 to get the result as a proportion rather than a percentage (for consistency). See the help file of that function for details.

  If the input model is of class "randomForest" of the \pkg{randomForest} package, variable importance is obtained with \code{model$importance}. See the help file of randomForest for details.

  If the input model is of class "bart" of the \pkg{dbarts} package, or of class "pbart" or "lbart" of the \pkg{BART} package, or a list produced by function "probit_flexBART" of the \pkg{flexBART} package, variable importance is obtained as the mean (if relative=TRUE, the default) or the total number (if relative=FALSE) of regression tree splits where each variable is used. If 'error.bars' is not NA, the error is also computed according to the specified metric ("sd" or standard deviation by default).
  
  If \code{imp.type} is set instead to "permutation", each variable is randomly shuffled \code{n.per} times, and the root mean squared difference is computed between the model predictions and those with the shuffled variable.
  }
\value{
  This function outputs, and optionally plots, a named numeric vector of variable importance, as measured by 'imp.type' (see Details). If the model is Bayesian (BART) and 'error.bars' is not NA, the output is a row-named data frame with the mean as well as the lower and upper bounds of the error bars (according to the specified error metric - the default is standard deviation) of variable importance.
}
\author{
  A. Marcia Barbosa
}
\references{
Greenwell B.M. & Boehmke B.C. (2020) Variable Importance Plots - An Introduction to the vip Package". The R Journal, 2020: https://journal.r-project.org/articles/RJ-2020-013/

Weissgerber T.L., Milic N.M., Winham S.J. & Garovic V.D. (2015) Beyond Bar and Line Graphs: Time for a New Data Presentation Paradigm. PLOS Biol 13:e1002128. https://doi.org/10.1371/JOURNAL.PBIO.1002128
}
\seealso{
\code{\link{plotCoeffs}}; \code{\link{summary.glm}}; \code{caret::varImp}; \code{embarcadero::varimp}; \code{enmpa::var_importance}; \code{predicts::varImportance}; \code{biomod2::bm_VariablesImportance}; \code{iml::FeatureImp}; \code{ingredients::feature_importance}; \code{mmpf::permutationImportance}; package \pkg{varImp}; package \pkg{vip}; numerous packages that implement Shapley values
}
\examples{
  # load sample models:

  data(rotif.mods)


  # choose a particular model to play with:

  mod <- rotif.mods$models[[1]]


  # get variable importance for this model:

  varImp(model = mod,
         cex.axis = 0.6)

  varImp(model = mod,
        imp.type = "perm",
        cex.axis = 0.6)


  # change some more parameters:

  par(mar = c(9, 4, 2.5, 1))
  varImp(model = mod,
         relative = FALSE,
         col = c("darkgreen", "orange"),
         plot.type = "barplot",
         cex.names = 0.85,
         ylim = c(0, 5),
         main = "Variable importance in \n my model")
}
