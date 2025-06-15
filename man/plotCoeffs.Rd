\name{plotCoeffs}
\alias{plotCoeffs}
\title{Plot model coefficients with confidence intervals}
\description{
  Gets the coefficient estimates of a model with their 95\% confidence intervals, and shows them in a point range plot.
}
\usage{
  plotCoeffs(model, labels = NULL, plot = TRUE, ...)
}
\arguments{
  \item{model}{A model object of class "glm", "lm" or "phylolm".}
  \item{labels}{Optional two-column matrix or data frame, with the first column containing names of variables that appear in 'model' (in no particular order), and the second column containing the corresponding names to use as labels in the plot. Can be useful when the model contains abbreviated variable names, and you want more complete variable names in the plot.}
  \item{plot}{Logical value. If set to FALSE, only the results data frame is produced, and no plot.}
  \item{...}{(If plot=TRUE) additional arguments to pass to \code{\link{lollipop}}, e.g. horizontal = TRUE.}
}
\details{
  This function uses \code{\link{summary}} to get the coefficients (excluding the intercept) and their standard errors from a fitted model object, and it computes the 95\% confidence intervals, i.e. each coefficient plus/minus 1.96 times its standard error. By default, it also uses \code{\link{lollipop}} to produce a point range plot with those results. In the plot, the labels of coefficients with significant p-values (<= 0.05) are displayed in bold.
  
  Note that coefficients are only directly comparable if the variables were standardized or if they vary in the same scale.
}
\value{
  A data frame (and a plot).
}
\author{
  A. Marcia Barbosa
}
\seealso{
  \code{\link{varImp}}, \code{\link{lollipop}}
}
\examples{
# get example model:

mod <- glm(am ~ mpg + wt + gear + carb, data = as.data.frame(scale(mtcars)))

summary(mod)


# default plot:

plotCoeffs(mod)


# with some plot tweaks:

labels <- data.frame(
  vars = c("gear", "wt", "mpg", "carb"), 
  labs = c("N gears", "Weight", "Miles per gallon", "N carburators")
  )

par(mar = c(3, 10, 2, 1))

plotCoeffs(mod, horiz = TRUE, cex = 2, grid = FALSE, axis.lab = "Coefficients", 
main = "Transmission", labels = labels, col = "purple")
}
