\name{range01}
\alias{range01}
\title{
Shrink or stretch a vector to make it range between 0 and 1
}
\description{
This function re-scales a numeric vector so that it ranges between 0 and 1. So, the lowest value becomes 0, the highest becomes 1, and the ones in the middle retain their rank and relative diference.
}
\usage{
range01(x, na.rm = TRUE)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical, whether to remove \code{NA} values.}
}
\details{
This function was borrowed from http://stackoverflow.com/questions/5468280/scale-a-series-between-two-points-in-r/5468527#5468527 and adapted to handle also missing values.
}
\value{
A numeric vector of the same length as the input, now with the values ranging from 0 to 1.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{standard01}}
}
\examples{
range01(0:10)

range01(-12.3 : 21.7)
}
