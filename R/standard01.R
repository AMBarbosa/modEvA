standard01 <-
function(score, direction = c("-1+1to01", "01to-1+1")) {
  # version 1.1 (11 June 2014)
  # standardizes a value between -1 and 1 to its corresponding value in the 0-1 scale, or vice-versa
  
  direction <- match.arg(direction, c("-1+1to01", "01to-1+1"))
  
  if (direction == "-1+1to01") {
    if (is.finite(score) && (score < -1 || score > 1)) stop("'score' must be between -1 and 1")
    std.score <- score + ((1 - score) / 2)
  } else {
    if (is.finite(score) && (score < 0 || score > 1)) stop("'score' must be between 0 and 1 to standardize in this direction")
    std.score <- 2 * (score - 0.5)
  }
  
  return(std.score)
}
