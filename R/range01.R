range01 <-
function(x, na.rm = TRUE) {
  # version 1.1 (9 Aug 2013)
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}
