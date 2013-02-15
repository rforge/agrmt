expand <- function(F) {
  # Converts frequency vector in a population vector
  # Argument: F = frequency vector
  # Example: F <- c(10,20,34,5)
  if (min(F) < 0) stop("Error: negative values found in frequency vector.") # input validation
  k <- length(F) # prepare expansion
  D <- NULL      # prepare expansion
  for (i in 1:k) {
    D <- c(D,rep(i,F[i]))   # expand
  }
  return(D)      # population vector
}
