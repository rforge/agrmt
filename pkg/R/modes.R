modes <-
function(V, tolerance=0) {
  # Find (multiple) modes of frequency vector
  # Arguments:    V = frequency vector
  #       tolerance = tolerance to consider values the same
  # Example: V <- c(30,40,210,130,530,50,10)
  # Note: if there are two modes of different values, only the first one will be reported:
  # e.g. V <- (1,2,1,3,2) will report mode = 1.
  if (min(V) < 0) stop("Error: negative values found in frequency vector.")      # input validation
  k <- length(V)  # number of values
  p <- NULL       # empty for preparation
  u <- unique(V)  # remove duplicates of V
  m <- u[which.max(tabulate(match(V, u)))] # mode of V
  for (i in 1:k) {             # check each position to see if it is equal to the mode
    if (isTRUE(all.equal(V[i],m,tolerance=tolerance))) p <- c(p,i) # add position of the mode
  }
  # Check whether modes are contiguous (= agreement) (c = {"contiguous", "divided"}
  if ((max(p) - min(p)) < length(p)) c <- TRUE else c <- FALSE
  r <- list(vector = V, mode = m, positions = p, contiguous = c) # m = mode of V; p = position of V at which mode is found
  return(r)
  }
