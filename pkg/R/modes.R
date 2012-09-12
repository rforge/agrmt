modes <-
function(V, pos=FALSE, tolerance=0.1) {
  # Find (multiple) modes of frequency vector (roughly the same frequencies)
  # Arguments:    V = frequency vector
  #             pos = positions of categories
  #       tolerance = tolerance to consider values the same
  # Example 1: V <- c(30,40,210,130,530,50,10) # will find position 5
  # Example 2: V <- c(3,0,4,1) # will find position 3
  # Example 3: V <- c(30,40,528,130,530,50,10) # will find positions 3 and 5 (528 and 530 nearly same)
  if (min(V) < 0) stop("Error: negative values found in frequency vector.")      # input validation
  # This error only occurs if the input is not a frequency vector. Use collapse() to generate a frequency vector.
  k <- length(V)  # number of values
  p <- NULL       # empty for preparation
  if (pos[1]==FALSE) if (is.numeric(pos) == FALSE) pos <- 1:k # no positions provided, assume 1:k (no zero)
  # this is old code for the mode if we use an expanded vector (not used)
  # (not a frequency vector): commented out
  #   u <- unique(V)  # remove duplicates of V
  #   m <- u[which.max(tabulate(match(V, u)))] # mode of V
  m <- which.max(V) # position of mode
  for (i in 1:k) {  # check each position to see if it is equal to the mode
    # because of the tolerance, frequencies need not be exactly the same
    if (isTRUE(all.equal(V[i],V[m],tolerance=tolerance))) p <- c(p,i) # add position of the mode
  }
  # Check whether modes are contiguous (= agreement) (c = {"contiguous", "divided"}
  if ((max(p) - min(p)) < length(p)) c <- TRUE else c <- FALSE
  r <- list(at = pos, frequencies = V, mode = pos[p], positions = p, contiguous = c)
  # at = positions or categories of vector, either given (argument "pos")
  #      or set to 1:k
  # frequencies = frequency vector V (provided)
  # mode = mode(s) of V, considering the categories of the vector
  # positions = position of V at which mode is found
  # contiguous = TRUE/FALSE if modes are contiguous
  return(r)
  }
