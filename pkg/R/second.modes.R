second.modes <-
function(V, tolerance=0) {
  m1 <- modes(V)
  V2 <- V[-m1$positions]
  m2 <- modes(V2)
  m <- list(m1$mode, m2$mode)
  c <- list(m1$contiguous, m2$contiguous)
  r <- list(vector = V, mode = m, contiguous = c) # m = mode of V; p = position of V at which mode is found
  return(r)
  }
