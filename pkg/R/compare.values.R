compare.values <-
function(A,B,tolerance=0.1) {
  # Helper function: How do two values compare?
  # Arguments: A = value 1
  #            B = value 2
  #    tolerance = tolerance (in %)
  if (A > B) r <- -1 else r <- 1 # A is bigger: relationship -1; B is bigger: relationship 1
  if (isTRUE(all.equal(A,B,tolerance=tolerance))) r <- 0 # A and B around the same: relationship 0
  return(r)
  }
