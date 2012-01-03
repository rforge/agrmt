polarization <-
function (V, old=FALSE) {
   # Calculates polarization (SOM Project)
   # Arguments: as Agreement function (V = frequnecy vector)
   p <- 2 - (agreement(V,old) + 1)  # pass arguments to agreement function, rescale
   return(p)
   }
