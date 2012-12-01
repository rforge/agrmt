ajusCheck <- function(V, t=seq(from=0.05, to=0.2, by=0.05)) {
# sensitivity check for AJUS (how does chaning the tolerance parameter affect results)
# arguments: V: vector describing the distribution
#            t: vector of tolerance values to test
# type:
type <- sapply(t, function(x) ajus(V,tolerance=x)$type)
# same for skew:
skew <- sapply(t, function(x) ajus(V,tolerance=x)$skew)
r <- list(tolerance = t, type = type, skew = skew)
return(r)
}

