ajusPlot <- function(V,tolerance=0.1) {
# plot AJUS with additional information
a <- ajus(V,tolerance)
l <- length(V)
m <- max(V)
plot(V, type="b", axes=FALSE, xlab="", ylab="")
axis(1, at=1:l)
axis(2)
text(1+l/5,m-m/10,paste("type",a$type, sep=" "),cex=2)
text(1+l/5,m-m/5,paste("skew",a$skew, sep=" "))
}

