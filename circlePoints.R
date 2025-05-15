NPOINTS = 10
circlePoints = exp(1i * 2*pi * (1:NPOINTS)/NPOINTS)
circlePointsXY = cbind(Re(circlePoints), Im(circlePoints))
round(circlePointsXY, 3)
plot((-1):1, (-1):1, axes=F, pch=' ', xlab='', ylab='')
points(circlePoints, pch=' ')
# for(i in 1:NPOINTS)
#   lines(circlePointsXY[c(i,ifelse(i==NPOINTS, 1, i+1)),], col='grey', lty=2)
text(circlePoints[1], labels = paste('R',1), xpd=NA)
for(i in 1:(NPOINTS-1)) {
  for(j in (i+1):NPOINTS) {
    cat(i, ' ', j, '\n')
    nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
    lines(circlePointsXY[c(i, nextPoint),], 
          col=i, lty=1)
    text(circlePoints[nextPoint], 
         labels = paste('R',nextPoint), xpd=NA)
    profvis::pause(0.2)
  }
}
test = function() {
for(i in 1:(NPOINTS-1)) {
  for(j in (i+1):NPOINTS) {
    cat(i, ' ', j, '\n')
    nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
    lines(circlePointsXY[c(i, nextPoint),], 
          col='red', lty=3)
    text(circlePoints[nextPoint], 
         labels = paste('R',nextPoint), xpd=NA)
    readline()
  }
}
}
# text(circlePoints, labels = paste('R',1:NPOINTS), xpd=NA)
