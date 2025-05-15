NPOINTS = 20
circlePoints = exp(1i * 2*pi * (1:NPOINTS)/NPOINTS)
circlePointsXY = cbind(Re(circlePoints), Im(circlePoints))
round(circlePointsXY, 3)
plot((-1):1, (-1):1, axes=F, pch=' ', xlab='', ylab='')
points(circlePoints, pch=' ')
# for(i in 1:NPOINTS)
#   lines(circlePointsXY[c(i,ifelse(i==NPOINTS, 1, i+1)),], col='grey', lty=2)
for(i in 1:NPOINTS) for(j in 1:NPOINTS)
  lines(circlePointsXY[c(i,ifelse(j==NPOINTS, 1, j+1)),], 
        col='grey', lty=3)
text(circlePoints, labels = paste('R',1:NPOINTS), xpd=NA)
