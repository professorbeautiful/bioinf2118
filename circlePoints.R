NPOINTS = 10
circlePoints = exp(1i * 2*pi * (1:NPOINTS)/NPOINTS)
circlePointsXY = cbind(Re(circlePoints), Im(circlePoints))
round(circlePointsXY, 3)
plot((-1):1, (-1):1, axes=F, pch=' ', xlab='', ylab='')
points(circlePoints, pch=' ')
# for(i in 1:NPOINTS)
#   lines(circlePointsXY[c(i,ifelse(i==NPOINTS, 1, i+1)),], col='grey', lty=2)
text(circlePoints[1], labels = paste('R',1), xpd=NA)
# for(i in 1:(NPOINTS-1)) {
#   for(j in (i+1):NPOINTS) {
#     cat(i, ' ', j, '\n')
#     nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
#     lines(circlePointsXY[c(i, nextPoint),], 
#           col=i, lty=1)
#     text(circlePoints[nextPoint], 
#          labels = paste('R',nextPoint), xpd=NA)
#     profvis::pause(0.2)
#   }
# }

for(i in 1:(NPOINTS-1)) {
  text(circlePoints[i], 
       labels = paste('R',i), xpd=NA)
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
triangle = intToUtf8(0x25B2) 
for(srt in 0:4)
text(0.2+srt/10,.6, triangle, cex=6,srt=srt*90/4)
newArrow = function(x=c(0,0.8), y=c(0,.8), lwd=1, ...) {
  print(x); print(y)
  lines(x=x, y=y, lwd=lwd, ...)
  text(x[2], y[2], triangle, cex=lwd, srt=360/2/pi/atan(diff(y)/diff(x)), ...)
}
newArrow(x=c(0.8,0.4), y=c(0,.4), lwd=2, col='blue')
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
