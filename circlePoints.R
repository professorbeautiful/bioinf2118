NPOINTS = 10
circlePoints = -exp(-1i * 2*pi * (0:(NPOINTS-1))/NPOINTS)
circlePointsXY = cbind(Re(circlePoints), Im(circlePoints))
round(circlePointsXY, 3)

### MOVIE omnibus factorization
plot((-1):1, (-1):1, axes=F, pch=' ', xlab='', ylab='')
# text(circlePoints, labels = paste('R', seq(along=circlePoints)), xpd=NA, cex=2)
### ADD LINES to reflect the omnibus factorization
text(circlePoints[1], labels = paste('R',1), xpd=NA, cex=2)
for(i in 2:(NPOINTS)) {
  color = switch(paste0('c',i), `c9`='green', `c10`='orange', i)
  text(circlePoints[i], labels = paste('R',i), xpd=NA, cex=2,
       col=color )
  # text(circlePoints[i-1], labels = paste('R',i-1), xpd=NA)
  for(j in (1):(i-1)) {
    cat(i, ' ', j, '\n')
    #nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
    lines(circlePointsXY[c(i, j),],
          col=color, lty=1)
    # text(circlePoints[nextPoint],
    #      labels = paste('R',nextPoint), xpd=NA, cex=2, col=i)
    profvis::pause(0.4)
  }
}

## Markov chain
plot((-1):1, (-1):1, axes=F, pch=' ', xlab='', ylab='')
circlePointsPushed = circlePoints  * 1.15
circlePointsPushed[10] = circlePointsPushed[10] * exp(2*pi*1i * (-0.015))
text(circlePointsPushed[1], labels = paste('R',1), xpd=NA, cex=2)
colors = 1:NPOINTS
colors[9] = 'green'
colors[8] = 'orange'
for(i in 2:(NPOINTS)) {
  arrows(Re(circlePoints[i-1]), Im(circlePoints[i-1]), 
         Re(circlePoints[i]), Im(circlePoints[i]), 
         xpd=NA, lwd=3, col=colors[i-1])
  text(circlePointsPushed[i], labels = paste('R',i), xpd=NA, cex=2,
       col=colors[i] )
  profvis::pause(0.2)
  
}
# 
# 
# for(i in 1:(NPOINTS-1)) {
#   text(circlePoints[i], 
#        labels = paste('R',i), xpd=NA)
#   for(j in (i+1):NPOINTS) {
#     cat(i, ' ', j, '\n')
#     nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
#     lines(circlePointsXY[c(i, nextPoint),], 
#           col=j, lty=1)
#     # text(circlePoints[nextPoint], 
#     #      labels = paste('R',nextPoint), xpd=NA)
#     profvis::pause(0.2)
#   }
# }
### and again
# for(i in 1:(NPOINTS)) 
#   text(circlePoints[i], 
#        labels = paste('R',i), xpd=NA, cex=2)
# 
# triangle = intToUtf8(0x25B2) 
# for(srt in 0:4)
# text(0.2+srt/10,.6, triangle, cex=6,srt=srt*90/4)
# newArrow = function(x=c(0,0.8), y=c(0,.8), lwd=1, ...) {
#   print(x); print(y)
#   lines(x=x, y=y, lwd=lwd, ...)
#   text(x[2], y[2], triangle, cex=lwd, srt=360/2/pi/atan(diff(y)/diff(x)), ...)
# }
# newArrow(x=c(0.8,0.4), y=c(0,.4), lwd=2, col='blue')
# test = function() {
#   for(i in 1:(NPOINTS-1)) {
#     for(j in (i+1):NPOINTS) {
#       cat(i, ' ', j, '\n')
#       nextPoint =  ifelse(j==(NPOINTS+1), 1, j)
#       lines(circlePointsXY[c(i, nextPoint),], 
#             col='red', lty=3)
#       text(circlePoints[nextPoint], 
#            labels = paste('R',nextPoint), xpd=NA)
#       readline()
#     }
#   }
# }
# text(circlePoints, labels = paste('R',1:NPOINTS), xpd=NA)
