###  ESLII  simulation data, ornage albue are each 10 mixtures.
### https://web.stanford.edu/~hastie/ElemStatLearn/
###  https://web.stanford.edu/~hastie/ElemStatLearn/datasets/mixture.example.info.txt 
load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))
str(ESL.mixture)
## The data matrix is ESL.mixture$x
## The label/target/dependent variable is ESL.mixture$y.

### First lets draw contours of the true distributions:

# BLUE = 0; ORANGE =  1;  
ESL.mixture$meancolor = rep(c('blue', 'orange'), each=10)
ESL.mixture$datacolor = sapply(as.character(ESL.mixture$y), 
                               switch, '0'='blue', '1'='orange')
### Plot the means of the means
plot( 1:0, 0:1, col=c('blue', 'orange'), pch=15, cex=2,
      xlim=range(ESL.mixture$x[ , 1]), xlab='X',
      ylim=range(ESL.mixture$x[ , 2]), ylab='Y')
### Plot the means
points(ESL.mixture$means, 
       #col=rep(c('blue', 'orange'), each=10),
       col=ESL.mixture$meancolor,
       pch=17)
title("ESL.mixture")
69*99 ### 6831, the length of the prob vector
contour(ESL.mixture$px1, ESL.mixture$px2, 
        matrix(ESL.mixture$prob, nrow=69), add=T)
text(1, -2, "Prob of being ORANGE conditional on (X,Y)",
     cex=0.8)
pointGrid = expand.grid(ESL.mixture$px1, ESL.mixture$px2)
boundaryPoints = pointGrid [ abs(ESL.mixture$prob - 0.5) < 0.03 ,]
head(boundaryPoints)
points(boundaryPoints, col='green', pch='.', cex=5)
contourvalues = contourLines(ESL.mixture$px1, ESL.mixture$px2, 
                        matrix(ESL.mixture$prob, nrow=69))
str(contourvalues)
contourlevels = print(sapply(contourvalues, `[[`, 'level') )
contourvalues.half = print(contourlevels==0.5)
sapply(which(contourvalues.half), 
       function(contourNumber) {
         lines(contourvalues[[contourNumber]], lwd=3, col='green')
       }
)

### and finally, the actual data:
points(ESL.mixture$x,
     col=c('blue', 'orange')[1+ESL.mixture$y])

### We can verify our posterior probabilities against ESL.mixture$prob.
require(mvtnorm)

errorCovar = diag(nrow=2) * (1/5)
pdfJ0 = function(xvalue) sum(
  apply(X = ESL.mixture$means[1:10, ], MARGIN = 1,
         FUN=dmvnorm, x=xvalue, sigma = errorCovar, log=FALSE) 
)
pdfJ1 = function(xvalue) sum(
  apply(X = ESL.mixture$means[11:20, ], MARGIN = 1,
         FUN=dmvnorm, x=xvalue, sigma = errorCovar, log=FALSE) 
)
oddsJ1 = apply(pointGrid, 1,
               function(x) pdfJ1(x)/pdfJ0(x) )
plot (ESL.mixture$prob, oddsJ1/(1+oddsJ1))
###  Oboy, they agree!
