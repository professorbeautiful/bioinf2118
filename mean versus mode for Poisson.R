### mean versus mode and median for Poisson
someLambdaValues = seq(0.1,10,by = 0.1)

argmax = function(f, set, ...) {
  maximizers = which(f(set, ...) ==  max(f(set, ...)))
  return(set[maximizers][1])
}

theModes = sapply(someLambdaValues,
                 function(lam)
                   argmax(f=dpois, 
                          set=floor(lam/5):lam, 
                          lambda=lam) [1]
)
                 
plot(someLambdaValues, theModes,
     xlab="mean", ylab="mode")
abline(a=0, b=1)
title("Poisson distribution\nmeans versus modes")

##  median:
theMedians = qpois(1/2, someLambdaValues)
plot(someLambdaValues, theMedians)
plot(theModes, theMedians)
abline(a=0, b=1)
title("Poisson distribution\nmodes versus medians")
