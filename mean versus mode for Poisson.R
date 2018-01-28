### mean versus mode for Poisson
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
title("Poisson distribution")
