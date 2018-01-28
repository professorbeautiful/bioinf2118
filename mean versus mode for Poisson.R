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
                 

##  Where is D(log(dpois)/dk) = 0? log(lam) = digamma((k+1)) ?
theModesD =  sapply(someLambdaValues, function(lam) {
  if (lam < 1) densityargmax = 0
  else
    densityargmax = uniroot(
      f = function(k) digamma(k+1) - log(lam), 
      interval = c(lam/5000, lam*5000))$root
  candidates = floor(densityargmax) + 0:1
  mode = argmax(dpois, candidates, lambda=lam) 
  return(mode)
})
plot(someLambdaValues, theModes,
     xlab="mean", ylab="mode")
abline(a=0, b=1)
title("Poisson distribution")
plot(theModes, theModesD)
