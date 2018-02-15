#### meanCoverageOfConfidenceIntervals.R 

cheeseX=c(0.86, 1.53, 1.57, 1.81, 0.99, 1.09, 1.29, 1.78, 1.29, 1.58)
mean(cheeseX); sd(cheeseX)

#  mu = 1.379;   sig = 0.3277;   n=10

simulateConfidenceInterval = function(
    X=cheeseX, CImethod=c("normal", "t"), alpha = 0.10,
    simMethod=c("normal","Bootstrap","cauchy"), 
    nsims=10000, nreps=1, mu = mean(X), sig = sd(X),
    plot=FALSE,
    reportTime=FALSE) {
  CImethod = CImethod[1]
  simMethod = simMethod[1]
  n = length(X)
  confLevel = 1 - alpha/2
  if(plot==TRUE) {
    plot(range(X), c(1,nsims), pch="")
    abline(v=mu)
    mtext(side = 3, "green: covers,  red: misses\n mean=mu")
  }
  timeReport = system.time( {
    simFunction = function(repNumber) {
      x = switch(simMethod,
                 normal=rnorm(n, mu, sig),
                 Bootstrap=sample(X, 10, replace=TRUE),
                 cauchy=rcauchy(n, mu, sig)
      )
      interval = 
        switch(CImethod,
               normal=mean(x)+
                 c(-1,1)*qnorm(confLevel)*sd(x)/sqrt(n),
               t     =mean(x)+
                 c(-1,1)*qt(confLevel,n-1)*sd(x)/sqrt(n)
        )
      covers = (mu > interval[1]) & (mu < interval[2])
      if(plot==TRUE) 
        lines(interval, rep(repNumber, 2), 
              col=c("red", "darkgreen")[(1+covers)])
      return(covers)   ## Boolean
    }
    for(rep in 1:nreps) {
      coverResult = sapply(1:nsims, simFunction)
      cat("1-alpha=", 1-alpha, "  coverage= ",  mean(coverResult), " CImethod=", CImethod, "   simMethod=", simMethod, "\n") 
    }
  })
  if(reportTime) return(timeReport) else return(invisible(NULL))
}


simulateConfidenceInterval(plot=TRUE, nsims=100)
simulateConfidenceInterval()    ### nsims=10000, the default
simulateConfidenceInterval(CImethod="t")
simulateConfidenceInterval(simMethod="Bootstrap")
simulateConfidenceInterval(CImethod="t", simMethod="Bootstrap")

simulateConfidenceInterval(CImethod="normal", simMethod="cauchy")
simulateConfidenceInterval(CImethod="t", simMethod="cauchy")
### Surprising result:  for cauchy, normal CI works better than t!

