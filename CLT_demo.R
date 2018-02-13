M = 1000            #### number of replicate data sets to construct
Nlist = c(1,10,100)   #### three sample sizes

distribExpressions = list(
  binomial = 'rbinom(n*M, size=1, prob=0.2)',
  poisson = 'rpois(n*M, 1)',
  exponential = 'rexp(n*M)',
  normal = 'rnorm(n*M)',
  cauchy = 'rcauchy(n*M)'
)
evaluateADistribution = 
  function(whichSampleSize, distribution, standardize=FALSE,
           adjust=1) {
    sampleSize = Nlist[whichSampleSize]
    # We use "eval" to turn the strings into results.
    simulations = eval(parse(text=distribExpressions[[distribution]]))
    simdatasets = matrix(simulations, nrow=sampleSize)
    simMeans = apply(X = simdatasets, MARGIN = 2, FUN = mean)
    if(standardize)
      simMeans = (simMeans-mean(simMeans)) / sd(simMeans)
    density(simMeans, kernel="triangular", adjust=adjust) 
    ### The bw argument changes the bandwidth to make it more narrow.
    #str(simdatasets)
    #str(simMeans)
    # summary(simMeans)
    # summary(colMeans(simdatasets))  ### same thing.
  }
simulateCLT = function(distribName, standardize=FALSE, 
                       theXranges, theYranges, adjust=1, ...) {
  theDensities = lapply(
    X = 1:length(Nlist), 
    FUN = evaluateADistribution,
    standardize = standardize,
    distribution = distribName
  )
  if (missing(theXranges))
    theXranges = sapply(theDensities, function(theDensity) range(theDensity$x))
  if (missing(theYranges))
    theYranges = sapply(theDensities, function(theDensity) range(theDensity$y))
  
  for(whichSampleSize in 1:length(Nlist)) {
    if(whichSampleSize==1)
      plot(theDensities[[whichSampleSize]],  type="b", 
           col=2, pch=as.character(whichSampleSize)
           , xlim=c(min(theXranges),max(theXranges))
           , ylim=c(min(theYranges),max(theYranges)),
           main = paste(distribName, '\n',
            distribExpressions[[distribName]]),
           ...=...
      )
    else points(theDensities[[whichSampleSize]], type="b", 
                col=whichSampleSize+1, pch=as.character(whichSampleSize))  
  }
  legend('topright', col=1:(length(Nlist)+1) , legend = c("n", Nlist),
         pch=c("", as.character(1:length(Nlist))))
}

simulateCLT('binomial')
simulateCLT('poisson')
simulateCLT('exponential')
simulateCLT('exponential', theXranges=c(0,3))
simulateCLT('exponential', standardize = TRUE)
simulateCLT('normal')
simulateCLT('cauchy')
