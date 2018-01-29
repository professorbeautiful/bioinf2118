M = 1000   #### number of replicate data sets to construct
Nlist = c(1,10,100)   #### three sample sizes

distribExpressions = list(
  binomial = 'rbinom(n*M, size=1, prob=0.2)',
  poisson = 'rpois(n*M, 1)',
  exponential = 'rexp(n*M)',
  normal = 'rnorm(n*M)',
  cauchy = 'rcauchy(n*M)'
)

for (distribName in names(distribExpressions)) {
  theDensities = lapply(1:length(Nlist), function(i) {
    n = Nlist[i]
    # We use "eval" to turn the strings into results.
    simulations = eval(parse(text=distribExpressions[[distribName]]))
    simdatasets = matrix(simulations, nrow=n)
    simMeans = apply(simdatasets, 2, mean)
    density(simMeans, kernel="triangular", bw=0.02) 
    #str(simdatasets)
    #str(simMeans)
    # summary(simMeans)
    # summary(colMeans(simdatasets))  ### same thing.
    ### bw argument changes the bandwidth to make it more narrow.
  }
  )
  theXranges = sapply(theDensities, function(theDensity) range(theDensity$x))
  theYranges = sapply(theDensities, function(theDensity) range(theDensity$y))
  
  for(i in 1:length(Nlist)) {
    if(i==1)
      plot(theDensities[[i]],  type="b", col=2, pch=as.character(i)
           , xlim=c(min(theXranges),max(theXranges))
           , ylim=c(min(theYranges),max(theYranges)),
           main = distribName
      )
    else points(theDensities[[i]], type="b", col=i+1, pch=as.character(i))  
  }
}