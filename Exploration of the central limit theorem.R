####  Exploration of the central limit theorem.  
par(mfrow=c(3,3))
for(size in c(1, 3, 5, 8, 12, 15, 20, 50, 100))
	plot(0:size, dbinom(0:size, size, 0.5),
		main=size)

par(mfrow=c(1,1))
size=1000
plot(0:size, dbinom(0:size, size, 0.5),
		main=size,
		xlim=c(450,550))
lines(xtemp<-seq(450,550,length=1000),
	dnorm(xtemp, 500, sqrt(1000*(1/2)*(1/2))))

####

### The CLT is achieved if the variables are i.i.d. with finite variance.

rbinom1 = function(n) rbinom(n, 1, prob=0.01)
rpois1 = function(n) rpois(n, lambda=0.01)

CLT_hist = function(theDistribution = rbinom1,
                    sampleSize = 1000,
                    repetitions = 100,
                    breaks=25,
                    ...) {
  hist(rowMeans(matrix(
    theDistribution(repetitions*sampleSize, ...),
                       nrow=repetitions)),
       main=substitute(theDistribution), breaks=breaks  )
}

CLT_hist(sampleSize=1)
CLT_hist(sampleSize=10)
CLT_hist(sampleSize=10000)
CLT_hist(sampleSize=100000)
CLT_hist(sampleSize=1000000)

CLT_hist(rpois1, sampleSize=10)
CLT_hist(rpois1, sampleSize=1000)
CLT_hist(rpois1, sampleSize=100000)

CLT_hist(rcauchy, sampleSize=10)
CLT_hist(rcauchy, sampleSize=1000)
CLT_hist(rcauchy, sampleSize=100000)


