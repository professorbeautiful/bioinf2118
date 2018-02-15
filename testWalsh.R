sigX = 10
sigY = 2

m = 100
n = 70
MEAN = 500

testWalsh = function(rep) {
  if(rep %% 100 == 0) cat(rep %/% 100, " ")
  dataX = rnorm(n = m, mean = MEAN, sd = sigX)
  dataY = rnorm(n = n, mean = MEAN, sd = sigY)
  
  t.test(x = dataX, y = dataY, alternative = 'two.sided',
         mu = 0, paired = FALSE, var.equal = FALSE) $ p.value
}

NREPS = 1e5

results = sapply( 1:NREPS, testWalsh)
summary(results)
plot(density(results[results < 0.05]))
qqplot(results, runif(1e5), log="xy")
