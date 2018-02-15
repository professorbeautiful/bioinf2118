Explorations in the one-sample normal test and t test.
===

We consider an experiment with i.i.d. normal data. 
```{r}

sigmaSq = 100
sigma = sqrt(sigmaSq)
n=31
alpha = 0.05
sdOfMean = sigma / sqrt(n)	

```
The critical value for normal test is 
```{r}


criticalValue = #  z(1-alpha) * sigma / sqrt(n)
	qnorm(1-alpha) * sdOfMean

```


(A)  Confirm that the critical value gives the right Type 1 error 
----
We validate on 10000 simulated data sets, each with 10 observations.
Each row is a dataset
```{r}
nreps = 10000   
simulatedDataSets = rnorm(n * nreps, mean = 0, sd= sqrt(sigmaSq))
simulatedDataSets = matrix(simulatedDataSets, nrow=nreps)

testStatistics = apply(simulatedDataSets, 1, mean)
```
In the "apply"  call, the "1" means keep the first dimension, the rows,
so we produce one result, one mean, per simulated data set.


What proportion of the testStatistics exceed the criticalValue?
```{r}

proportionRejected = mean(testStatistics > criticalValue)
print(proportionRejected)   ### It should be close to alpha, 0.05.

```


 proportionRejected * 10000  should be binomial(10000, alpha). Using the normal approximation,
 how many standard deviations away?
```{r}
(proportionRejected - 0.05) * nreps/ sqrt(alpha*(1-alpha)*nreps)
```
 not far from 0.05 in s.d's.

GOOOD!   looks close, usually less than 2 s.d.'s ( results will vary).


  (B)   Statistical power.   
----
  
```{r}
muAlternative = 0:13
power.at.muA = 1-pnorm(qnorm(1-alpha) - 
							muAlternative/(sigma/sqrt(n)))
plot(muAlternative, power.at.muA, ylim=0:1)
abline(h=0.05)

```

  (C)   Sample size requirement.  
---

```{r}


beta = 0.05    #### so power = 0.95
muA = 6
sumOfZs = qnorm(1-alpha) + qnorm(1-beta)
nRequired = (sumOfZs * sigma / muA)^2
print(nRequired)
nRequired = ceiling(nRequired)
```
Note the use of the function **ceiling**
nRequired > 30

Let's confirm that this N is enough, by simulation.

```{r}
simulatedDataSets.muA = matrix(ncol=nRequired,  rnorm(nreps*nRequired, muA, sigma))
testStatistics.muA = apply(simulatedDataSets.muA, 1, mean)
sdOfMean = sigma / sqrt(nRequired)	
criticalValue.31 = #  z(1-alpha) * sigma / sqrt(n)
	qnorm(1-alpha) * sdOfMean
mean(testStatistics.muA > criticalValue.31)
```

This is bigger than 1 - beta = 0.95, so the sample size nRequired has the required power.


(D)   Analyzing a dataset.  
---

 I want to pick out a dataset that happens to have a mean around 6; see "12-testing, part 2.doc".  Then we can see

```{r}

closeToSix = order(abs(6 - apply(simulatedDataSets.muA, 1, mean)))[1]
theDataset = simulatedDataSets.muA[ closeToSix, ]
Xbar = mean(theDataset)  ## That's close to 6.
Xbar
cat("P value = ",  1 - pnorm(Xbar/(sigma/sqrt(31))), "\n") 
```
Highly significant! 
The P value is the probability of the upper tail --  things more likely than our Xbar.

Compare with our simulated datasets:
```{r}

cat("# Observed significant | H0 = ", sum(apply(simulatedDataSets, 1, mean) >= Xbar), 
    "  compared to expected, ", (1 - pnorm(Xbar/sdOfMean))*10000, "\n")

``` 

(E)  One-sample one-sided t-test.   
---
If sigma is NOT known, we use the t statistic:
```{r}

tTestStatistic = function(X) #####  X is a vector of observations, i.i.d. normal. ######
{ 	sPrime = sd(X)   ### sumOfSquares divided by (n-1)
		n = length(X)
		### t =               standard normal  
		###         ---------------------------------------------
		###		     sqrt(standard chisquare / degrees-of-freedom) 
		### t = (mean(X)/(sigma/sqrt(n))) /
		#               sqrt( sumOfSquares/sigma^2 / (n-1))
		# The unknonw sigma cancels out top and bottom
		return( 
			( mean(X) * sqrt(n) ) /  ( sPrime)
		)
}
criticalValue = 
	qt(1-alpha, n-1)  # Compare with before:    z(1-alpha) X sigma / sqrt(n)
## Compare to the case where we know sigma:
cat("Critical values are \n")
c(infinity=qnorm(1-alpha), "31-1"=qt(1-alpha, 31-1), "10-1"=qt(1-alpha, 10-1))

```

####  Validate against simulations.
tTestStatistics = apply(simulatedDataSets, 1, tTestStatistic)
proportionRejected = mean(tTestStatistics > criticalValue)
print(proportionRejected)

######## On our dataset  with Xbar=6,  P-value is 
cat("Test statistic is ", 
	tTestStatistic(theDataset), "\n")
cat("P value = ", 
	1 - pt(tTestStatistic(theDataset), length(theDataset) - 1), "\n")

## Compare to the built-in function
t.test(theDataset)
t.test(theDataset, alternative="greater")
#####  This is significant.  

### With a sample size of only n=10, the power is insufficient.

simulatedDataSets.muA.n10 = matrix(ncol=10,  rnorm(nreps*nRequired, muA, sigma))
testStatistics.muA.n10 = apply(simulatedDataSets.muA.n10, 1, mean)
testStatistics.muA.n10 = apply(simulatedDataSets.muA.n10, 1, tTestStatistic)
proportionRejected.muA.n10 = mean(testStatistics.muA.n10 > criticalValue)
print(proportionRejected.muA.n10)





