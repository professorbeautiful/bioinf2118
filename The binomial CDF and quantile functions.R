####  2011-02-01
###  The binomial CDF and quantile functions

###  the binomial CDF
pbinom(0:10, 10, 0.4)
plot(stepfun(0:10, c(0, .Last.value)),	main="CDF = F(x), pbinom", ylab="F(x)")


####  the binomial quantile function
options("device")$device()  ### new graphics device 
ptemp<-seq(0,1,length=101)
plot(ptemp, qbinom(ptemp, 10, 0.4), type="b",	main="Quantile function = F^(-1)(p) = qbinom")

### Notice that the two graphics are essentially mirror images.

####  quantile function
qbinom(
	pbinom(0:10, 10, 0.4),
	10, 0.4)
### This shows that pbinom=F(X) and qbinom=F^(-1)(X) are inverse functions.

  
