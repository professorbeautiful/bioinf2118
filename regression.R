####    regression.R     ####################

#   install.packages("mvtnorm")

require(mvtnorm)

K = 10 #  number of covariates
N = 10 #  Sample size
X = data.frame(rmvnorm(n=N, mean=rep(0,K)))   #  design matrix 
###  For now, the predictors are independent.
saveOpts = options(digits=3); X; options(saveOpts)

###  First generative model:  Y depends on X1 alone, linearly.  
beta0 = 5;  beta1 = 2
Y = beta0 + beta1 * X$X1 + rnorm(N)
simData = data.frame(Y, X)
plot(simData)
plot(X$X1, Y)
abline(beta0, beta1)  ### The "truth"
abline(v=0, lty=2, col="red")

lm.out = lm(Y ~ X1, data=simData)
lm.out

str(lm.out)
names(lm.out)
class(lm.out)
cbind(lm.out$coef, c(beta0, beta1)); 
## two more ways to extract the coefficients:
coef(lm.out);  coefficients(lm.out)
abline(coef(lm.out), lty=2, col="green",  lwd=4)  ### The "fit"

with(lm.out, {
	for(i in 1:N)
		lines(rep(X$X1[i], 2), 
			c(fitted.values[i],   fitted.values[i] + residuals[i]),  col="blue")
	}
)
legend(x="topleft", col=c("black","green","blue"), lty=c(1,2,1), lwd=c(1,2,1),
       legend=c("truth", "estimate", "residuals (obs)")
        )
simData$name = apply(matrix(sample(letters, 40, replace=T),  nrow=10), MARGIN=1, FUN=paste, 
      collapse="")
identify(simData$X1, simData$Y, labels=simData$name)
#  locator(1)
symbols(locator(1), rectangles=matrix(c(0.5, 1),nrow=1), add=T)

par(mfrow=c(2,2)); plot(lm.out, ask=FALSE)
par(mfrow=c(1,1));  plot(X$X1, resid(lm.out));  abline(h=0, lwd=2, lty=2, col="orange")
influence.measures(lm.out)

Q = sum(lm.out$residuals^2)   ## sum of squared residuals 

sigmaSqHat.biased =  Q/N
sigmaSqHat.unbiased =  Q/(N-2)
cat(
	"sigmaSqHat.biased = ",  sigmaSqHat.biased,
	"\nsigmaSqHat.unbiased = ", sigmaSqHat.unbiased,
	"\n"
)
abline(h=c(-1,1)*sqrt(sigmaSqHat.unbiased), col="blue")

######   Calculating lm.out$coef ourselves.
X = as.matrix(cbind(X0=rep(1,N), X))
X01 = X[ , 1:2]
myCoef = print(solve(t(X01) %*% X01) %*% t(X01) %*% Y)
lm.out$coef

##########    Now, "overspecified" model:  too many parameters!

lm.out.all = lm(data=simData, 
	Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
plot(simData$X1, resid(lm.out.all));
  abline(h=0, lwd=2, lty=2, col="orange")
resid(lm.out.all)
lm.out.all
sum(lm.out.all$residuals^2)   ## sum of squared residuals 

######  Investigating intercepts
X = as.matrix(X) 
lm(Y ~ X);	
lm(Y ~ X[ , -1]); 
lm(Y ~ -1 + X)

plot(lm(Y ~ -1 + X[ , -c(1, 3:11)]))

######   Multiple regression

beta = c(beta0, beta1, 
		beta1, beta1, rep(0, ncol(X)-3))

Y = beta0 + as.matrix(X) %*% beta[-1] + rnorm(N)
YX = data.frame(Y,X)
lm.out = lm(data=YX, Y ~ .)
resid(lm.out)

step(lm(data=YX, Y ~ 1), scope=Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9)
lm(data=YX, Y ~ . - X10)
resid(.Last.value)

for(k in (1:10)) {
	cat( "=======", k, "=======\n")
	theFormula = as.formula("Y ~  " %&%
			paste("X", 1:k, sep="", collapse=" + "))
	print(theFormula)
	lm.out = lm(data=YX, theFormula)
#	print(lm.out$coef)
	print(summary(lm.out))
}

randomKorder = sample(1:10)
for(k in 1:10) {
  columns = randomKorder[1:k]  
	lm.out = lm(Y ~ X[ , columns])
	Q = sum(lm.out$residuals^2)   ## sum of squares 
	sigmaHat.biased =  Q/N
	sigmaHat.unbiased =  Q/(N - (k+1))
	cat("k = ", k,	"columns", columns, "   sigmaHat.biased = ",  sigmaHat.biased,
							"   sigmaHat.unbiased = ", sigmaHat.unbiased,   "\n"	)
}

simData$G = sample(letters[1:3],size=10, replace=T)
simData$G = factor(simData$G)
simData$Y[simData$G=="a"] = simData$Y[simData$G=="a"] + 10
plot(simData$Y ~ simData$G)
smartBarPlot(simData$G, simData$Y)
lm(data=simData, Y ~ G)

