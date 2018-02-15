##### Ridge regression

##  make some data, with a nearly collinear design matrix
samplesize = 50
sharedfactor <- rnorm(samplesize)
nFeatures = 2
x1 <- sharedfactor + rnorm(samplesize)* 0.001
x2 <- sharedfactor + rnorm(samplesize)* 0.001
b1 <- 1;   b2 <- 1
y <- b1*x1 + b2*x2 + rnorm(samplesize)
originaldata <- data.frame(y=y, x1=x1, x2=x2)

###### Standard linear model.
original.lm <- print(lm(y ~ x1 + x2, data = originaldata))

designmatrix <- as.matrix(originaldata[, -1])
XTX <- t(designmatrix)%*%designmatrix
svd(XTX)$d    
#### Eigenvalues:   to detect a badly conditioned matrix. 
#### When the ratio of largest to smallest is a big number,
#### there is a tendency to collinearity. 

##Ridge regression;  lambda = ridge size
lambdalist <-  c(0, 10^(-6:4))
resultNames <- c("b1hat","b2hat", "Sq Err", "deviance", "eig1", "eig2", "conditionNumber")
results <- matrix(NA, nrow=length(lambdalist), ncol=length(resultNames))
dimnames(results) <- list(as.character(lambdalist), resultNames)

for(lambda in lambdalist) {
  if(lambda == 0) ridgedata <- originaldata
  else {
    pseudodata <- data.frame(y=rep(0,nFeatures), 
                             x1=c(sqrt(lambda),0), x2=c(0,sqrt(lambda)))
    ridgedata <- rbind(originaldata, pseudodata)
    ## This will be the equivalent of adding a 'ridge', a constant lambda,
    ## to each diagonal element of X'X.
    ## This makes the matrix inversion more stable.
	}
  ridge.lm <- lm(y ~ x1 + x2, data = ridgedata)
	designmatrix <- as.matrix(ridgedata[, -1])
	XTX=t(designmatrix)%*%designmatrix
	results[as.character(lambda), "b1hat"] <- coef(ridge.lm)[2]
	results[as.character(lambda), "b2hat"] <- coef(ridge.lm)[3]
	results[as.character(lambda), "Sq Err"] <- 
	  round(digits=3, sum( (c(b1,b2) - coef(ridge.lm)[2:3])^2) )
	results[as.character(lambda), "deviance"] <- deviance(ridge.lm)
	results[as.character(lambda), c("eig1", "eig2")] <- svd(XTX)$d
	results[as.character(lambda), "conditionNumber"] <- round(digits=2, svd(XTX)$d[1] / svd(XTX)$d[2])
}
round(results, digits=3)
cat("Correct answer is b1 = ", 1, " ,  b2 = ", 1, "\n")
pairs(originaldata)
#### Plot the "complexity" trade-off.
plot(1:length(lambdalist), results[ , "Sq Err"], 
     xlab=expression(lambda), xaxt= "n", log="y", type="l")
axis(side = 1, at = 1:length(lambdalist), 
     labels = as.character(lambdalist))
title(expression(SqErr==(hat(beta[1])-beta[1])^2 + (hat(beta[2])-beta[2])^2))

### OVERFITTING

### Now, let's add in random features. 47 will give us a perfect fit.
nExtraFeatures = 47
nFeatures = nExtraFeatures+2
expandedData = cbind(originaldata, matrix(rnorm(47*samplesize), nrow=samplesize))
names(expandedData) = c(names(originaldata), paste('x', 3:nFeatures, sep=''))
expanded.lm = lm(y ~ ., data=expandedData)
sum(expanded.lm$res^2)
summary(expanded.lm$coefficients)

####  Does ridge regression help?
for(lambda in lambdalist) {
  if(lambda == 0) ridgedata <- expandedData
  else {
    pseudodata <- data.frame(y=rep(0,nFeatures), 
                             diag(rep(sqrt(lambda), nFeatures)))
    names(pseudodata) = names(expandedData)
    ridgedata <- rbind(expandedData, pseudodata)
  }
  ridge.lm <- lm(y ~ ., data = ridgedata)
  designmatrix <- as.matrix(ridgedata[, -1])
  XTX=t(designmatrix)%*%designmatrix
  results[as.character(lambda), "b1hat"] <- coef(ridge.lm)[2]
  results[as.character(lambda), "b2hat"] <- coef(ridge.lm)[3]
  results[as.character(lambda), "Sq Err"] <- 
    round(digits=3, sum( (c(b1,b2) - coef(ridge.lm)[2:3])^2) )
  results[as.character(lambda), "deviance"] <- deviance(ridge.lm)
  svdXTXd = svd(XTX)$d
      try(results[as.character(lambda), c("eig1", "eig2")] <- range(svdXTXd))
  results[as.character(lambda), "conditionNumber"] <- 
    round(digits=2, max(svdXTXd)/min(svdXTXd))
}


### How can we select the best shrinker?

fold = 10
testSetNumber = 1 + (0:(samplesize-1))  %% fold
table(testSetNumber)

cvPerformance = function(lambda) {
  performance = sapply( 1:fold, function(iFold) {
    trainingSet = expandedData [testSetNumber != iFold, ]
    testSet = expandedData [testSetNumber == iFold, ]
    if(lambda == 0) ridgedata <- trainingSet
    else {
      pseudodata <- data.frame(y=rep(0,nFeatures), 
                               diag(rep(sqrt(lambda), nFeatures)))
      names(pseudodata) = names(trainingSet)
      ridgedata <- rbind(trainingSet, pseudodata)
    }
    ridge.lm <- lm(y ~ ., data = ridgedata)
    predictions = predict.lm(object = ridge.lm, newdata = testSet)
    return(c(b1=ridge.lm$coef['x1'], b2=ridge.lm$coef['x2'], 
             performance=mean( (testSet$y - predictions)^2 )))
  })
  return(rowMeans(performance))
}

options.save = options(warn=-2)
cvResult = rbind(lambdalist, 
      sapply(lambdalist, cvPerformance)
      )
options(options.save)
plot(1:length(lambdalist), cvResult[ "performance", ], 
     xlab=expression(lambda), ylab="predictive mean sq error", 
     xaxt= "n", log="y", type="l")
axis(side = 1, at = 1:length(lambdalist), 
     labels = as.character(lambdalist))
title('cross-validation performance')
mtext(expression(E((Y-hat(Y))^2)))

