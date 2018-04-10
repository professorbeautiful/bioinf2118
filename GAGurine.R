require(MASS)
data(GAGurine)
?GAGurine
head(GAGurine)
dim(GAGurine)
names(GAGurine)
summary(GAGurine$Age)
summary(GAGurine$GAG)
with(GAGurine, table(Age < median(Age), 
                     GAG < median(GAG),
     dnn=c("Age<median", "GAG<median")) )

### Open a related article in the browser.
### (GAG: glycosaminoglycans)
GAGurineArticle =  #"http://rheumatology.oxfordjournals.org/content/50/suppl_5/v41.full"
  "https://academic.oup.com/rheumatology/article/50/suppl_5/v41/1778437"
if(regexpr("^win", version$os) == 1) {
  system(paste("start", GAGurineArticle))
} else
  system(paste("open", GAGurineArticle))

with(GAGurine, plot(Age, GAG))

GAG.lm = lm(GAG ~ Age, data=GAGurine)
class(GAG.lm)
summary(GAG.lm)
anova(GAG.lm)

lines(GAGurine$Age, predict(GAG.lm), pch="x", type="b", col="blue")

source('lines.loess.R')
with(GAGurine, lines.loess(Age, GAG, col="red", lwd=2))

par(mfrow=c(2,2))
plot(GAG.lm)


##########  Let's try logging GAG

GAGurine$logGAG = log10(GAGurine$GAG)
par(mfrow=c(1,1))
with(GAGurine, plot(Age, logGAG))

logGAG.lm = lm(logGAG ~ I(Age), data=GAGurine)
summary(logGAG.lm)
anova(logGAG.lm)

lines(GAGurine$Age, predict(logGAG.lm), pch="x", type="b", col="blue", lwd=3)

with(GAGurine, lines.loess(Age, logGAG, col="red", lwd=2))

par(mfrow=c(2,2))
plot(logGAG.lm)
## observation 314 is labeled most extreme.  
## May have too much effect.

### The boxcox transformation family includes powers and the log.
boxcox = function(x, p) {
  if(p==0)   
    #### Special case-- remember L'Hopital's rule for limit of a ratio?
    return(log(x))
  else
    return((x^p - 1)/p)
}
boxcox.inverse = function(y, p) {
  if(p==0)   
    #### Special case-- remember L'Hopital's rule for limit of a ratio?
    return(exp(y))
  else
    return(   (p*y + 1)^(1/p))
}
#### Test:
boxcox.inverse(boxcox(c( 0.1, 1, 10), p=0), p=0)

par(mfrow=c(1,1))
dataRangeX = seq(0, 2, by=0.01)
dataRangeY = seq(-2, 2, by=0.01)
plot( range(dataRangeX), range(dataRangeY),pch="")
for(p in seq(-2,2, by = .1) ) {
  lines(dataRangeX, boxcox(dataRangeX, p), type="l")
}
lines(dataRangeX, boxcox(dataRangeX, p=0), col='red', lwd=2)
lines(dataRangeX, boxcox(dataRangeX, p=1), col='blue', lwd=2)
lines(dataRangeX, boxcox(dataRangeX, p=2), col='green', lwd=2)
title('BoxCox: p in seq(-2,2, by = .1)')
legend('topleft', legend = c("p = 0 (log)", "p = 1 (linear)", "p = 2 (quadratic)"),
       text.col = c('red', 'blue', 'green'))

par(mfrow=c(2,3))
BoxCoxregression = function(p, verbose=FALSE){
  Ytemp = boxcox(GAGurine$GAG, p)
  pGAG.lm = lm( Ytemp ~ Age, data=GAGurine)
  predictions = boxcox.inverse(predict(pGAG.lm), p=p) 
  mse=mean( (GAGurine$GAG - predictions)^2 )
  logLik = logLik(pGAG.lm)
  if(verbose)  print(c(p, mse, logLik))
  return(list(predictions=predictions, mse=mse, logLik=logLik))
}
meanSquaredErrors = sapply(seq(-1.4, -0.2, length=6), function(p) {
  plot(GAGurine$Age, GAGurine$GAG)
  BCresult = BoxCoxregression(p)
  predictions = BCresult$predictions
  mse = BCresult$mse
  logLik = BCresult$logLik
  lines(GAGurine$Age, 
        predictions, 
        pch="x", type="b", col="blue", lwd=3)
  title(paste("p = ", p))
  ###  getS3method('logLik', 'lm')    #### not comparable, since the free parameter is p
  return(c(p, mse ) )
})
par(mfrow=c(1,1))
optimum = optimize(
  function(p) BoxCoxregression(p, verbose=TRUE)$mse, interval = c(-1.4, 0.2))
plot(t(meanSquaredErrors))
points(optimum$minimum, optimum$objective, col='red', pch="X")

### Please note the difference between:
####  (A) transforming the TARGET variable (which we did here)
####  (B) transforming the PREDICTOR
####  Linear regression assumes HOMOSKEDASTICITY: 
####    equal variance regardless of the expected value. 
####  Check using plot.lm.
