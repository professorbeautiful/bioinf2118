require(MASS)
data(GAGurine)
?GAGurine
head(GAGurine)
dim(GAGurine)
summary(GAGurine$Age)
summary(GAGurine$GAG)
with(GAGurine, table(Age < median(Age), 
                     GAG < median(GAG)),
     dnn=c("Age<median", "GAG<median"))

### Open a related article in the browser.
if(regexpr("^win", version$os) == 1) {
  system("start http://rheumatology.oxfordjournals.org/content/50/suppl_5/v41.full")
} else
  system("open http://rheumatology.oxfordjournals.org/content/50/suppl_5/v41.full")

with(GAGurine, plot(Age, GAG))


GAG.lm = lm(GAG ~ Age, data=GAGurine)
class(GAG.lm)
summary(GAG.lm)
anova(GAG.lm)

lines(GAGurine$Age, predict(GAG.lm), pch="x", type="b", col="blue")

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

boxcox = function(x, p) {
  if(p==0)
    return(log(x))
  else
    return((x^p - 1)/p)
}

dataRangeX = seq(0, 2, by=0.01)
dataRangeY = seq(-2, 2, by=0.01)
plot( range(dataRangeX), range(dataRangeY),pch="")
for(p in seq(-2,2, by = .1) ) {
  lines(dataRangeX, boxcox(dataRangeX, p), type="l")
}
lines(dataRangeX, boxcox(dataRangeX, p=0), col='red', lwd=2)
lines(dataRangeX, boxcox(dataRangeX, p=1), col='green', lwd=2)

loglikelihoods = sapply(seq(-5,1, by = 1), function(p) {
  Ytemp = boxcox(GAGurine$GAG, p)
  pGAG.lm = lm( Ytemp ~ Age, data=GAGurine)
  plot(GAGurine$Age, GAGurine$GAG)
  lines(GAGurine$Age, predict(pGAG.lm), pch="x", type="b", col="blue", lwd=3)
  title("p = ", p)
  c(p, logLik(pGAG.lm))
})
plot(t(loglikelihoods))
