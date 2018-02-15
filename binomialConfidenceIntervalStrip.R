nFlips = 10
thVec = seq(0,1,length=1000)
sampleSpace = 0:nFlips
plot(sampleSpace, sampleSpace/nFlips, xlab="# heads", ylab="theta")
alpha = 0.05
title(expression( paste("Set product:    ", bolditalic(X) ~~ X ~~ Phi ) ))
for(theta in thVec) {
  upperTailProb = 1-pbinom(q = (0:nFlips) - 1, size = nFlips, prob = theta)
  valuesTooBig = (upperTailProb < alpha/2)
  points(sampleSpace, rep(theta, nFlips+1), pch=valuesTooBig+1, cex=valuesTooBig)
  lowerTailProb = pbinom(q = (0:nFlips), size = nFlips, prob = theta)
  valuesTooSmall = (lowerTailProb < alpha/2)
  points(sampleSpace, rep(theta, nFlips+1), pch=valuesTooSmall+1, cex=valuesTooSmall)
  acceptanceRegion = c(max(0, sampleSpace[valuesTooSmall]), 
                       min(nFlips,sampleSpace[valuesTooBig]))
  lines(acceptanceRegion, rep(theta, 2),  col='yellow')
}  
for(k in 0:nFlips) 
  lines(x = c(k,k), y=binom.confint.new(k, nFlips), col="lightgreen", lwd=2)

nHeads = 8
lines(x = c(nHeads,nHeads), y=binom.confint.new(nHeads, nFlips), col="green", lwd=4)
