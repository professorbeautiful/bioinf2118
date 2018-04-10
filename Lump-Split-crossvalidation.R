####   Lump-Split-crossvalidation
####    Illustrating cross-validation with the Dark-Light dataset

DLdata = matrix(c(3,5,2,90),nrow=2)
dimnames(DLdata) = list(feature=c("D","L"), outcome=c("R","N"))
DLlong = data.frame(expand.grid(dimnames(DLdata)))
DLlong$n = c(DLdata)

leaveOneOut = function(weight=1/2, feature, outcome) {
  smallerDataSet = DLdata
  smallerDataSet[feature=feature, outcome=outcome] =
    smallerDataSet[feature=feature, outcome=outcome] - 1
  proportionOverall = sum(smallerDataSet[ , 'R'])/sum(smallerDataSet)
  proportionThisGroup =  sum(smallerDataSet[ feature, 'R'])/sum(smallerDataSet[feature, ])
  prediction = weight*proportionOverall + (1-weight)*proportionThisGroup
  penalty = switch(outcome, R=(1-prediction)^2, N=prediction^2)
  return(penalty * DLdata[feature, outcome])
}

totalPenalty = function(weight)
  leaveOneOut(weight=weight, feature='D', outcome='R') +
  leaveOneOut(weight=weight, feature='D', outcome='N') +
  leaveOneOut(weight=weight, feature='L', outcome='R') +
  leaveOneOut(weight=weight, feature='L', outcome='N')

penaltyVector = sapply(weights, totalPenalty)
plot(x=(weights<-seq(0,1,length=100)),
     y=penaltyVector,
     xlab='weights',
     ylab="",
#     ylim=c(0,max(penaltyVector)),
     type='l', col='red', axes=F)
axis(1)
axis(2, col='red', col.axis='red')
mtext(text = 'penalty', side = 2, col='red',line=2)
optimalWeight = weights[which(penaltyVector==min(penaltyVector))]
cat('optimalWeight = ', optimalWeight, '\n')
proportionOverall = sum(DLdata[ , 'R'])/sum(DLdata)
proportionThisGroup =  sum(DLdata[ 'D', 'R'])/sum(DLdata['D', ])
optimalPrediction = optimalWeight*proportionOverall + (1-optimalWeight)*proportionThisGroup
cat('optimal prediction for dark = ', optimalPrediction, '\n')
points(optimalWeight, totalPenalty(optimalWeight), col='red', pch=17)
abline(h=min(penaltyVector),col='red')
title('cross-validation optimum', 
      'split <----------------------------> lump')

par(new=T)
plot(weights, weights*0.08+(1-weights)*0.60, axes=F, type='l', lty=2,
      col='green', ylab='')
axis(4, col='green', col.axis='green')
mtext('estimate Pr(R|D)', side = 4, col='green', line = -1) 
