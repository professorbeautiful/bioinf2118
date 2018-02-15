prior = c(0.9, 0.1)
diagnosticModelFamily = rbind(c(0.95, 0.03, 0.02), 
                   c(0.03, 0.95, 0.02)) 
diagnosticJoint = matrix(prior, nrow=2, ncol=3) *
         diagnosticModelFamily
dimnames(diagnosticJoint) = list( c("healthy","sick"),c("negative","positive","indeterminate"))

sampleSize = 100
simulateHealthyPos = function(ignoreMe=NULL){
  simdata = rmultinom(1, sampleSize, diagnosticJoint)
  simdata = matrix(simdata, nrow=2)
  dimnames(simdata) = dimnames(diagnosticJoint) 
  simdata["healthy", "positive"]
}

nReps = 1000
healthyPosSample = sapply(1:nReps, simulateHealthyPos)
hist(healthyPosSample)
table(healthyPosSample)


