
### This is Table 1.
diagnosticModelFamily = rbind(c(0.95, 0.03, 0.02), 
                   c(0.03, 0.95, 0.02)) 


prior = c(0.9, 0.1)
### This is Table 2.
diagnosticJoint = matrix(prior, nrow=2, ncol=3) *
                  diagnosticModelFamily
dimnames(diagnosticJoint) = list( c("healthy","sick"),c("negative","positive","indeterminate"))
print(diagnosticJoint)


#### This is Table 3.
normalize = function(x) x/sum(x)
predictiveProbabilities = apply(diagnosticJoint, 2,
                                normalize)
###   "2" means columns  and "1" means rows.

######  Now we will convert this to a Rmarkdown doc, and expand it.
###### See simulateDiagnosticData.Rmd instead.