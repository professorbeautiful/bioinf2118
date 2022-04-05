##  PrisonersPicnic-linearmodel-simulation
## NOT FINISHED!

#  made-up model and data, Aug 1974, Streptococcal Pharyngitis Outbreak at a Florida Jail
coefficients = data.frame(b0 = 0.1, b1=1, b2=1.2, b1b2=0.7)
formula(~ x1 + x2 + x1*x2)
AD = expand.grid(A=0:1, D=0:1)
linPred = with(coefficients, 
  b0 + b1*AD$A + b2*AD$D + b1b2*min(AD$A, AD$D))
odds.to.prob = function(odds) odds/(1+odds)
cbind(AD, linPred, odds.to.prob(linPred))
#allOdds = 
prisonersPicnic.model = normalize( odds.to.prob(linPred))
names(prisonersPicnic.model) =  c('--','A-', '-D', 'AD')
sampleSize = 100
counts = rmultinom(n=5, sampleSize, prisonersPicnic.model)
