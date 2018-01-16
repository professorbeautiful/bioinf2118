###  prisonersPicnic- Chi-square tests and fisher tests

###  Is A.ate related to C.sick?

marginACdata = apply(prisonersPicnic.array.Observed, c(1,3), sum)

##  relative risk
riskIfAte = marginACdata["ate", "sick"]/sum(marginACdata["ate", ])
riskIfAteNot = marginACdata["ate not", "sick"]/sum(marginACdata["ate not", ])
relativeRisk = riskIfAte / riskIfAteNot
relativeRisk
optSave = options(digits=3); relativeRisk; 
options(optSave)

odds <- function(p) { 
  return(p/(1-p))
}
odds(riskIfAte)
odds(riskIfAteNot)
odds(riskIfAte) / odds(riskIfAteNot)   ### "odds ratio"
# When the denominators are large, odds = risk roughly.
# This is the basis for case-control studies, which estimate odds ratios.

marginACdata[1,1]*marginACdata[2,2]/marginACdata[1,2]/marginACdata[2,1]### "odds ratio"

c(chisq.test(marginACdata)$p.value,
  chisq.test(marginACdata, simulate.p.value=TRUE)$p.value)
fisher.test(marginACdata)  
### Yikes! This odds ratio estimate is a little different! 
#           Note that the conditional Maximum Likelihood Estimate (MLE) 
#           rather than the unconditional MLE (the sample odds ratio) is used. 
#           fisher.test only presents it in the 2 by 2 case.

###  What if the 3 variables ("features") are all unrelated?
marginA = apply(prisonersPicnic.array, 1, sum)
marginB = apply(prisonersPicnic.array, 2, sum)
marginC = apply(prisonersPicnic.array, 3, sum) 
prisonersPicnic.dataframe$Expected =
  SAMPLE_SIZE * with(prisonersPicnic.dataframe,
                     marginA[E.ate] * marginB[D.drank] * marginC[S.sick]
                     )
sum(prisonersPicnic.dataframe$Expected)
prisonersPicnic.dataframe$ObsMinusExp = prisonersPicnic.dataframe$Observed - prisonersPicnic.dataframe$Expected
prisonersPicnic.dataframe$Residuals = 
  prisonersPicnic.dataframe$ObsMinusExp/sqrt(prisonersPicnic.dataframe$Expected)
prisonersPicnic.dataframe
### These are the RESIDUALS. Examining them can help you find patterns.
### "It's the models that DON'T fit that teach you something."
