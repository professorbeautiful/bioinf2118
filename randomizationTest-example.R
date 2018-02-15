## randomizationTest-example

HairColorTable = matrix(c(3,2,5,90), nrow=2,byrow=F,
                        dimnames=list(c("R", "N"), c("D", "L"))
)
HairColorTable

cat("HairColor-Exact = ", fisher.test(HairColorTable)$p.value, "\n")
cat("HairColorPchisq = ", chisq.test(HairColorTable)$p.value, "\n")
###  note the warning about the test.

###  Rearrange for permutation:
HairColorDataFrame = data.frame(response=rep(c("R", "N"), times=c(8,92)))
HairColorDataFrame$hair=rep(c("D","L","D","L"), times=c(3,5,2,90))
HairColorDataFrame[sample(nrow(HairColorDataFrame), 15), ]
with(HairColorDataFrame, table(response, hair))
cat("HairColorPchisq = ", 
    HairColorPchisq<-(chisq.test(.Last.value)$p.value), "\n")

nReps = 30000
options(warn=-1)  ### Turn off warnings

myPvalue = function(ignoreMe, 
                    dfFunction=function()
                      with(data.frame(
                        response=sample(HairColorDataFrame$response), ### Use sample() to shuffle the responses.
                        hair=HairColorDataFrame$hair),
                        table(response,hair)
                      )
) {
  if(ignoreMe %% 1000 == 0 ) cat(ignoreMe %/% 1000, " ")
  tableTemp = dfFunction()
  x = tableTemp["R","D"]
  #print(tableTemp)
  c(P=chisq.test(tableTemp)$p., x=x)
}
randomizationResultsHyperGeom = sapply(1:nReps, myPvalue)
summarizeRandomization = function(randomizationResults){
  randomizationX = randomizationResults['x', ]
  print(table(randomizationX))
  randomizationPvalues = randomizationResults['P', ]
  print(table(randomizationPvalues))
  plot(randomizationX, randomizationPvalues)
  mean(randomizationPvalues <= HairColorPchisq, na.rm = TRUE)
}
summarizeRandomization(randomizationResultsHyperGeom)
## Close to the Fisher 'exact' test, because that was the sampling mechanism.
## Now let's try it with independent Poissons.
randomizationResultsPoisson = sapply(
  1:nReps, myPvalue, 
  dfFunction=function()
    apply(HairColorTable, 1:2, rpois, n=1)
    )
summarizeRandomization(randomizationResultsPoisson)
### What if only the sample size is fixed?
randomizationResultsMultinomial = sapply(
  1:nReps, myPvalue, 
  dfFunction=function(){
    HairColorTableTemp = HairColorTable
    probMatrix = outer(rowSums(HairColorTable), colSums(HairColorTable)) 
  HairColorTableTemp[] = rmultinom(
    1, sum(HairColorTableTemp), prob=probMatrix)
  HairColorTableTemp
  }
)
summarizeRandomization(randomizationResultsMultinomial)
##  0.00168 in one rep, 0.00100 in the next (N=3000)


cbind(table(randomizationResults),
      rev(table(randomizationPvalues)) )
### Compare with what chisq.test provides via simulation.
HairColorPchisq.Sim = chisq.test(HairColorTable, 
                                 simulate.p.value=T)$p.value
HairColorPchisq.Sim
?chisq.test  ### Aha, the default is B=2000.


### "parametric bootstrap"
## Let's create an interval for the odds ratio.
bootMyHairColor = function(ignoreMe) {
  #### Use the observed data frequencies to estimate the "truth".
  tableTemp = rmultinom(1, size=sum(HairColorTable), 
                        prob=HairColorTable/sum(HairColorTable))
  tableTemp = matrix(tableTemp, nrow=2)
  pvalue = chisq.test(tableTemp)$p.
  attr(pvalue, "table") = tableTemp
  pvalue
}
nReps = 3000
bootstrapPvalues = lapply(1:nReps, bootMyHairColor)
bootstrapPvalues.p =  sapply(bootstrapPvalues, invisible )
summary(bootstrapPvalues.p)
length(table(bootstrapPvalues.p))
plot(ecdf(bootstrapPvalues.p))
### NOTE this is the P value distribution under ~HA, not H0!
mean(bootstrapPvalues <= HairColorPchisq)
mean(bootstrapPvalues <= HairColorPchisq, na.rm=T)
attributes(bootstrapPvalues[is.na(bootstrapPvalues.p)][[1]])

options(warn=1)   ### Reinstate warnings

