### prisonersPicnic- data preparation.R

### Data:  from "02-More Probability.docx"
### Exercise: from "15-categorical data.docx"

#H1: Ate, Drank, and Sick are independent.
#H2: Ate and Drank are independent conditional on Sick.
#H3: Ate, Drank, and Sick may be pairwise associated, but there is no “3-way interaction”.

#  First, select the 8 data cells from the document, and "copy".

prisonersPicnic = strsplit('
0.48  0.15  0.06 	0.17 
0.04 	0.01 	0.02 	0.07 
', split='[\n \t]+') [[1]] [-1]
prisonersPicnic =  as.numeric(prisonersPicnic)
#prisonersPicnic = matrix(prisonersPicnic, nrow=2, byrow=TRUE)

## Put into a data frame. 
### Looking across the rows,
## "sick" changes fastest, then "ate", then "drank".
prisonersPicnic.dataframe = 
	expand.grid( 
		S.sick=c("sick","sick_not"), 
		A.ate=c("ate","ate_not"),
		D.drank=c("drank","drank_not") )
prisonersPicnic.dataframe$proportion = c(prisonersPicnic)
pP = as.data.frame( prisonersPicnic.dataframe[1:4])
##  oy.   fixed order 2022-01-24
####  Reshaping the data in the form of an array:
prisonersPicnic.array = 
  array(prisonersPicnic, 
        dim=c(2,2,2), 
        dimnames=list(
          S.sick=c("sick","sick_not"), A.ate=c("ate","ate_not"), 
          D.drank=c("drank","drank_not")) )

### And reshape the array...
prisonersPicnic.array = aperm(prisonersPicnic.array, c("A.ate", "D.drank", "S.sick"))
### Spot-check again:
prisonersPicnic.array ["ate_not", "drank", "sick_not"]
prisonersPicnic.array ["ate", "drank_not", "sick_not"]
save(prisonersPicnic.array, file = "prisonersPicnic.array.rdata")
pPa = prisonersPicnic.array
probs_sick = apply(pPa, 3, sum)
odds_sick = probs_sick[1]/probs_sick[2]
options(digits=3)
odds_by_A = sapply(c("ate", "ate_not"), 
                   function(A)
                     sum(pPa[A,,"sick"]) /
                     sum(pPa[A,,"sick_not"])
)
odds_by_D = sapply(c("drank", "drank_not"), 
                   function(D)
                     sum(pPa[,D,"sick"]) /
                     sum(pPa[,D,"sick_not"])
)
# model:

model = pPa
model[,,"sick"] = model[,,"sick"] / probs_sick["sick"]
model[,,"sick_not"] = model[,,"sick_not"] / probs_sick["sick_not"]

# likelihood ratios:
Lik_for_drank = apply(model, 2:3, sum)["drank",]
LR_for_drank =Lik_for_drank[1]/Lik_for_drank[2]
Lik_for_ate = apply(model, c(1,3), sum)["ate",]
LR_for_ate =Lik_for_ate[1]/Lik_for_ate[2]

Lik_for_ate_drank = model["ate","drank",]
LR_for_ate_drank =Lik_for_ate_drank[1]/Lik_for_ate_drank[2]

c(LR_for_ate_drank, LR_for_drank*LR_for_ate)

odds_by_group = pPa[,,"sick"] /pPa[,,"sick_not"]
print(odds_by_group, digits=2)


odds_sick_A = lapply(unique(pP$A.ate), function(A)
  sum(pP$proportion[pP$A.ate==A & pP$S.sick=='sick'])/
    sum(pP$proportion[pP$A.ate==A & pP$S.sick=='sick_not'])
)
odds_sick_A
prisonersPicnic.dataframe$proportion = c(prisonersPicnic)

sampleSize = 100
prisonersPicnic.dataframe$Expected = 
  sampleSize * prisonersPicnic.dataframe$proportion
prisonersPicnic.dataframe$Observed = 
  rmultinom( n = 1, size = sampleSize, 
             prob = prisonersPicnic.dataframe$proportion)

### Spot-check (compare against original):
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & D.drank=="drank" & S.sick=="sick_not"])

### This next is the same... but what a mess the syntax is!
prisonersPicnic.dataframe$proportion[
  prisonersPicnic.dataframe$A.ate=="ate" & prisonersPicnic.dataframe$D.drank=="drank" & prisonersPicnic.dataframe$S.sick=="sick_not"
  ]

## Now let's re-arrange in alphabetical order.
prisonersPicnic.dataframe = prisonersPicnic.dataframe[c(
  "A.ate", "D.drank", "S.sick", "proportion", "Observed", "Expected"
  )]
### Spot-check again.
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & D.drank=="drank" & S.sick=="sick_not"])

### Now re-order, so that the cycling is most rapid for A, next for D, slowest for S
prisonersPicnic.dataframe = prisonersPicnic.dataframe[
  with(prisonersPicnic.dataframe, order(S.sick,D.drank,A.ate)), ]
print(prisonersPicnic.dataframe)
save(prisonersPicnic.dataframe, file = "prisonersPicnic.dataframe.rdata")


### Let's create an array of observations too,
### and compare the array to the data.frame.
cbind(prisonersPicnic.array, prisonersPicnic.dataframe$proportion)
prisonersPicnic.array.Observed = prisonersPicnic.array
prisonersPicnic.array.Observed[] = prisonersPicnic.dataframe$Observed
### check:
cbind(prisonersPicnic.dataframe$Observed,  
      c(prisonersPicnic.array.Observed))
prisonersPicnic.array.Observed["ate_not", "drank_not", "sick"]  ##sick_not!!
prisonersPicnic.array.Observed["ate", "drank", "sick_not"]  ##sick_not!!
save(prisonersPicnic.array.Observed, file = "prisonersPicnic.array.Observed.rdata")

