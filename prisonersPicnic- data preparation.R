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
prisonersPicnic = matrix(prisonersPicnic, nrow=2, byrow=TRUE)

## Put into a data frame. 
### Looking across the rows,
## "sick" changes fastest, then "ate", then "drank".
prisonersPicnic.dataframe = 
	expand.grid( 
		S.sick=c("sick","ok"), A.ate=c("ate","ate not"), 
    D.drank=c("drank","drank not") )

prisonersPicnic.dataframe$proportion = c(prisonersPicnic)

sampleSize = 100
prisonersPicnic.dataframe$Expected = 
  sampleSize * prisonersPicnic.dataframe$proportion
prisonersPicnic.dataframe$Observed = 
  rmultinom( n = 1, size = sampleSize, 
             prob = prisonersPicnic.dataframe$proportion)

### Spot-check (compare against original):
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & D.drank=="drank" & S.sick=="ok"])

### This next is the same... but what a mess the syntax is!
prisonersPicnic.dataframe$proportion[
  prisonersPicnic.dataframe$A.ate=="ate" & prisonersPicnic.dataframe$D.drank=="drank" & prisonersPicnic.dataframe$S.sick=="ok"
  ]

## Now let's re-arrange in alphabetical order.
prisonersPicnic.dataframe = prisonersPicnic.dataframe[c(
  "A.ate", "D.drank", "S.sick", "proportion", "Observed", "Expected"
  )]
### Spot-check again.
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & D.drank=="drank" & S.sick=="ok"])

### Now re-order, so that the cycling is most rapid for E, next for D, slowest for S
prisonersPicnic.dataframe = prisonersPicnic.dataframe[
  with(prisonersPicnic.dataframe, order(S.sick,D.drank,A.ate)), ]
print(prisonersPicnic.dataframe)
save(prisonersPicnic.dataframe, file = "prisonersPicnic.dataframe.rdata")

####  Reshaping the data in the form of an array:
prisonersPicnic.array = 
  array(prisonersPicnic, 
        dim=c(2,2,2), 
        dimnames=list(
          S.sick=c("sick","ok"), A.ate=c("ate","ate not"), 
          D.drank=c("drank","drank not")) )
### Spot-check:
prisonersPicnic.array ["ok", "ate", "drank"]

### And reshape the array...
prisonersPicnic.array = aperm(prisonersPicnic.array, c("A.ate", "D.drank", "S.sick"))
### Spot-check again:
prisonersPicnic.array ["ate", "drank", "ok"]
save(prisonersPicnic.array, file = "prisonersPicnic.array.rdata")
prisonersPicnic.array

### Let's create an array of observations too,
### and compare the array to the data.frame.
cbind(prisonersPicnic.array, prisonersPicnic.dataframe$proportion)
prisonersPicnic.array.Observed = prisonersPicnic.array
prisonersPicnic.array.Observed[] = prisonersPicnic.dataframe$Observed
### check:
cbind(prisonersPicnic.dataframe$Observed,  
      c(prisonersPicnic.array.Observed))
prisonersPicnic.array.Observed["ate not", "drank not", "sick"]  ##ok!!
prisonersPicnic.array.Observed["ate", "drank", "ok"]  ##ok!!
save(prisonersPicnic.array.Observed, file = "prisonersPicnic.array.Observed.rdata")

