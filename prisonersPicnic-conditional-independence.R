
####### Conditional independence

prisonersPicnic.dataframe.by.S = split(prisonersPicnic.dataframe, prisonersPicnic.dataframe$S) 
prisonersPicnic.dataframe.by.S[["sick"]]
prisonersPicnic.dataframe.by.S[["ok"]]
prisonersPicnic.array[ , , "sick"]
prisonersPicnic.array[ , , "ok"]

####  Exercise:
####  Assess whether A and B are independent conditional on C.
####  Use either data frame or array data structure.
####  Assume a sample size of 100 in the ENTIRE data set.
####  Calculate the two values for odds ratio relating A to B.
