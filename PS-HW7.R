#------------------------
#Necessary package for using loglm( )
#------------------------
library(MASS)

#------------------------
#Prissioner's Picnic data
#------------------------

sick = c("sick", "sick", "nosick", "nosick", "sick", "sick", "nosick", "nosick")
drank = c("drank", "drank", "drank", "drank", "nodrank", "nodrank", "nodrank", "nodrank")
ate = c("ate", "noate", "ate", "noate", "ate", "noate", "ate", "noate")
#The proportions where multiplyed by 1000
numbers = c(481, 60, 155, 168, 38, 22, 10, 66)
dataFrame = data.frame(cbind(sick,drank,ate,numbers))
dataFrame 
temp = xtabs(~., data=dataFrame)

#------------------------
#Fitting the hypothesis
# assign results of each loglm to a variable
#------------------------

# H1: ate, drank, sick are independent
h1 = loglm(formula=~1 + 2 + 3, data = temp)

# H2: sick is related to ate but not drank
h2 = loglm(formula=~1 + 2 + 3 + 1:3, data = temp)

# H3: sick is related to drank but not ate
h3 = loglm(formula=~1 + 2 + 3 + 1:2, data = temp)

# H4: ate and drank are independent conditional on sick
h4 = loglm(formula=~1 + 2 + 3 + 1:2 + 1:3, data = temp)

# H5: ate, drank, sick may be pairwise associated but there is no three way interaction
h5 = loglm(formula=~1 + 2 + 3 + 1:2 + 1:3 + 2:3, data = temp)

# H6 the saturated model
h6 = loglm(formula=~1:2:3, data= temp)

#------------------------
#Use the function anova ( ) on these models
#------------------------

anova(h1, h2, h3, h4, h5, h6)

#------------------------
#Also explore using funcions residuals ( ), coefficients ( ), and summary ( )
#------------------------

residuals(h1) #or any other hypothesis
residuals(h6)

coefficients(h1, h2, h3, h4, h5, h6) #including all the hypothesis
coefficients(h1, h3, h5) #or any given combination

summary(h1)#or any other hypothesis
summary(h6)
