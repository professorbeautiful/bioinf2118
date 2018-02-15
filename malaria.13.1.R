#   13.1 In the malaria data set, analyze the risk of malaria with age and log-transformed antibody level as explanatory variables.

#  See also malaria.R

library(ISwR)
head(malaria) 
table(malaria$mal)  ### 27 cases, 73 noncases
with(malaria, 
     smartBarPlot(split(log10(ab), mal), pch=mal+1, col=mal+2))
with(malaria, 
     smartBarPlot(split(age, mal), pch=mal+1, col=mal+2))

glm.mal.1 = glm(data=malaria, mal ~ age + log10(ab), family=binomial)
anova(glm.mal.1)
summary(glm.mal.1)
glm(data=malaria, mal ~ age + log10(ab), family=binomial)
glm.mal.poly = glm(data=malaria, mal ~ poly(age,2) + log10(ab), family=binomial)
anova(glm.mal.poly)
summary(glm.mal.poly)
glm.mal.poly.3 = glm(data=malaria, mal ~ poly(age,3) + log10(ab), family=binomial)
anova(glm.mal.poly.3)
summary(glm.mal.poly.3)

glm.mal.categories = glm(data=malaria, mal ~ factor(age) + log10(ab), family=binomial)
glm.mal.categories
anova(glm.mal.categories)
summary(glm.mal.categories)

### Only age:
glm.mal.age = glm(data=malaria, mal ~ age, family=binomial)
glm.mal.age
anova(glm.mal.age)
summary(glm.mal.age)

glm.mal.age.poly = glm(data=malaria, mal ~ poly(age,3), family=binomial)
glm.mal.age.poly
anova(glm.mal.age.poly)
summary(glm.mal.age.poly)
