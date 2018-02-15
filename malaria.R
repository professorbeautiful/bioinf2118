##  exercises-chapter 6,  6.1, 6.3, 6.4

library(ISwR)
head(malaria) 
plot(data=malaria, ab ~ age, log="y", pch=mal+1, col=mal+2)
lines.loess(malaria$age, malaria$ab )

plot(data=malaria, log10(ab) ~ age,  pch=mal+1, col=mal+2)
lines.loess(malaria$age, log10(malaria$ab) )
?loess
lines.loess(malaria$age, log10(malaria$ab), span=0.25 )

lm.out.1 = lm(data=malaria, log(ab) ~ age)
anova(.Last.value)
predict(lm.out.1, newdata = 15, )
lm.out.2 = lm(data=malaria, log(ab) ~ poly(age,2))
anova(.Last.value)
deviance(lm.out)
deviance(lm.out.2)
lm(data=malaria, log(ab) ~ age + age^2)
lm(data=malaria, log(ab) ~ age + I(age^2))
anova(.Last.value)

