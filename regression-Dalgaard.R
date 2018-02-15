# regression-Dalgaard

while( ! require(ISwR) )
  install.packages("ISwR")

data("thuesen")
help("thuesen")

dim(thuesen)
thuesen
##Notice one missing value.

attach(thuesen)
search()
ls(pos=2)
plot(short.velocity, blood.glucose)

summary(lm(blood.glucose ~ short.velocity, 1))

abline(lm(blood.glucose ~ short.velocity))
### Note the special method for abline when the arg is a "lm" object.


lm.out =lm(data=thuesen[-is.na(thuesen$short.velocity), ],
          blood.glucose ~ short.velocity)

points(short.velocity, predict(lm.out), col="red", pch=2)


