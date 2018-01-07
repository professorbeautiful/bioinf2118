SampleSize = 100 
x = rnbinom(SampleSize, 1, .5)
summary(x)
range(x)
hist(x)

xtable = table(x)
xtable

observedValues = as.numeric(names(xtable))
plot(dnbinom(observedValues, 1, .5),
	xtable/SampleSize, pch="")
text(dnbinom(observedValues, 1, .5),
     xtable/SampleSize,
     names(xtable))
abline(a=0, b=1)


plot(observedValues, 
     pnbinom(observedValues, 1, .5),
     ylim=0:1)
     

plot(pnbinom(observedValues, 1, .5),
	cumsum(xtable/SampleSize))
text(pnbinom(observedValues, 1, .5),
     cumsum(xtable/SampleSize),
     observedValues)
abline(a=0,b=1)

par(mfrow=c(1,2))

plot(pnbinom(Q<- 0:6, size=1, prob=0.5), Q,
       pch="Q", cex=2, col="green")

points(temp<-seq(0,1,length.out=101),
       qnbinom(temp, 1, 0.5))

title("pnbinom and qnbinom are inverse functions--\non the sample space (Q, the quantiles)")
     