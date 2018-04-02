##### Interesting and realistic example:

intercept = 50  ### Keep away from zero!!
shared = print(10*matrix(rbinom(10, 100, 0.11), ncol=2, nrow=10)) ## same columns
noise = matrix(rbinom(20, 100, 0.11), ncol=2, nrow=10) 
columnEffects = print(matrix(c(0,20), ncol=2, nrow=10, byrow=TRUE))
dataWilcoxonExample = print(intercept + columnEffects + shared + noise)
colnames(dataWilcoxonExample) = c("Xwilc", "Ywilc")
attach(as.data.frame(dataWilcoxonExample))
wilcox.test(Xwilc, Ywilc, paired=TRUE)$p.
wilcox.test(Xwilc, Ywilc, paired=FALSE)$p.
t.test(Xwilc, Ywilc, paired=TRUE)$p.
t.test(Xwilc, Ywilc, paired=FALSE)$p.

#####  What's going on?
plot(dataWilcoxonExample)
abline(a=0, b=1)
##### Notice that most of points are above the line.

boxplot(dataWilcoxonExample, ylim=c(0, max(Ywilc)))
jitteredData = cbind(c(jitter(col(dataWilcoxonExample))), (c(dataWilcoxonExample)))
####  Here col(data) is 1 or 2, for Xwilc and Ywilc
points(jitteredData)
for(i in 1:nrow(dataWilcoxonExample)) 
  lines(jitteredData[c(i, i+nrow(dataWilcoxonExample)), ], 
        col="red", lwd=3)
### Notice that most of the lines are rising.

#####  How biologists present data.

stupidBarPlot(as.data.frame(dataWilcoxonExample), xlab="group", ylab="data")

stupidBarPlot(as.data.frame(dataWilcoxonExample), xlab="group", ylab="data"
              , ylim=c(0,max(Ywilc)))



