##   http://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test/
##   Select the numeric data (without header) in the table on the left.
##   (It's better to skip to the interesting example, line 32.)


data.orig = scan()
1	125	110	1	15
2	115	122	-1	7
3	130	125	1	5
4	140	120	1	20
5	140	140	 NA	0
6	115	124	-1  9
7	140	123	1	17
8	125	137	 -1	12
9	140	135	1	5
10	135	145	 -1	10


data = matrix(data.orig, ncol=5, byrow = T)
data = data [, 2:3]

colnames(data) = c("X", "Y")

## MAKE THIS CHANGE 2nd TIME THROUGH, to test robustness.
# data[5,1] = 139;   data[3,1] = 131 
differences = print( - apply(data, 1, diff))
unsignedranks = print(rank(abs(differences)))

rbind(differences, unsignedranks)
signedranks  = print(unsignedranks * sign(differences))
sum(signedranks)
signedranks  = print((unsignedranks-1) * sign(differences))  ### why??
sum(signedranks)
sum((unsignedranks-1) * (differences > 0))  
    # this is the statistic reported by  wilcox.test
wilcox.test(differences)
wilcox.test(data[,1], data[,2], exact=F)
wilcox.test(data[,1], data[,2], exact=T)

t.test(data[ , 1], data[ , 2], paired=TRUE)
t.test(data[ , 1], data[ , 2])   ###  two-sample, not paired

plot(data)
abline(a=0, b=1)
boxplot(data)
jitteredData = cbind(c(jitter(col(data))) , jitter(c(data)))
points(jitteredData)
for(i in 1:nrow(data)) lines(jitteredData[c(i, i+nrow(data)), ], col="red", lwd=3)

##### More interesting and realistic example:

intercept = 50  ### Keep away from zero!!
shared = print(10*matrix(rnorm(10), ncol=2, nrow=10)) ## same columns
noise = matrix(rnorm(20), ncol=2, nrow=10) 
columnEffects = print(matrix(c(0,2), ncol=2, nrow=10, byrow=TRUE))
data = print(intercept + columnEffects + shared + noise)
wilcox.test(data[ , 1], data[ , 2], paired=TRUE)$p.
wilcox.test(data[ , 1], data[ , 2], paired=FALSE)$p.
t.test(data[ , 1], data[ , 2], paired=TRUE)$p.
t.test(data[ , 1], data[ , 2], paired=FALSE)$p.

#####  What's going on?
plot(data)
abline(a=0, b=1)
boxplot(data)
jitteredData = cbind(c(jitter(col(data))), (c(data)))
points(jitteredData)
for(i in 1:nrow(data)) 
  lines(jitteredData[c(i, i+nrow(data)), ], 
        col="red", lwd=3)

#####  How biologists present data.

stupidBarPlot(as.data.frame(data))

stupidBarPlot(as.data.frame(data), ylim=c(0,65))



