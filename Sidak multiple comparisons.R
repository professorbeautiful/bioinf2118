B=1e5
numTests = 10
minPvalues = sapply(1:B, 
                    function(ignoreMe) min(runif(numTests)) )
head(minPvalues)
plot(density(minPvalues))
#abline(a=2, b= -2, col='red')
Pobs = 0.04

sub_P<-subset(minPvalues,minPvalues<Pobs)
length(sub_P) / B

1 - (1-Pobs)^numTests
