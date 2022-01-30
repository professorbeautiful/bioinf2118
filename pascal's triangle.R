#Permutations of the set of 7 things
sample(1:7)

## simulations of choosing subsets 
subsetSize =  2
wholeSetSize = 5
randomSubsets = apply(
  sapply(1:1000, function(dummy) 
    sample(size = subsetSize, x = 1:wholeSetSize)),
  2,
  function(x)paste(as.character(sort(x)), collapse=",") )
subsetTable = table(randomSubsets)
names(subsetTable)
head(subsetTable)
hist(subsetTable)
## these two numbers should be close or equal.
## The number of subsets of size 3 is...
  choose(wholeSetSize, subsetSize)
## the number of subsets we got in our simulation is...
  length(subsetTable)



#Pascal's triangle
triangleSize = 7
plot(0:triangleSize, 0:triangleSize, pch='',
     xlab='subset size', ylab='parent size')
for(n in 1:triangleSize)
  for(k in 0:n)
    text(k, n, choose(n,k))
symbols(2, 5, rectangles = matrix(c(0.55, 0.55),  nrow=1), 
        inches=F, fg='red', add = T, xpd=NA )
title('binomial coefficients\nchoose(7,3) = 7!/3!4! = 35')



table_of_0_1 = table(
  sapply( 1:1000, function(x) sum(sample(0:1, 5, replace = T)) )
)
plot(0:5, table_of_0_1/1000, ylim=c(0, .9))
ournumbers = c(58,261,538,522,264,44)
points(0:5, ournumbers/sum(ournumbers), col='red', pch="N")

sum(c( 1,5, 10, 10, 5, 1) * 10^(0:5) )  
11^5

