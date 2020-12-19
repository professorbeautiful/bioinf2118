#Permutations of the set of 7 things
sample(1:7)

## simulations of choosing subsets of size 3.
subsetTable = table(apply(
  sapply(1:1000, function(dummy) sample(size = 3, x = 1:7)),
  2,
  function(x)paste(as.character(sort(x)), collapse=",") )
)
hist(subsetTable)
## these two numbers should be close or equal.
length(subsetTable)
choose(7, 3)

#Pascal's triangle
triangleSize = 7
plot(0:triangleSize, 0:triangleSize, pch='',
     xlab='subset size', ylab='parent size')
for(n in 1:triangleSize)
  for(k in 0:n)
    text(k, n, choose(n,k))
symbols(3, 7, rectangles = matrix(c(0.55, 0.55),  nrow=1), 
        inches=F, fg='red', add = T, xpd=NA )
title('binomial coefficients\nchoose(7,3) = 7!/3!4! = 35')


