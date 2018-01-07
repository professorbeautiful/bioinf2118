# dalgaard 3.1
# 3.1 Calculate the probability for each of the following events: 
#(a) A standard normally distributed variable is larger than 3. 
#(b) A normally distributed variable with mean 35 and standard deviation 6 is larger than 42. 
#(c) Getting 10 out of 10 successes in a binomial distribution with probability
# 0.8. 
#(d) X < 0.9 when X has the standard uniform distribution. 
#(e) X > 6.5 in a c2 distribution with 2 degrees of freedom.

1-pnorm(3)          #a
1-pnorm(42, 35, 6)  #b
dbinom(10,10,0.8)   #c
punif(0.9)          #d
1-pchisq(6.5,df=2)  #e


