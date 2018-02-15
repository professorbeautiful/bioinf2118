j_part = function(k, m, j, n) (-1)^k *
  exp(lchoose(m-j, k) + n*log(m-j-k) - 
        n*log(m))
j_excluded = function(m, j, n)
  choose(m, j) * 
  sum(sapply(0:(m-j), j_part, m=m, j=j, n=n)
  )
### Proportion of samples which do NOT omit exactly nDays-nPeople days.
nPeople = 23; nDays = 365
1 - sapply(seq(10, 60, by=10),
           function(nPeople) 
             j_excluded(nDays, nDays-nPeople, nPeople ) 
)
### Great agreement with birthdayProb()initially, then goes haywire. 

j_excluded(720, 0, 4000 )
### Test the formula:
counts = sapply(1:5000,
                function(ignoreMe)
                  countPermutations(
                    nSimulations=4000, verbose = FALSE) )
# 0.0603
mean(counts==720)   ### 0.054
### Not that close.  But what is the distr?
pbinom(sum(counts==720), size = 5000, prob = j_excluded(720, 0, 4000 ))
## P = 0.03, or 0.06 two-sided.