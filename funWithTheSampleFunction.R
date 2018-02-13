### 

sample(1:6)
sample(x = 1:6, size = 2)
sample(x = 1:6, size = 6)
sample(x = 1:6, size = 12)  ## error!
sample(x = 1:6, size = 12, replace = T)
sample(x = 1:6, size = 12, replace = F)  ## error!
factorial(6)   ## There are 6! permutations.

# This function will count the number of permutations
### Generate random permutations
countPermutations = function(
  nSimulations = 40, size = 6, verbose = FALSE) {
  permutations = sapply(1:nSimulations, 
                  function(dummyArgument) 
                    sample(x = 1:size, size = size, replace = F))
  if(verbose) print(str(permutations))
  permutStrings = apply(X = permutations, MARGIN = 2, FUN = paste, collapse="")
  if(verbose) print(str(permutStrings))
  if(verbose) print(table(table(permutStrings)))
  return(length(table(permutStrings)))
}
### The apply() function takes and array or matrix and applies a function to some "face".
### Here the MARGIN argument says "keep the 2nd dimension" which is columns.


### Now let's test our function:
countPermutations(verbose=TRUE)
## The number of distinct permutations is...

### Now, for large numbers of simulations,
### the number of permutations generated is:
countPermutations(nSimulations <- 4000)
#### Since the number of permutations will be 720 or a little less.
factorial(6)  ###  = 720



## The birthday calculation:
## The chance that you get a specific permutation at least once is an example of the "birthday problem".
## Given a party of people (say, 23 people), what is the chance that at least 2 have the same birthday?
## See http://www.math.uah.edu/stat/urn/Birthday.html#gen  (Courtesy of Dan Steinberg.)

birthdayProb = function(nPeople = 23, nDays=365)
  1 - exp(lfactorial(nDays) - 
          lfactorial(nDays-nPeople) 
        - nPeople*log(nDays))
birthdayProb(seq(10, 60, by=10)) ### Checks.
birthdayProb(22:23)
#Therefore 23 people is the minimum sized party for which the chance of duplicate birthday exceeds 1/2.

plot(seq(2, 60), birthdayProb(seq(2, 60)), xlim=c(0,60), ylim=c(0,1))
