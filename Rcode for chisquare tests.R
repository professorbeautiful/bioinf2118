#########chisquareForEqualP.simulation

####  This is a one-way table.
chisquareForEqualP.simulation = function(
	nCells=5,  							# the number of cells in the table.
	samplesize=100,						# the count total for the table.
	modelProbs=rep(1/nCells, nCells),	# the null hypothesis probabilities.
	RANDOMIZATION_TEST=FALSE,
	PRINTME=TRUE,
	TESTME=TRUE,
	...)
{
	simulatedData = rmultinom(1, samplesize, modelProbs)
	simulatedData = as.vector(simulatedData)
	expected = samplesize * modelProbs
	if(PRINTME) print(rbind(E=expected, O=simulatedData))
	chisquare.result = chisq.test(simulatedData, p=modelProbs,
				simulate.p.value= RANDOMIZATION_TEST, ...)
	return(chisquare.result)
}

simResult = chisquareForEqualP.simulation()
simResult
class(simResult)
unclass(simResult)
# This shows the actual elements of the return value.
getS3method("print","htest")

#################
####   From man page for chisq.test:
### Case A. Tabulated data
x <- c(89,37,30,28,2)  # original
x <- c(20,10,8,8,2)
p <- c(0.40,0.20,0.20,0.19,0.01)
                    # Expected count in category 5
                    # is 1.86 < 5 ==> chi square approx.
chisq.test(x, p = p)            #   maybe doubtful, but is ok!
chisq.test(x, p = p,simulate.p.value = TRUE)$p.val

## Case B. Raw data
x <- trunc(5 * runif(100))	# Tabulate it first!
chisq.test(table(x))        # NOT 'chisq.test(x)'!
#
#

