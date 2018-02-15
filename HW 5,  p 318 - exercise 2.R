### p 318 - exercise 2.

### [B > 90 | A = 80] =  
#N(
#  muB + rho*sigB*(A-muA)/sigA, 
#  (1-rho^2)*sigB^2)   
####  See page 316 at the top. 

###  the set-up. 
muA = 85
muB = 90
rho = 0.8
sigA = 10
sigB = 16
A = 80


muB.A = muB + rho*sigB*(A-muA)/sigA
sigsqB.A = (1-rho^2)*sigB^2

condProbBgt90 = 1 - pnorm(90, muB.A, sqrt(sigsqB.A))

####   0.2524925

####  Check it with a simulation:
varAB = rbind( c(sigA^2,  rho*sigA*sigB),
			c(rho*sigA*sigB,   sigB^2 ) ) 
(diag(1-rho, 2) + rho )  * 
	outer(c(sigA,sigB), c(sigA, sigB))
require(mvtnorm)
ABsample = rmvnorm(100000, c(muA,muB), varAB)
Asim = ABsample[,1]
Bsim = ABsample[,2]
require("mvbutils")
normalize(table(Bsim[Asim %in.range% c(78, 82)] > 90))
### 0.258