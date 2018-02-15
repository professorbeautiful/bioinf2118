### mcnemar exercise 2011-04-14

pastedData = "TRUTH:   disease present
Test B is positive
Test B is negative
Test A is positive
100
17
Test A is negative
8
15
TRUTH:   disease absent
Test B is positive
Test B is negative
Test A is positive
14
36
Test A is negative
22
200"
ourMcNemarAdventure = function(pastedData) {
  mcdata.orig = strsplit(split = "\n", pastedData) [[1]]
  mcdata.character.matrix = matrix(mcdata.orig, ncol=3, byrow=T)
  library(abind)
  mcdata.character.array = abind(mcdata.character.matrix[1:3, ], 
                                 mcdata.character.matrix[4:6, ],
                                 along=3)
  mcdata.character.array
  mcdata.array = mcdata.character.array[ -1, -1,  ]
  mcdata.array =  apply(mcdata.array, MARGIN = 1:3, as.numeric) 
            # as.numeric(mcdata.array) does not work here! 8-(
  dimnames(mcdata.array) = list(
    A=c("Apos","Aneg"),  # ROW
    B=c("Bpos","Bneg"),  # COLUMN
    TRUTH=c("present","absent") # SLICE
  )	
  
  ######
  
  mcdata.df = expand.grid(dimnames(mcdata.array))
  mcdata.df$count = c(mcdata.array)
  (mcdata.df)
  ######  Check it!  OK.
  
  #### Now we can use mcdata.array and mcdata.df to test hypotheses.
  
  ######  Alt hypothesis (1-sided):  A is more likely to say "positive" than B.
  ###### One approach: Just collapse over TRUTH:
  ##  (Here are 3 ways of collapsing)
  (AposBneg = mcdata.array["Apos", "Bneg", "present"] +
              mcdata.array["Apos", "Bneg", "absent"])
  (AposBneg = sum(mcdata.array["Apos", "Bneg", ]))
  (AposBneg = sum( mcdata.df [mcdata.df$A=="Apos" & 
                              mcdata.df$B=="Bneg", "count"]))
  ### Here is the other marginalized diagonal:
  AnegBpos = sum(mcdata.array["Aneg", "Bpos", ])
    cat("McNemar PtestMorePos = ", 
           PtestMorePos <- round(digits=3, 
                                   pbinom(min(AnegBpos,AposBneg), AposBneg+AnegBpos, 1/2)),
        "\n")
  
  ## Compare with mcnemar.test-- TWOSIDED
  ## Collapsing over the 3rd dimension (TRUTH).
  mcnemar.test(apply(mcdata.array, 1:2, sum) )  ## P=0.801 chisq.

    ## This is very limited, though.
  
  ##### Another approach:  control for TRUTH.
  catn("(present) P = " %&% round(digits=3,
                                  Ppresent <- 1 - pbinom(17-1, 17+8, 1/2)))
  catn("(absent) P = " %&% round(digits=3,
                                 Pabsent <- 1 - pbinom(16-1, 16+22, 1/2)))
  ### Combine Pvalues using Fisher's method:
  catn("(combined) P = " %&% round(digits=3,
                                   Pcombined <- 1-pchisq(df=2*2, 
                                                         -2*log(Ppresent*Pabsent))))
  
  
  ######  Hypothesis:  A is more likely to be CORRECT.
  mcdata.combined = mcdata.array[ , , 1] + mcdata.array[ 2:1, 2:1, 2]
  dimnames(mcdata.combined) = list(c("Aright", "Awrong"), c("Bright", "Bwrong"))
  mcdata.combined
  mcnemar.test(mcdata.combined)  ## P=0.078 chisq.
  
  mcdata.df$Acorrect = 
    (mcdata.df$A=="Apos" & mcdata.df$TRUTH=="present") |
    (mcdata.df$A=="Aneg" & mcdata.df$TRUTH=="absent") 
  mcdata.df$Bcorrect = 
    (mcdata.df$B=="Bpos" & mcdata.df$TRUTH=="present") |
    (mcdata.df$B=="Bneg" & mcdata.df$TRUTH=="absent") 
  
  ###  Collapsing over TRUTH:
  AcorrectBincorrect = mcdata.df$Acorrect & 
    ! mcdata.df$Bcorrect
  nAcorrectBincorrect = sum(mcdata.df$count[AcorrectBincorrect])
  ABdisagree = mcdata.df$Acorrect != mcdata.df$Bcorrect
  nABdisagree = sum(mcdata.df$count[ABdisagree])
  cat("(collapsing) P = ",
      1 - pbinom(nAcorrectBincorrect - 1, nABdisagree, 1/2), "\n")
  # P = 0.038 - one-sided!
  
  ## Alternatively,
  mcdata.correctness = mcdata.array[ , , 1] + t(mcdata.array[ , , 2])
  dimnames(mcdata.correctness) =  list(A=c("correct", "incorrect"),
                                       B=c("correct", "incorrect"))
  mcdata.correctness
  mcnemar.test(mcdata.correctness)  # P = 0.078 - two-sided! (& chisq)
  
  #### Controlling for TRUTH:
  #### Caution: If most cases are "present", then A might be
  ###  more right just because A says "positive" more often.
  ###             pos vs neg  ------  A vs B
  ###                    \\            //
  ###                   present vs absent
  
  ### To define and predict "correctness", 
  ### adjusting for propensity to say "positive",
  ### we need to make a data set double the size.
  mcdata2 = rbind(mcdata.df, mcdata.df)
  mcdata2$test = rep(c('A','B'), each=8)
  (mcdata2$correct =  ifelse(mcdata2$test=="A", 
                             mcdata2$Acorrect, mcdata2$Bcorrect))
  mcdata2 = mcdata2[c('TRUTH','count','test','correct')]
  
  require("MASS")
  glm(data=mcdata2,  correct ~ TRUTH + test, weights=count, family=poisson)	
  summary(.Last.value)
  ## With the interaction term, not any better!
  glm(data=mcdata2,  correct ~ TRUTH*test, weights=count, family=poisson)	
  summary(.Last.value)
} 
  ### Now let's repeat "correcting" one data point:
  