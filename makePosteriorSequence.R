CondProb_folder  = '"/Users/Roger/Sites/Bioinf-2118-2018-website/N-files-for-handouts/CondProb video"'

options(digits=4)
priorFor4 = c(0.9,0.10, 9, 90)/100

names(priorFor4) = c('dD', 'hD',	'iD',	'noD')
longgroupnames = c(dD='detectable', hD='hidden',	
                   iD='imposter',	noD='no disease')
pr_positive_given_group = c(90,10,50, 10)/100
names(pr_positive_given_group) = 
  c('dD', 'hD',	'iD',	'noD')
pr_dist_given_group = rbind(pr_positive_given_group, 1 - pr_positive_given_group) 
rownames(pr_dist_given_group) = c('pos', 'neg')
normalize= function(x)  x/sum(x)

makePosteriorSequence = function(
    priorFor4 = c(0.9,0.10, 9, 90)/100,
    prob_pos_given_group = pr_dist_given_group[1,],
  dataSequence = c(1,1,1,1), ### 1's and 2's
  wordsForData = c("Positive", "Negative"), # \\quad not working here.
  dropZeroGroups = FALSE,
  makeHTML = TRUE, 
  marginalize = FALSE) {
  
  pr_dist_given_group = rbind(prob_pos_given_group,  
                              1 - prob_pos_given_group)
  
  names(priorFor4) = c('dD', 'hD',	'iD',	'noD')
  names(priorFor4) = c('dD\ndetectable', 'hD\nhidden',	'iD\nimposter',	'noD\nno_disease')
  posteriorFor4 = posteriorSequence = priorFor4
  sapply(seq(along=dataSequence), function(ndata) {
    data = dataSequence[ndata]
    posteriorFor4 <<- matrix(normalize(
      posteriorFor4 * pr_dist_given_group[data,]), nrow=1) 
    rownames(posteriorFor4) =  paste0(#'Report ', 
                                      ndata, ':', wordsForData[data])
    posteriorSequence <<- rbind(posteriorSequence, posteriorFor4)
  })
  if(dropZeroGroups){
    posteriorSequence = posteriorSequence[ 
      , which (priorFor4>0)]
    rownames(posteriorSequence)[1] = " "
  } else {
  #rbind(priorFor4, (posteriorSequence))
    rownames(posteriorSequence)[1] = "4 group probs"
  }
  if(marginalize) {
    Dplus = apply(posteriorSequence[ , 1:2], 1, sum)
    Dminus = apply(posteriorSequence[ , 3:4], 1, sum)
    posteriorSequence[, 1:2] = cbind(Dplus, Dminus)
    posteriorSequence = posteriorSequence[ , 1:2]
  }
  if(ncol(posteriorSequence) == 2){
    colnames(posteriorSequence) = c("+D", "-D")
    rownames(posteriorSequence)[1] = " "
  }
  #if( makeHTML)
  require(xtable)
  library(kableExtra)
  return( knitr::kable(format='html',
               posteriorSequence, padding=10, digits=3,
               table.attr='class="myTable"')
            %>% row_spec(1, bold=TRUE, italic=TRUE)
            %>% pack_rows(
              index=c(`________________________________Prior Probs`=1, 
                      `Reports_________________________Posterior Probs`=length(dataSequence)) )
           # %>% pack_rows(2, )
            )
}