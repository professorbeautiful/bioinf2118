###   Hardy-Weinberg example.

tdata = c( AA=90,  AB=40,  BB=10)

hwThetaHat = (tdata[1]+tdata[2]/2)/sum(tdata)

Ehat = c(hwThetaHat^2, 2*hwThetaHat*(1-hwThetaHat), (1-hwThetaHat)^2)  * sum(tdata)

rbind(tdata, Ehat)
Q = sum((tdata-Ehat)^2/Ehat)
1 - pchisq(Q, df=1)

# 
if( ! require("HardyWeinberg") ) ## dumb semantics
  install.packages("HardyWeinberg")
HWChisq(tdata)
c(ours=1 - pchisq(Q, df=1), 
  pack=HWChisq(tdata, verbose = FALSE)$pval)
### The difference is that HWChisq uses a "continuity correction" of 0.5.
c(ours=1 - pchisq(Q, df=1), 
  pack=HWChisq(tdata, verbose = FALSE)$pval,
  HWExact(tdata, verbose=FALSE)$pval)

help(HWData)
m <- 100 # number of markers
n <- 100 # sample size
HWData.out <- HWData(n,m)
HWTernaryPlot(HWData.out,100,region=1,vertex.cex=2,signifcolour=TRUE)

HWTernaryPlot(t(as.matrix(tdata)), 	
	region=1,vertex.cex=2,signifcolour=FALSE)
 
HWTernaryPlot(HWData.out,100,region=1,vertex.cex=2,signifcolour=TRUE)
HWTernaryPlot(t(as.matrix(tdata)), 
              hwcurve=FALSE, vbounds=FALSE, mafbounds = FALSE,
              region=0, cex=4,signifcolour=FALSE, new=F)


########   Hardy-Weinberg example, using real data.  ###############

###  For individual results:
lungGenDataNames = readxl::read_excel('Lung SPORE project 4 data.12.09.FINAL.xlsx', 
               col_names = FALSE) [1, ]
lungGenDataNames = unlist(lungGenDataNames)
lungGenData = readxl::read_excel('Lung SPORE project 4 data.12.09.FINAL.xlsx', 
                                 skip = 5, col_names = lungGenDataNames)
dim(lungGenData)

str(lungGenData)
lungGenData=as.matrix(lungGenData)

###  For count data :   See Project4Tables-jlw-2010-03-29-Table4W4.csv
lungGenTables = read.csv('Project4Tables-jlw-2010-03-29-Table4W4.csv', 
                      header=T)
str(lungGenTables)
lungGenTables.g0 =  data.frame(group=0, lungGenTables$SNP, lungGenTables[8:10])
names(lungGenTables.g0) = c('group', 'SNP', 't1', 't2', 't3')
lungGenTables.g1 =  data.frame(group=1, lungGenTables$SNP, lungGenTables[12:14])
names(lungGenTables.g1) = c('group', 'SNP', 't1', 't2', 't3')
lungGenTables = rbind(lungGenTables.g0, lungGenTables.g1) 

analyzeSNP = function(rownum, verbose=F){
  countsThisSNP = lungGenTables[rownum, 3:5]
  t1 = countsThisSNP$t1
  t2 = countsThisSNP$t2
  t3 = countsThisSNP$t3
  hwThetaHat = (t1 + t2/2)/sum(countsThisSNP)
  Ehat = c(hwThetaHat^2, 
           2*hwThetaHat*(1-hwThetaHat), 
           (1-hwThetaHat)^2)  * 
    sum(countsThisSNP)
  
  if(verbose) print(rbind(countsThisSNP, Ehat))
  
  Q = sum( (countsThisSNP-Ehat)^2/Ehat )
  Qadjusted = sum(( abs(countsThisSNP-Ehat) - 1/2)^2/Ehat)
  c(P = 1 - pchisq(Q, df=1),  Padjusted =  1 - pchisq(Qadjusted, df=1) )
}

allP = sapply(1:nrow(lungGenTables), analyzeSNP)

#install.packages("HardyWeinberg")
require("HardyWeinberg")
hwCounts = lungGenTables[3:5]
names(hwCounts) =  c("AA", "AB", "BB")  ### instead of t1, t2, t3; necessary.
hwCount1 = unlist(hwCounts[1, , drop=TRUE] )
HWChisq(hwCount1) 
HWExact(hwCount1)
HWAlrPlot(as.matrix(hwCounts))
HWClrPlot(as.matrix(hwCounts))
HWIlrPlot(as.matrix(hwCounts))
HWTernaryPlot(as.matrix(hwCounts))
HWGenotypePlot(as.matrix(hwCounts))

