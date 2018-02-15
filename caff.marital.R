caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218
                         ,327,106,67),
                        nrow=3,byrow=T)
caff.marital
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital
names(dimnames(caff.marital)) <- c("marital","consumption")
as.data.frame(caff.marital)
dimnames(.Last.value)
as.data.frame(as.table(caff.marital))
dimnames(.Last.value)
