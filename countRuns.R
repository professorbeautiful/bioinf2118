###  From the game The Mind, played with Eva.
###  We got a deal which had 8 consecutive.


countMaxRun = function(numCards=24) {
  X<-sort(sample(100, numCards, replace = F))
  diffX = diff(X)
  rleX = rle(diffX==1)
  max(rleX$lengths[rleX$values==TRUE]) + 1
}

table(sapply(1:10000,countMaxRun))
