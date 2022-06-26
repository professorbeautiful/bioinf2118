gerdPlot = function(nRow=25, nCol=40, sens = 0.90, spec=0.90, prev=0.01) {

  N = nRow * nCol
#  print(N * c(TP,FP,TN,FN))
  grid = expand.grid(1:nRow, 1:nCol)
  colors = c(TP='blue', TN='red',FP='dark green', FN='purple')
  comboOrder = c('TP', 'FN','FP', 'TN' )
  pcombo = c(
    TP=prev*sens, FN=prev*(1-sens), 
    TN=(1-prev)*spec, FP=(1-prev)*(1-spec))
  pcombo = pcombo[comboOrder]
  ncombo = round(N*pcombo)
  # print(ncombo)
  # print(sum(ncombo))
  comboCum = cumsum(ncombo)
  # print(comboCum)
  comboCum = c(0, comboCum[-length(ncombo)])
  # print(comboCum)
  names(comboCum) = comboOrder
  #colors = c(TP='red', TN='blue',FP='green', FN='black')
  plot(grid, axes=F, pch="",
       xlab='', ylab='')
  for(group in comboOrder)
    text(grid[comboCum[group] + (1:ncombo[group]), ], 
         label=group, cex=0.7, col=colors[group])
  comboLong = c(TN='  true negative',
                FP='  false positive',
                TP='   true positive',
                FN='false negative') [comboOrder]
  mtext(paste("#people: ", N), adj=0)
  for(group in comboOrder)
    mtext(paste(ncombo[group], group, ': ', comboLong[group]), 
          col=colors[group],
          outer=FALSE, xpd=NA, line=which(group==comboOrder)-1, adj=1)
  
}
#gerdPlot()
