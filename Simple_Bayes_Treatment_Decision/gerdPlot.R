gerdPlot = function(nRow=25, nCol=40, sens = 0.90, spec=0.90, prev=0.01) {

  N = nRow * nCol
#  print(N * c(TP,FP,TN,FN))
  grid = expand.grid(1:nRow, 1:nCol)

  combo = c('TP', 'FN', 'TN', 'FP' )
  pcombo = c(prev*c(sens, 1-sens), (1-prev)*c(spec,1-spec))
  comboOrder = rev(order(pcombo))
  names(pcombo) = combo
  combo = combo[comboOrder]
  pcombo = pcombo[comboOrder]
  ncombo = round(N*pcombo)
  print(ncombo)
  print(sum(ncombo))
  comboCum = cumsum(ncombo)
  print(comboCum)
  comboCum = c(0, comboCum[-length(ncombo)])
  print(comboCum)
  names(comboCum) = combo
  colors = c(TP='red', TN='blue',FP='green', FN='black')
  plot(grid, axes=F, pch="",
       xlab='', ylab='')
  for(group in combo)
    text(grid[comboCum[group] + (1:ncombo[group]), ], 
         label=group, cex=0.7, col=colors[group])
  
}
#gerdPlot()
