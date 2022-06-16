gerdPlot2 = function(nRow=25, nCol=40, sens = 0.90, spec=0.90, prev=0.01) {
  
  N = nRow * nCol
  
  combo = c('TP', 'FN', 'TN', 'FP' )
  pcombo = c(prev*c(sens, 1-sens), (1-prev)*c(spec,1-spec))
  names(pcombo) = combo
  ncombo = round(N*pcombo)
  
  colors = c(TP='red', TN='blue',FP='dark green', FN='purple')
  plot(0:1, 0:1, axes=F, pch="", xlim=0:1, ylim=0:1,
       xlab='', ylab='')
  # for(group in combo)
  # for(group in combo)
  #   text(grid[comboCum[group] + (1:ncombo[group]), ], 
  #        label=group, cex=0.7, col=colors[group])
  #abline(h=c(0, sum(pcombo[c('TP', 'FN' )]))) 
  
  rectX = c(prev/2, prev/2, 1-(1-prev)/2, 1-(1-prev)/2)
  rectY = c(sens/2, 1 - (1-sens)/2 , 1-(spec)/2, (1-spec)/2)
  print(rectX)
  print(rectY)
  rectCenters = cbind( rectX, rectY)
  rectangles =  rbind(
    c(prev, sens),
    c(prev, 1-sens),
    c(1-prev, spec),  #TN
    c(1-prev, 1-spec)   #FP
  )
  symbols(rectCenters, rectangles =rectangles, add=TRUE,
          inches=F, bg=colors)
  labelXY = rbind(
    c(-0.05, -0.05),
    c(-0.05, 1.05),
    c(1.05, 1.05), #TN
    c(1.05, -0.05)
  )
  print(labelXY)
  for(group in 1:4)
    lines(x=c(labelXY[group, 1], round(labelXY[group, 1])), 
          y=c(labelXY[group, 2], round(labelXY[group, 2])),
          col=colors[group], xpd=NA)
  text((labelXY), labels=combo, col=colors, xpd=NA)
  print(pcombo)
  pPos = pcombo['TP'] + pcombo['FP']
  lines(c(0,0,1,1,0),c(0,1,1,0,0),lwd=4)
  
  lines(c(0, 0, prev, prev, 1, 1), 
        c(1, sens, sens, 1-spec, 1-spec, 0),
        lwd=7, lty='dashed')
  mtext("Positive test", 1, cex=2)
  mtext("Negative test", 3, cex=2)
  mtext("Sick", 2, cex=2)
  mtext("Healthy", 4, cex=2)
}
#gerdPlot2(spec=0.8, prev=0.1)
#abline(h=(1:99)/100, v=(1:99)/100, col='white', lty='dotted')
#points(expand.grid((1:99)/100, (1:99)/100), col='white', pch='.')
