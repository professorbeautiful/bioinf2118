axis.log.odds = 0
axis.odds = 3
axis.prob = 6
logoddscolor = 'red'
oddscolor = 'blue'
probcolor = 'green'
axisNameFont = c('serif', 'bold')
yrange = c(-5,5)
rescaled.odds = function(od,
                         expander = 0.7*(yrange[2]-yrange[1]), 
                         offset = yrange[1]+ 0.1*expander)
  od*expander + offset
rescaled.prob = function(p,
                         expander = yrange[2]-yrange[1], 
                         offset = yrange[1])
  p*expander + offset


logoddslist = seq(yrange[1],yrange[2],length=100)
oddslist = exp(logoddslist)
problist = odds.to.prob(oddslist)

plot3scales = function() {
  plot(c(-1,axis.prob), yrange, axes=F, pch=' ',
       xlab='',ylab='')
  sapply(logoddslist, function(logodds)
  {
    odds = exp(logodds)
    lines(c(axis.log.odds, axis.odds), 
          c(logodds, rescaled.odds(odds)), col='lightgrey')
    lines(c(axis.odds, axis.prob), 
          c(rescaled.odds(odds), rescaled.prob(odds.to.prob(odds))), col='lightgrey')
  })
  abline(v=axis.log.odds, col=logoddscolor, lwd=3)
  lines(c(axis.odds, axis.odds), rescaled.odds(c(0, max(oddslist))), lwd=3, col=oddscolor)
  lines(c(axis.prob, axis.prob), rescaled.prob(c(0,1)), lwd=3, col=probcolor)
  
  text(axis.log.odds, yrange[2], 'log odds', col=logoddscolor, xpd=NA, adj = c(0.5,-2.7), cex=1.5, vfont=axisNameFont)
  text(axis.odds, yrange[2], 'odds', col=oddscolor, xpd=NA, adj = c(0.5,-2.7), cex=1.5, vfont=axisNameFont)
  text(axis.log.odds, yrange[1], 'neg inf', col=logoddscolor, xpd=NA, adj = c(0.5,2))
  text(axis.log.odds, yrange[2], 'infinity', col=logoddscolor, xpd=NA, adj = c(0.5,-1))
  text(axis.odds, rescaled.odds(0), 'zero', col=oddscolor, xpd=NA, adj = c(0.5,1))
  text(axis.odds, (yrange[2]), 'infinity', col=oddscolor, xpd=NA, adj = c(0.5,-1))
  
  text(axis.prob, yrange[2], 'prob', col=probcolor, xpd=NA, 
       vfont = axisNameFont, adj = c(0.5,-2.7), cex=1.5)

  # Hershey to see options
  
  text(axis.prob, rescaled.prob(0), 'zero', col=probcolor, xpd=NA, adj=c(-0.5,1))
  text(axis.prob, rescaled.prob(1), 'one', col=probcolor, xpd=NA, adj=c(-0.5,-1))
}
plot3scales()

temparrow = function(x,y,...)
  arrows(x0=x[1], y0=y[1], x1=x[2], y1=y[2], length=0.05, ...)
logoddslist.reduced = 
  logoddslist[ (1:length(logoddslist)) %%5 == 0]
probsToShow = c(0.1, 0.25, 0.5, 0.75, 0.9)
show2paths <- function(logodds) {
  odds = exp(logodds)
  thisProb = odds.to.prob(odds)
  temparrow(c(axis.log.odds, axis.odds), c(logodds, rescaled.odds(odds)), col='black')
  temparrow(c(axis.odds, axis.prob), 
            c(rescaled.odds(odds), rescaled.prob(odds.to.prob(odds))), col='black')
  biggerThanWhich = which(thisProb>probsToShow)
  if(length(biggerThanWhich) > 0) {
    newProb = max(probsToShow[biggerThanWhich])
    text(axis.prob, rescaled.prob(newProb),
         newProb, col=probcolor, xpd=NA, adj=c(-0.2,0))
  }
}

for(logodds in logoddslist.reduced){
  show2paths(logodds)
  readline("wait")
}



