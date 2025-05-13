axis.log.odds = 0
axis.odds = 2
axis.prob = 4
logoddscolor = 'red'
oddscolor = 'blue'
probcolor = 'green'
axisNameFont = c('serif', 'bold italic')

yrange = c(-5,12)
plot(c(-1,axis.prob), yrange, axes=F, pch=' ',
     xlab='',ylab='')
logoddslist = seq(yrange[1],yrange[2],length=100)
oddslist = exp(logoddslist)
problist = odds.to.prob(oddslist)
rescaled.odds = function(od,
                         expander = 0.015*(yrange[2]-yrange[1]), 
                         offset = yrange[1]+ 0.2)
  od*expander + offset
rescaled.prob = function(p,
                         expander = yrange[2]-yrange[1], 
                         offset = yrange[1])
  p*expander + offset
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

text(axis.log.odds, yrange[2], 'log odds', col=logoddscolor, xpd=NA, adj = c(0.5,-2.2), vfont=axisNameFont)
text(axis.odds, yrange[2], 'odds', col=oddscolor, xpd=NA, adj = c(0.5,-2.2), vfont=axisNameFont)
text(axis.log.odds, yrange[1], 'neg inf', col=logoddscolor, xpd=NA, adj = c(0.5,2))
text(axis.log.odds, yrange[2], 'infinity', col=logoddscolor, xpd=NA, adj = c(0.5,-1))
text(axis.odds, rescaled.odds(0), 'zero', col=oddscolor, xpd=NA, adj = c(0,1))
text(axis.odds, (yrange[2]), 'infinity', col=oddscolor, xpd=NA, adj = c(0.5,-1))

text(axis.prob, yrange[2], 'prob', col=probcolor, xpd=NA, 
     vfont = axisNameFont, adj = c(0.5,-2.2))
# Hershey to see options
text(axis.prob, rescaled.prob(0), 'zero', col=probcolor, xpd=NA, adj=c(-0.2,1.2))
text(axis.prob, rescaled.prob(1), 'one', col=probcolor, xpd=NA, adj=c(-0.2,-1))

temparrow = function(x,y,...)
  arrows(x0=x[1], y0=y[1], x1=x[2], y1=y[2], length=0.05, ...)
logoddslist.reduced = 
   logoddslist[ (1:length(logoddslist)) %%5 == 0]
LRlist = rep(10,4)
logLRlist = log(LRlist)
priorOdds = 0.01
logoddslist.reduced = log(priorOdds) +
  c(0, cumsum( logLRlist) )

#probsToShow = c(0.1, 0.25, 0.5, 0.75, 0.9)
for(logoddsUpTo in seq(along=logoddslist.reduced)){
  logodds = logoddslist.reduced[logoddsUpTo]
  odds = exp(logodds)
  thisProb = odds.to.prob(odds)
  (paste(logodds, odds, thisProb, '\n'))
  if(logoddsUpTo>1) {  # incorporate a datum, draw vertical arrow.
    logodds.pair = c(logoddslist.reduced[logoddsUpTo-1], logodds)
    temparrow(c(axis.log.odds, axis.log.odds) - 0.05, 
            logodds.pair, col='black', lwd=2)
    text(axis.log.odds - 0.4, mean(logodds.pair), 
         paste0("R", logoddsUpTo - 1) )
  }
  #  arrows across 3axes.
  temparrow(c(axis.log.odds, axis.odds), 
            c(logodds, rescaled.odds(odds)), col='black')
  temparrow(c(axis.odds, axis.prob), 
        c(rescaled.odds(odds), rescaled.prob(thisProb)), col='black')
  #biggerThanWhich = which(thisProb>probsToShow)
  # if(length(biggerThanWhich) > 0) {
  #   newProb = max(probsToShow[biggerThanWhich])
  #   text(axis.prob, rescaled.prob(newProb),
  #        newProb, col=probcolor, xpd=NA, adj=c(-0.2,0))
  # }
  # text(mean(c(axis.odds, axis.prob)), rescaled.odds(odds),labels = 
  #        round(digits=3, (odds)),
  #      , col=oddscolor, xpd=NA, adj=c(-0.2,0))
  text(axis.prob, rescaled.prob(thisProb),labels = 
       round(digits=3, (thisProb)),
               , col=probcolor, xpd=NA, adj=c(-0.2,0))
       
  readline("wait")
}



