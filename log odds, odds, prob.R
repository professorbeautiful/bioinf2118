axis.log.odds = 0
axis.odds = 3
axis.prob = 6
logoddscolor = 'red'
oddscolor = 'blue'
probcolor = 'green'
axisNameFont = c('serif', 'bold italic')

yrange = c(-5,5)
plot(c(-1,axis.prob), yrange, axes=F, pch=' ',
     xlab='',ylab='')
logoddslist = seq(yrange[1],yrange[2],length=100)
oddslist = exp(logoddslist)
problist = odds.to.prob(oddslist)
rescaled.prob = function(p,
                         expander = yrange[2]-yrange[1], 
                         offset = yrange[1])
  p*expander + offset
sapply(logoddslist, function(logodds)
  {
  odds = exp(logodds)
  lines(c(axis.log.odds, axis.odds), c(logodds, odds), col='lightgrey')
  lines(c(axis.odds, axis.prob), 
        c(odds, rescaled.prob(odds.to.prob(odds))), col='lightgrey')
})
abline(v=axis.log.odds, col=logoddscolor)
lines(c(axis.odds, axis.odds), c(0, max(oddslist)), lwd=3, col=oddscolor)
lines(c(axis.prob, axis.prob), rescaled.prob(c(0,1)), lwd=3, col=probcolor)

text(axis.log.odds, yrange[2], 'log odds', col=logoddscolor, xpd=NA, adj = c(0.5,-2.2), vfont=axisNameFont)
text(axis.odds, yrange[2], 'odds', col=oddscolor, xpd=NA, adj = c(0.5,-2.2), vfont=axisNameFont)
text(axis.log.odds, yrange[1], 'neg inf', col=logoddscolor, xpd=NA)
text(axis.log.odds, yrange[2], 'infinity', col=logoddscolor, xpd=NA, adj = c(0.5,-1))
text(axis.odds, 0, 'zero', col=oddscolor, xpd=NA, adj = c(0,1))
text(axis.odds, yrange[2], 'infinity', col=oddscolor, xpd=NA, adj = c(0.5,-1))

text(axis.prob, yrange[2], 'prob', col=probcolor, xpd=NA, 
     vfont = axisNameFont, adj = c(0.5,-2.2))
# Hershey to see options
text(axis.prob, rescaled.prob(0), 'zero', col=probcolor, xpd=NA, adj=c(-0.2,0))
text(axis.prob, rescaled.prob(1), 'one', col=probcolor, xpd=NA, adj=c(-0.2,-0))

temparrow = function(x,y,...)
  arrows(x0=x[1], y0=y[1], x1=x[2], y1=y[2], length=0.05, ...)
for(logodds in logoddslist[ (1:length(logoddslist)) %%5 == 0]){
  odds = exp(logodds)
  temparrow(c(axis.log.odds, axis.odds), c(logodds, odds), col='black')
  temparrow(c(axis.odds, axis.prob), 
        c(odds, rescaled.prob(odds.to.prob(odds))), col='black')
  readline("wait")
}


