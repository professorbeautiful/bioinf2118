odds = function(p) p/(1-p)
pshort=c(0.01, 0.1, 1/4, 0.5, 3/4, 0.9, 0.99)

p = exp(seq(log(0.01), log(0.99), length=1000))
plot(p, odds(p), log='y', type='l', axes=F,
     ylab="", xlab="", ylim=c(0.001, 100))
axis(1,      col.axis='red',
     at = 
       c(0.01,.1,.25,.5,.75,.9,.99)
      )
mtext('p', 1, col='red', line=2)
points(pshort, odds(pshort))
oddsvalues = c('1/100', '1/10',
               '1/3', '1',
               '3', '9', '99')
text(pshort, odds(pshort), 
     oddsvalues ,
     col='blue', adj=1, xpd=NA 
     , pos=3, offset = 0.5
     )
pticks= c(0.1, 0.9)
text(pticks, 
     0.003 - 0.0035 * (pticks==0.01)
     + 0.01 * (pticks==0.99), 
     signif(digits=2, pticks) ,
     col='red',xpd=NA)
points(pshort, odds(pshort), type='h',
       col='red', lty=3)
# text(pshort[1], 1,
#      "odds(0.01) \n= 1/99", xpd=NA)

arrows(-0.1, 0.001, y1= 90, xpd=NA,
       col='blue') 
arrows(-0.1, y0= 90, y1=0.001, xpd=NA,
       col='blue') 
text(xpd=NA,
     -0.1, 9999, "odds\n = p/(1-p)", col='blue')
text(xpd=NA,
     -0.1, 500, "Infinity", col='blue')
text(xpd=NA,
     -0.1, 1/10000, "Zero", col='blue')
