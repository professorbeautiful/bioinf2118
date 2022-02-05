odds = function(p) p/(1-p)
pshort=c(0.01, 0.1, 0.5, 0.9, 0.99)

p = exp(seq(log(0.01), log(0.99), length=1000))
plot(p, odds(p), log='y', type='l', axes=F,
     ylab="", ylim=c(0.001, 100))
axis(1,      col.axis='red')
mtext('p', 1, col='red', line=2)
points(pshort, odds(pshort))
text(pshort, odds(pshort), 
     signif(digits=2, odds(pshort)) ,
     col='blue', adj=1, xpd=NA, 
     pos=2, offset = 0.5)
text(pshort, 
     0.005 - 0.0035 * (pshort==0.01)
     + 0.01 * (pshort==0.99), 
     signif(digits=2, pshort) ,
     col='red',xpd=NA)
points(pshort, odds(pshort), type='h',
       col='red', lty=3)
# text(pshort[1], 1,
#      "odds(0.01) \n= 1/99", xpd=NA)

text(0.3, 9, "odds of p", col='blue')
