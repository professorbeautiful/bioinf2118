# homework 10

# 6.1
library(ISwR)
plot(y=rmr$metabolic.rate, rmr$body.weight)

rmr.lm = lm(rmr$metabolic.rate ~ rmr$body.weight)

# FAILS   predict(rmr.lm, newdata=data.frame(body.weight=70))

# FAILS   predict(rmr.lm, newdata=data.frame(`rmr$body.weight`=70))
# these didn't work, but... the direct approach:
rmr.lm$coefficients["(Intercept)"] + 70 * rmr.lm$coefficients["rmr$body.weight"] 
##  1305.394 

#  Or, better,
rmr.lm = lm(data=rmr, metabolic.rate ~ body.weight)
predict(rmr.lm, newdata=data.frame(body.weight=70))
rbind( pred.70.c<-predict(rmr.lm, newdata=data.frame(body.weight=70), 
        interval="confidence"),
       pred.70.p<-predict(rmr.lm, newdata=data.frame(body.weight=70), 
        interval="prediction")
)
pred.w.clim = predict(rmr.lm, 
                      interval="confidence")
pred.w.plim = predict(rmr.lm, 
                      interval="prediction")
matplot(rmr$body.weight, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
abline(v=70)

confint(rmr.lm)
