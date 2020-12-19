x = rnorm(20); y = (x > 0); 
tooGood = data.frame(x, y)
tooGoodFit = glm(y~x, family='binomial');
plot(x,y);  
coef(tooGoodFit); 
cbind(tooGood$y, exp(predict(tooGoodFit)), predict(tooGoodFit) 
      )[order(predict(tooGoodFit)), ]
plot( exp(predict(tooGoodFit)) , tooGood$y , log='x')

require('penalized')
vignette("penalized")
tooGoodFit.ridge = penalized(response = y, penalized = x, lambda1 = 0, lambda2 = 1)
tooGoodFit.ridge.fitted = fitted(tooGoodFit.ridge, penalized='x')
cbind(tooGood$y, tooGoodFit.ridge.fitted  
      ) [order(tooGoodFit.ridge.fitted), ]
plot( tooGoodFit.ridge.fitted , tooGood$y )

#### lasso ####
tooGoodFit.lasso = penalized(response = y, penalized = x, lambda1 = 1, lambda2 = 0)
tooGoodFit.lasso.fitted = fitted(tooGoodFit.lasso, penalized='x')
cbind(tooGood$y, tooGoodFit.lasso.fitted  
) [order(tooGoodFit.lasso.fitted), ]
plot( tooGoodFit.lasso.fitted , tooGood$y )

plot( tooGoodFit.lasso.fitted , tooGoodFit.ridge.fitted )
