# png(400,400,file="sons.and.fathers.png")
fs.mean <- 70 	### Mean height for sons & fathers
fs.sd <- 5		### Standard deviation of heights
fs.cor <- 0.7 	### Correlation between heights
fathers.and.sons.covariance <- 	fs.sd^2 * 
		rbind(	c(1, fs.cor),
				c(fs.cor, 1))
require(mvtnorm)  ###### Install package first!
fathers.and.sons <- #### Generate the data
	rmvnorm(500, mean=c(fs.mean, fs.mean), 
		sigma=fathers.and.sons.covariance)
colnames(fathers.and.sons) <- cq(father,son)
head(fathers.and.sons)
cor(fathers.and.sons); var(fathers.and.sons)

plot(fathers.and.sons,
     xlab="Father's height",
     ylab="Son's height")
title("Height of fathers and sons")

### Matrix conversion between a circle and an ellipse.
fathers.and.sons.svd <- svd(fathers.and.sons.covariance)
fathers.and.sons.scaling <- 
		fathers.and.sons.svd$u %*% 
		diag(sqrt(fathers.and.sons.svd$d)) %*%
		fathers.and.sons.svd$v

### Which points are inside the 50% contour?
fs.scaledDistance <- apply(FUN=sum, MARGIN=1,
	((fathers.and.sons-fs.mean) %*% 
	 solve(fathers.and.sons.scaling) ) ^2)
fs.isInside <-(fs.scaledDistance < qchisq(0.5, 2))
points(fathers.and.sons[fs.isInside, ], col="red")
###  Check: should be ~ 50% of points inside:
mean(fs.isInside)  ###  OK!!!

#### Now let's plot the 50% contour:
angles<-seq(0,2*pi,length=400)
fathers.and.sons.contour.50 <- fs.mean +
	sqrt(qchisq(0.5, 2)) * 
	cbind(cos(angles), sin(angles)) %*% 
	fathers.and.sons.scaling
lines(fathers.and.sons.contour.50, col="red", lwd=2)

#######   Major axis = diagonal:
abline(a=0, b=1, col="blue", lwd=3)
#### Note the symmetry around the diagonal.

#######	Regression line:   conditional expectation:
fs.slope <- fathers.and.sons.covariance[1,2] /
			sqrt(  fathers.and.sons.covariance[1,1]
				* fathers.and.sons.covariance[2,2])
fs.intercept <- fs.mean + (0-fs.mean)*fs.slope
abline(a=fs.intercept, b=fs.slope, 
	col="darkgreen", lwd=3)
legend("topleft", c("regression line", "major axis", "50% contour", 
	"50% highest density"),
	lty=c(1,1,1,0), pch=c("","","", "o"), 
	lwd=c(3,3,2,0), col=cq(darkgreen,blue,red,red),
	cex=0.7)
regression.to.the.mean.arrow = function(X)
  arrows(x0=X, x1=X, y0=X, y1=fs.intercept+fs.slope*X, lwd = 7)
regression.to.the.mean.arrow(60)
regression.to.the.mean.arrow(80)
legend("bottomright", "regression to the mean",  lwd=6)
