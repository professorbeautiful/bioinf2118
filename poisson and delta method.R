plot(lambda.vec<-exp(seq(log(0.01), 
                         log(1000), length=50)), 
     sapply(lambda.vec, 
            function(lambda)
              var((rpois(1e5, lambda)))),
     log="xy", main="raw Poisson")

plot(lambda.vec<-exp(seq(log(0.01), 
                         log(1000), length=50)), 
     sapply(lambda.vec, 
            function(lambda)
              var(sqrt(rpois(1e5, lambda)))),
     log="xy")
abline(h=1/4)


###  Freeman-Tukey

plot(lambda.vec<-exp(seq(log(0.01), 
	log(1000), length=50)), 
		sapply(lambda.vec, 
		function(lambda) {
			Xvec <- rpois(1e5, lambda);
			var(sqrt(Xvec) + sqrt(Xvec+1))
		}),
  log="x", ylab="variance of transformed data")
abline(h=1)

plot(c(-3,3), c(-3,3), col="white")
lambda.vec <- 10^(seq(log10(0.01), log10(1000), length=6)) 
colorOffset = length(lambda.vec) - min(1, log10(lambda.vec[1]))
invisible(sapply(lambda.vec, 
	function(lambda) {
			Xvec <- rpois(1e4, lambda);
			qq <- qqnorm(plot.it=FALSE,
				(sqrt(Xvec) + sqrt(Xvec+1) - sqrt(4*lambda + 1)))
			#print(str(qq))
			lines(qq$x[order(qq$x)], qq$y[order(qq$x)], col=(round(log10(lambda) + colorOffset)))
		}
		))
legend(-3, 3, legend=as.character(round(lambda.vec,3)), 
	lty=rep(1,length(lambda.vec)), text.col=round(log10(lambda.vec) + colorOffset),
	col=round(log10(lambda.vec) + colorOffset))
