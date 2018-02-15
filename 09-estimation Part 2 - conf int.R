data = c(3, 5, 5, 7, 9, 12, 12, 15, 15, 20)
mean(data)
sd(data)

mean(data) + c(-1, 1) * 
			qnorm(0.975) * sd(data) / sqrt(length(data)) 

pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)
pnorm(4) - pnorm(-4)
plot(temp<-seq(0.1,10, by=0.1), 
     1-(pnorm(temp)-pnorm(-temp)),
     log="y")

### Bootstrapping-- how good is the coverage?

