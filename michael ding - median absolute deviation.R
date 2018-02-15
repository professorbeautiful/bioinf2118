### michael ding - median absolute deviation

N=10000
U=10
bimod <- function (N=10000, U=10) {
  x = c(rnorm(n = N/2, -U, 1), rnorm(n = N/2, +U, 1))
  (x-mean(x))/sd(x)
}


mads = sapply(0:30, function(U) {
  x=bimod(U=U)
  xs=(x-mean(x))/sd(x)
  median(abs(xs))
  })
plot(0:30, mads)
x3 = bimod(N)
plot(density(x3))
x1s = (x1 -mean(x1))/sd(x1)
plot(density(x1s))

x2 = rnorm(N)
x2s = (x2 -mean(x2))/sd(x2)
lines(density(x2s))


median(abs(x1s))
median(abs(x2s))


       