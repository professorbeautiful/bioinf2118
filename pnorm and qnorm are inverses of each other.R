##  pnorm and qnorm are inverses of each other

par(mfrow=c(1,2))

curve(from = -4, to = -2, expr = pnorm)
curve(from = 0, to = 0.1, expr = qnorm)
