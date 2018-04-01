## noncentral t

curve(dt(x, df=5), from = -6, to=6)
curve(dt(x, df=5, ncp=1),  add=T)
curve(dt(x, df=5, ncp=2),  add=T)