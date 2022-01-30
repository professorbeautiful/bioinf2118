points = data.frame(x=sin((1:10)*2*pi/10), y=cos((1:10)*2*pi/10))

plot(points$x, points$y, axes=F, pch="")
for (i in 1:10)
  text(points$x[i], points$y[i], (bquote(A[.(i)])),
       col='green')

for(i in 1:9) for(j in i:10) 
  lines(points$x[c(i,j)], points$y[c(i,j)], col="grey")

?plotmath

#text(1, 1, expression(A[1]))

