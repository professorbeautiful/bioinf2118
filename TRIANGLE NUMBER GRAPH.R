
# TRIANGLE NUMBER GRAPH
plot(Num<-1:10,  Num*(Num-1)/2, xlab="# of partigoers", 
     ylab="number of people pairs")

lines(Num<-seq(1,10, by=0.1),  Num*(Num-1)/2)
points(5, 10, col='red', cex=1.5)
text(x = 5 + 1.75, y=5*4/2 - 3, labels="(5, 10)", 
     col='red')
title('10 = 1 + 2 + 3 + 4\n=  4*(4+1)/2')
     