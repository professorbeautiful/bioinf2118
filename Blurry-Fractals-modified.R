# https://www.linkedin.com/groups/Blurry-Fractals-I-love-ggplot2-77616.S.5855241489969348609?view=&gid=77616&type=member&item=5855241489969348609&trk=eml-anet_dig-b_nd-pst_ttle-cn

library(ggplot2)
library(numDeriv)
library(RColorBrewer)
library(gridExtra)
## Polynom: choose only one or try yourself
f  <- function (z) {z^3-1}        #Blurry 1
#f  <- function (z) {z^4+z-1}     #Blurry 2
#f  <- function (z) {z^5+z^3+z-1} #Blurry 3

## Supressing texts, titles, ticks, background and legend.
opt <- theme(legend.position="none",
             panel.background = element_blank(),
             axis.ticks=element_blank(), 
             axis.title=element_blank(), 
             axis.text =element_blank())


## original code, comparing color schemes.
z <- outer(seq(-2, 2, by = 0.01), 1i*seq(-2, 2, by = 0.01),'+')
for (k in 1:5) z <- z-f(z)/matrix(grad(f, z), nrow=nrow(z))
z.df <- data.frame(expand.grid(x=seq(ncol(z)), y=seq(nrow(z))), z=as.vector(exp(-Mod(f(z)))))
# Note that "color=z" refers to the z column in z.df.
p1 <- ggplot(z.df, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(8, "Paired")) + opt
p2 <- ggplot(z.df, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(7, "Paired")) + opt
p3 <- ggplot(z.df, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(6, "Paired")) + opt
p4 <- ggplot(z.df, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(5, "Paired")) + opt
# Arrange four plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol=2)

## More interesting: compare iterations (but it takes a little longer)
z <- outer(seq(-2, 2, by = 0.01),1i*seq(-2, 2, by = 0.01),'+')
k = 0
repeat  {
  k = k + 1
  z.df <- data.frame(expand.grid(x=seq(ncol(z)), y=seq(nrow(z))), z=as.vector(exp(-Mod(f(z)))))
  z <<- z-f(z)/matrix(grad(f, z), nrow=nrow(z))
  readline(prompt="Press enter to see plot")
  thePlot = ggplot(z.df, aes(x=x, y=y, color=z)) + 
    geom_tile() +
    scale_colour_gradientn(colours=brewer.pal(8, "Paired")) + 
    ggtitle(paste("iteration ", k)) +
    opt
  print(thePlot)
}


