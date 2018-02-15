# https://www.linkedin.com/groups/Blurry-Fractals-I-love-ggplot2-77616.S.5855241489969348609?view=&gid=77616&type=member&item=5855241489969348609&trk=eml-anet_dig-b_nd-pst_ttle-cn

library(ggplot2)
library(numDeriv)
library(RColorBrewer)
library(gridExtra)
## Polynom: choose only one or try yourself
#f  <- function (z) {z^3-1}        #Blurry 1
#f  <- function (z) {z^4+z-1}     #Blurry 2
f  <- function (z) {z^5+z^3+z-1} #Blurry 3
z <- outer(seq(-2, 2, by = 0.01),1i*seq(-2, 2, by = 0.01),'+')
for (k in 1:5) z <- z-f(z)/matrix(grad(f, z), nrow=nrow(z))
## Supressing texts, titles, ticks, background and legend.
opt <- theme(legend.position="none",
             panel.background = element_blank(),
             axis.ticks=element_blank(), 
             axis.title=element_blank(), 
             axis.text =element_blank())
z <- data.frame(expand.grid(x=seq(ncol(z)), y=seq(nrow(z))), z=as.vector(exp(-Mod(f(z)))))
# Create plots. Choose a palette with display.brewer.all()
p1 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(8, "Paired")) + opt
p2 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(7, "Paired")) + opt
p3 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(6, "Paired")) + opt
p4 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(5, "Paired")) + opt
# Arrange four plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol=2)