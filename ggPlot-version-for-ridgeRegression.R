
###... and here is the ggplot2 equivalent.
###  It works but the curves are kind of stupid.
# library(ggplot2)
# qplot(x = 1:length(lambdalist), y = results[ , "Sq Err"],
#       geom ="line") +
#   coord_trans(y="log") +
#   xlab("lambda") +
#   scale_x_discrete(labels=as.character(lambdalist)) +
#   scale_y_continuous(breaks=10^seq(-4,2), labels = round(10^seq(-4,2), 2))
