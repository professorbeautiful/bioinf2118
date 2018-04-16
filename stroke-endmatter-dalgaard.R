###  From the end-matter of Dalgaard.

stroke.trim <- function(t1, t2)
  subset(transform(stroke,
                   entry=t1, exit=pmin(t2, obsmonths),
                   dead=dead & obsmonths <= t2),
         entry < exit)

stroke2 <- do.call(rbind, mapply(stroke.trim,
                                 c(0,0.5,2,12), c(0.5,2,12,Inf), SIMPLIFY=F))
table(stroke$dead)
table(stroke2$dead)
