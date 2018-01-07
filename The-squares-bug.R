##  Testing symbols with squares.
## The squares arg does not do the vertical dimension correctly.
## use rectangles instead.


symbols(1/2, 1/2, squares=0.2, add=FALSE, inches=F,
        bg="green")
abline(h=c(.4,.6), v=c(.4,.6))

symbols(1/2, 1/2, rectangle=matrix(c(0.2,0.2), nrow=1),
         add=FALSE, inches=F, bg="green")
abline(h=c(.4,.6), v=c(.4,.6))
