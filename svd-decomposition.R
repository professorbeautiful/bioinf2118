X=matrix(rnorm(9), nrow=3)

svdX = svd(X)

X - svdX$u %*% diag(svdX$d) %*% t(svdX$v)


