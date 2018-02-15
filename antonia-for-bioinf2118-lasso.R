head(data) 


designMatrix = as.matrix(data[2:8])
solve(t(designMatrix) %*% designMatrix)

solve(t(designMatrix) %*% designMatrix) %*%
  t(designMatrix) %*% designMatrix

data