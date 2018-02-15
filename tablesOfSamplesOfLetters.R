makeATable = function(index) {
  myTable = table(sample(letters, size = 100, replace = T))
  result = rep(0, 26)
  names(result) = letters
  result[names(myTable)] = myTable
  result
}
makeATable(NA)

theSamples = sapply(1:500, makeATable)
str(theSamples)

table(theSamples["a", ])
table(apply(theSamples, 1, max))
cor(t(theSamples)) [1:5, 1:5]
