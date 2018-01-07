
diceSelected = c(0, 3, 4, 3)
diceTotals = c(10, 10, 15, 15)
diceTable = rbind(diceSelected, diceTotals - diceSelected)
dimnames(diceTable) = list(c("selected", "left"), c("green", "black", "red", "blue"))

diceTable
fisher.test(diceTable)
fisher.test(diceTable[ , c("green", "red")])
