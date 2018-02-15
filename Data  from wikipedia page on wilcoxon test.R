### Data  from wikipedia page on wilcoxon test

temp = tempfile()
cat("1	125	110	1	15
2	115	122	-1	7
3	130	125	1	5
4	140	120	1	20
5	140	140	 NA	0
6	115	124	-1  9
7	140	123	1	17
8	125	137	 -1	12
9	140	135	1	5
10	135	145	 -1	10", file=temp)
data.orig = read.table(temp)

data = data.orig [, 2:3]

colnames(data) = c("X", "Y")
