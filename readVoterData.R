voterDataString = 
  "Dept\tPrefers.Mr.Jones	Prefers.Ms.Smith	undecided	TOTAL
Eng and science	24	23	12	59
Humanities, soc sciences	24	14	10	48
Fine arts	17	8	13	38
Industr, public admin	27	19	9	55
TOTAL	92	64	44	200"
temp = tempfile()
cat(voterDataString, file=temp)
voterData = read.delim(temp, sep = '\t')
str(voterData)
save(voterData, file='voterData.rd')
load('voterData.rd')
