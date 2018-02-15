studentNames = print(strsplit( gsub('  ', '',
  "Bailes,Anna
  Dadashzadeh,Esmaeel (Reza)
  Dunham,Brandan
  Hausler,Ryan M
  Johnson,Adriana
  Rost,Lauren M
  McLaverty,Brian P
  Shapiro,Monica E"),
  split="\n"
)  [[1]] )

studentNames = sapply(strsplit(studentNames, split=','),
   function(n) paste(n[2], n[1]))

pickAStudent = randomStudent = sampleStudent = 
  function(reset = FALSE, withReplacement=FALSE) {
    if(reset) studentsPicked <<- character(0)
    if(withReplacement)
      return(sample(studentNames, size = 1))
    if( ! exists('studentsPicked'))
      studentsPicked <<- character(0)
    names = setdiff(studentNames, studentsPicked)
    print(names)
    who = sample(names, 1, replace = FALSE)
    if(length(studentsPicked) < length(studentNames) - 1)
      studentsPicked <<- c(studentsPicked, who)
    else
      studentsPicked <<- character(0)
    cat('\n# ', length(studentsPicked), ' ', who, '\n')
  }
