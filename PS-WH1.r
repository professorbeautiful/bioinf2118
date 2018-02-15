#Problem 1: Refer to the diagnostic testing table

diagnostic = rbind(c(0.95, 0.03, 0.02), c(0.03, 0.95, 0.02))

dimnames(diagnostic) = list( c("healthy","sick"),c("negative","positive","indeterminate"))

diagnostic

#Problem 2: Write a function that takes as its arguments an outcome and a state-of-nature, and returns the probability of that outcome
#
# example of use: hw2("negative", "healthy")
#
hw2 = function(outcome, state_of_nature){
  prob = 0
  if(outcome == "negative"){
    if(state_of_nature == "healthy"){prob = 0.95}
    if(state_of_nature == "sick"){prob = 0.03}
  }
  if(outcome == "positive"){
    if(state_of_nature == "healthy"){prob = 0.03}
    if(state_of_nature == "sick"){prob = 0.95}
  }
  if(outcome == "indeterminate"){
    if(state_of_nature == "healthy"){prob = 0.02}
    if(state_of_nature == "sick"){prob = 0.02}
  }
  return(prob)
}

#Problem 2: Second Version
#

hw2B = function(outcome, state_of_nature){
  colIndex = 0
  rowIndex = 0
  for(i in 1:length(rownames(diagnostic))) {
    if(state_of_nature==rownames(diagnostic)[i]){
      rowIndex = i
    }
  }
  for(i in 1:length(colnames(diagnostic))){
    if(outcome==colnames(diagnostic)[i]){
      colIndex = i
    }
  }

 return(diagnostic[rowIndex,colIndex])
}

#Problem 3: Generalize that function to handle an event as well as an outcome
#
# example of use: hw3(c("positive", "indeterminate"), "sick");
#

hw3 = function(event, state_of_nature){
  
  rowIndex = 0
  for(i in 1:length(rownames(diagnostic))) {
    if(state_of_nature==rownames(diagnostic)[i]){
      rowIndex = i
    }
  }

prob = 0;
  for(i in 1:length(event)){
    outcome = event[i];
    for(j in 1:length(colnames(diagnostic))){
      if(outcome==colnames(diagnostic)[j]){
	prob = prob + diagnostic[rowIndex,j]
      }
    }
  }

 return(prob)
}



#Problem 4: Generalize that function to take a third argument which is the model family
#
# example of use: hw4(c("positive", "indeterminate"), "sick", diagnostic);
#
hw4 = function(event, state_of_nature, model_family){
  
  rowIndex = 0
  for(i in 1:length(rownames(model_family))) {
    if(state_of_nature==rownames(model_family)[i]){
      rowIndex = i
    }
  }

prob = 0;
  for(i in 1:length(event)){
    outcome = event[i];
    for(j in 1:length(colnames(model_family))){
      if(outcome==colnames(model_family)[j]){
	prob = prob + model_family[rowIndex,j]
      }
    }
  }

 return(prob)
}

#Problem 5: For this last function perform a validation that is working correctly Make sure it works "at the boundaries"
