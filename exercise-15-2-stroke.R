#  Exercises with the stroke data:  10.4 and 15.2


### exercise 10.4
#10.4 Split the stroke data according to obsmonths into time intervals 0–0.5,
#0.5–2, 2–12, and 12+ months after stroke.

library(ISwR)
dim(stroke)
names(stroke)
summary(stroke$obsmonths)

cutvalues =  c(0, 0.5, 2, 12, Inf)
obsmonths_binned = cut(stroke$obsmonths, cutvalues)
table(obsmonths_binned)
sum(.Last.value)  ### 829
dim(stroke) ### OK, 829 rows.  That's our sample size. All accounted for.

## Now, the invaluable function "split".
splitstroke = split(stroke, obsmonths_binned)
length(splitstroke)  ### one for each of the four intervals
names(splitstroke)
str(splitstroke[[1]])
## So we see the structure: stroke data frame has been split into a list of data
## frames, one for each bin.


### exercise 15.2

# 15.2 With the split stroke data from Exercise 10.4, fit a Poisson regres- sion
# model corresponding to a constant hazard in each interval and with
# multiplicative effects of age and sex.

stroke$obsmonths_binned = obsmonths_binned

## We want a data frame where for each observation we have
##    - total obs time in each interval
##    - whether the stroke happened in that interval

cutvaluematrix = cbind(cutvalues[-5], cutvalues[-1]) 
colnames(cutvaluematrix) = c("start", "end")
## This will give us the four intervals.

totalTimeOnStudyByInterval = 
  apply(cutvaluematrix, 1, function(interval) {
    pmax( 0, pmin(stroke$obsmonths, interval[2]) - interval[1])
  })
colnames(totalTimeOnStudyByInterval) = paste0("timeIn", levels(obsmonths_binned))
### We add the four totalTimeOnStudyByInterval columns to our data frame.
strokePlus = data.frame(stroke, totalTimeOnStudyByInterval)
strokePlus[1:6, c("obsmonths", "obsmonths_binned", 
                  grep("timeIn", names(strokePlus), value=TRUE))]
strokePlus$id = 1:nrow(stroke)  ## We'll need this id soon.

### Make four copies of strokePlus, for selected columns.
strokeLong = as.data.frame(
  sapply(strokePlus[,  c("id","dead","obsmonths","age","sex")], rep, length(cutvalues)-1))
## Mark each copy with one of the intervals, and beginning and end.
strokeLong$interval = rep(levels(obsmonths_binned), each=nrow(stroke))
strokeLong$beginning = rep(cutvaluematrix[ , "start"], each=nrow(stroke))
strokeLong$end = rep(cutvaluematrix[ , "end"], each=nrow(stroke))
strokeLong$timeInInt = c(totalTimeOnStudyByInterval)

### Remove rows with no observation time in the interval
strokeLong = subset(x = strokeLong, subset=(timeInInt>0))

### Did the person die in this interval?
strokeLong$deadHere = strokeLong$dead & (strokeLong$obsmonths < strokeLong$end )

### Reordering by id will help us check if we did this right.
strokeLong = strokeLong[order(strokeLong$id), ]
strokeLong[strokeLong$id < 5, ]
table(strokeLong$deadHere)
table(stroke$dead)  ### confirmed the right number of deaths.

## OK ready to go.  Let's model.

glm.stroke = glm(family=poisson, data=strokeLong, 
    deadHere ~ interval + age + sex,
    offset = log(timeInInt))
print(glm.stroke)
summary(glm.stroke)
anova(glm.stroke)    
