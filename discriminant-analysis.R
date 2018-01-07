library(MASS)
## MASS has many many useful functions!
## "Modern Applied Statistics with S-Plus", Venables and Ripley
my.da = lda
CV = F
irisData = iris[iris$Species!="setosa",]
fit <- my.da(Species ~ 
           #  Sepal.Length + Sepal.Width, 
            Petal.Length + Petal.Width,
           data=irisData, 
           na.action="na.omit", CV=CV )
fit # show results

with(irisData, plot(Petal.Length, Petal.Width,
                col=as.numeric(Species) + 1))
if(CV) table(fit$class, iris$Species)

if(!CV) {
  scaling = fit$scaling
#  abline(a=(1/2)/scaling["Petal.Width","LD1"], b=scaling["Petal.Length","LD1"]/scaling["Petal.Width","LD1"])
#  abline(a=(1/2)/scaling["Petal.Width","LD2"], b=scaling["Petal.Length","LD2"]/scaling["Petal.Width","LD2"])
  plot(fit, abbrev=3)
  }
