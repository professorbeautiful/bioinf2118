
Answers:
6.3
low
[1] 0.3121693
[1] 0.2636487 0.3606899
ht
[1] 0.03431700 0.09266713
ht, N=10
[1] -0.02946593  0.18331208

6.4
[1] 0.6662844 0.9210172

birthwt = read.delim("../Bioinf-2118-2013 in Dropbox/Shahbaba_Datasets/birthwt.txt",
                     sep=" ")

read.table("../Bioinf-2118-2013 in Dropbox/Shahbaba_Datasets/birthwt.txt",
           header=T)

# From Shahabab chapter 2, page 19.
# • low: indicator of birth weight less than 2.5 kg (0 = normal birth weight, 1 = low
#                                                      birth weight).
# • age: mother’s age in years.
# • lwt: mother’s weight in pounds at last menstrual period.
# • race: mother’s race (1 = white, 2 = African-American, 3 = other).
# • smoke: smoking status during pregnancy (0 = not smoking, 1 = smoking).
# • ptl: number of previous premature labors.
# • ht: history of hypertension (0 = no, 1 = yes).
# • ui: presence of uterine irritability (0 = no, 1 = yes).
# • ftv: number of physician visits during the first trimester.
# • bwt: birth weight in grams.
Nbirthwt = nrow(birthwt)

table(birthwt$low)
prLowBirthWeight = mean(birthwt$low)
stErr_prLowBirthWeight = sqrt(prLowBirthWeight*(1-prLowBirthWeight)/Nbirthwt)

### 85% two-sided confidence interval, using normal approximation:
alpha = (1-0.85)/2
prLowBirthWeight + stErr_prLowBirthWeight * qnorm(c(alpha, 1-alpha))
### Alternatively
qnorm(c(alpha, 1-alpha), mean=prLowBirthWeight, sd=stErr_prLowBirthWeight)

#########

table(birthwt$ht)
prHT = mean(birthwt$ht)
stErr_prHT = sqrt(prHT*(1-prHT)/Nbirthwt)

### 85% two-sided confidence interval, using normal approximation:
alpha = (1-0.90)/2
prHT + stErr_prHT * qnorm(c(alpha, 1-alpha))
### Alternatively
qnorm(c(alpha, 1-alpha), mean=prHT, sd=stErr_prHT)
t.confint(ht)  ### Hmm, a little wider. Ah, it's using the t.

### What if we only had the first 10 observations?
Nbirthwt = 10
prHT = mean(birthwt$ht[1:Nbirthwt])
stErr_prHT = sqrt(prHT*(1-prHT)/Nbirthwt)
qnorm(c(alpha, 1-alpha), mean=prHT, sd=stErr_prHT)


### or the first 11?
Nbirthwt = 13
prHT = mean(birthwt$ht[1:Nbirthwt])
stErr_prHT = sqrt(prHT*(1-prHT)/Nbirthwt)
qnorm(c(alpha, 1-alpha), mean=prHT, sd=stErr_prHT)


### exercise 6.4 shahbaba
names(birthwt)
attach(birthwt)
mean(ftv)
t.confint = function(v, conf=0.90)
  mean(v) + c(1,-1)*qt((1-conf)/2, df=length(v)-1) * sd(v)/sqrt(length(v))
t.confint(ftv)
t.confint(ftv, .95)
t.test(ftv)
###  See the document "Shahbaba exercises 6.4, 6.7, 6.8.R"