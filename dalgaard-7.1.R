zelazo
sapply(zelazo, summary)
zelazo.df = data.frame(group=rep(names(zelazo), times=sapply(zelazo, length)),
                       age=unlist(zelazo))
lm(data=zelazo.df, age ~ group)
anova(lm(data=zelazo.df, age ~ group))
summary(lm(data=zelazo.df, age ~ group))

lm(data=zelazo.df, age ~ (group=="none"))
summary(..())

bartlett.test(data=zelazo.df, age ~ group)

pairwise.t.test(x = zelazo.df$age, g=zelazo.df$group)

wilcox.test(zelazo$active, zelazo$none)
wilcox.test(zelazo$active, zelazo$ctr.8w)
