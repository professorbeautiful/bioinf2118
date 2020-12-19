#gun deaths.R
# Firearm-related death rate per 100,000 population per year
# from https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate

deaths = read.delim('gun deaths.txt')
names(deaths)
deaths$Country = gsub("^ *", "", deaths$Country)
USdeaths = deaths[which(deaths$Country=="United States"), 2]
table(deaths$Total <= USdeaths)  ## 64 of 74 are less.
summary(deaths$Total/USdeaths)
rownames(deaths) = deaths$Country
deaths$Homicides = as.character(deaths$Homicides)
deaths$Homicides = as.numeric(gsub(' *\\(.*', '', deaths$Homicides))
deaths$Suicides = as.numeric(gsub(' *\\(.*', '', deaths$Suicides))
deaths[c("United States", "Canada"),]
deaths[c("United States"), c(2,4,5)] / deaths[c( "Canada"),c(2,4,5)] 


