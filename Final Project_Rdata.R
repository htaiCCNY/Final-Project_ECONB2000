attach(dat_ATUS)

library(sjPlot)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(AER)
library(car)
install.packages("rgl")

### Data/ Variable Cleaning 



colnames(dat_ATUS)
df <- (dat_ATUS)

summary(df)
nrow(df)

no_NA <- na.omit(df)
nrow(no_NA)

DATA <- (no_NA)
summary(DATA)
NROW(DATA)

DF <- as.data.frame(DATA)
nrow(DF)

dim(DF[complete.cases(DF),])

options(max.print = 1000000)



Edu_Attainment <- as.numeric(SPEDUC)
Time_EDUC <-as.numeric(ACT_EDUC)
Immigration_Year<- as.numeric(YRIMMIG)
Race <- as.numeric(RACE)
Cit_Status <- as.numeric(CITIZEN)
age <- SPAGE




Edu_Attainment_fac <- as_factor(SPEDUC)
Time_EDUC_fac <-as_factor(ACT_EDUC)
Immigration_Year_fac<- as_factor(YRIMMIG)
Race_fac <- as_factor(RACE)
Cit_Status_fact <- as_factor(CITIZEN)
age <- SPAGE

summary(Cit_Status_fact)

DF_EDUC <- data.frame(Time_EDUC,Cit_Status,Edu_Attainment,Race, age)
DF_Factor <- data.frame(Edu_Attainment_fac,Time_EDUC_fac,Immigration_Year_fac,Race_fac,Cit_Status_fact,age)


### Summary Statistics 



sjPlot::tab_xtab(var.row = Edu_Attainment_fac, var.col = Time_EDUC_fac ,title = "Education Level & Time Spent on Educational Activities",show.row.prc = "TRUE")
sjPlot::tab_xtab(var.row = Time_EDUC, var.col = Race , title = "Time Spent on Educational Activities and Year of Immigration", show.row.prc = "TRUE")

glimpse (Time_EDUC_fac)
glimpse (Edu_Attainment_fac)
glimpse (Race_fac)
glimpse(age)


summary(DF_Factor) ###  Variables as factor, for categorical data
summary(DF_EDUC)  ###  Variables as numeric

stargazer(DF_EDUC,type = "text", out = "table")

###  Regression Models 

Iso <- lm(Edu_Attainment~Cit_Status)
Iso2 <- lm(Edu_Attainment~Cit_Status_fact)
Iso
Iso2
summary(Iso)
plot(Iso)



summary(Iso2)


oneway.test_iso<-aov(Iso)
oneway.test_iso
coeftest(Iso)

stargazer(Iso, type = "text")

stargazer(Iso2, type="text")

###  Model1: Educational Attainment level by 
###  Citizenship Status, time spent on educational activities, and race

Model1 <-lm(Edu_Attainment~Cit_Status+Time_EDUC+Race)
Model1
Mod1Fac <- lm(Edu_Attainment_fac~Cit_Status_fact+Time_EDUC_fac+Race_fac)
Mod1Fac

stargazer(Mod1Fac, type = "text")

summary(Model1)
stargazer(Model1, type = "text")


coeftest(Model1)
coefMod1 <- coeftest((Model1))
cor(Cit_Status, Edu_Attainment)
stargazer(coefMod1, type = "text")

scatterplot(Cit_Status_fact,Edu_Attainment_fac)

plot(Model1)
plot1 <- plot(Model1)
abline(plot1)


one.way.ANOVA <- aov(Edu_Attainment~Cit_Status,data = DF_EDUC)
one.way.ANOVA

###   Model1 shows that the chosen independent variables are statistically significant
###   Significant at the "0" level. R squared < 1 and low P value indicate the fitness of model
###   but visual plots show a better fit. 

###   Model 2
###  Educational Attainment and Time spent on educational activity + age

Model2 <- lm(Edu_Attainment~Time_EDUC + age)
Model2
summary(Model2)                  
plot(Model2)
scatterplot(Edu_Attainment,age)

model2A <- (Edu_Attainment_fac~Time_EDUC_fac+age)
model2A

stargazer(model2A, type = "text")
stargazer(Model2, type = "text", title = "Educational Attainment and Time spent on.Ed, and age", out = "text")

one.way.ANOVAM2 <-aov(Edu_Attainment~Time_EDUC + age)
one.way.ANOVAM2

Age_timeeduc <- lm(age~Time_EDUC)
Age_timeeduc
summary(Age_timeeduc)
scatterplot(age, Time_EDUC)

