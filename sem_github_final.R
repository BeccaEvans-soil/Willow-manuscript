setwd("C:/Users/becca/Dropbox/Becca-John PhD share/DATA/2021 soils")
#packages
library(tidyverse)
library(lme4)
library(nlme)
library(emmeans)#constrasts
library(lmerTest)
library(ggplot2)
library(MASS)
library(car) #multicollinarity 
library(multcompView)
library("piecewiseSEM")
library(ggeffects)
library(parameters)
library(marginaleffects)
#Data

sem_pri_08 <- read.csv("sem_pri_08.csv")

sem_pri_016 <- read.csv("sem_pri_16.csv")

sem_pri_816 <- read.csv("sem_pri_816.csv")


#create models 0 vs 8 


n <-lme(soil.c~n.treat.dummy+log_sum21+site.dummy,random=~1|block, data=sem_pri_08)

area <-lme(log_sum21~weevil.dummy*n.treat.dummy+log_sum13+site.dummy, random=~1|block,data=sem_pri_08)

site <- lme(log_sum13~site.dummy,random=~1|block, data=sem_pri_08)

#models

summary(n)
anova(n)

summary(area)
anova(area)

summary(site)
anova(site)


#piecewise
modc<- psem(n, area, site)


#conserve returns the most conservative p value
#standardize the path coefficients, default is scale 
#test. type=II or III sig of categorical variables (I used dummy)

summary(modc)


#post hoc effects of weevil*n

emmeans(area, specs = pairwise ~ weevil.dummy:n.treat.dummy, type = "response")



#create models 0 vs 16 


n <-lme(soil.c~n.treat.dummy+log_sum21+site.dummy,random=~1|block, data=sem_pri_016)

area <-lme(log_sum21~weevil.dummy*n.treat.dummy+log_sum13+site.dummy, random=~1|block,data=sem_pri_016)

site <- lme(log_sum13~site.dummy,random=~1|block, data=sem_pri_016)


summary(n)
anova(n)

summary(area)
anova(area)

summary(site)
anova(site)


#piecewise
modc<- psem(n, area, site)


#conserve returns the most conservative p value
#standardize the path coefficients, default is scale 
#test. type=II or III sig of categorical variables (I used dummy)

summary(modc)

#post hoc effects of weevil*n

emmeans(area, specs = pairwise ~ weevil.dummy:n.treat.dummy, type = "response")



#create models 8 vs 16 


n <-lme(soil.c~n.treat.dummy+log_sum21+site.dummy,random=~1|block, data=sem_pri_816)

area <-lme(log_sum21~weevil.dummy*n.treat.dummy+log_sum13+site.dummy, random=~1|block,data=sem_pri_816)

site <- lme(log_sum13~site.dummy,random=~1|block, data=sem_pri_816)


summary(n)
anova(n)

summary(area)
anova(area)

summary(site)
anova(site)


#piecewise
modc<- psem(n, area, site)


#conserve returns the most conservative p value
#standardize the path coefficients, default is scale 
#test. type=II or III sig of categorical variables (I used dummy)

summary(modc)

#post hoc effects of weevil*n

emmeans(area, specs = pairwise ~ weevil.dummy:n.treat.dummy, type = "response")


