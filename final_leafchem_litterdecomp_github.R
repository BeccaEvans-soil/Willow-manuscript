library(tidyverse)
library(lme4)
library(emmeans)#constrasts
library(ggplot2)
library(pbkrtest)#contrast 
library(MASS)
library(lmerTest)

#mean CN vs individual decomp bags
mean_CNvs_ind_decomp_bags<- lmerTest::lmer(per.mass.lost~n.treat+weevil+open_under+perC_mean+perN_mean+weevil:n.treat+(1|block),  data=decomp_meanCN)
anova(mean_CNvs_ind_decomp_bags)
summary(mean_CNvs_ind_decomp_bags)

#mean CN vs individual decomp bags PRI
mean_CNvs_ind_decomp_bags_pri<- lmerTest::lmer(per.mass.lost~n.treat+weevil+open_under+perC_mean+perN_mean+(1|plot.unq),  data=decomp_meanCN_pri)
anova(mean_CNvs_ind_decomp_bags_pri)

emmeans(mean_CNvs_ind_decomp_bags_pri, list(pairwise ~ n.treat), adjust = "tukey")


#mean CN vs individual decomp bags SEC
mean_CNvs_ind_decomp_bags_sec<- lmerTest::lmer(per.mass.lost~n.treat+weevil+open_under+perC_mean+perN_mean+(1|plot.unq),  data=decomp_meanCN_sec)
anova(mean_CNvs_ind_decomp_bags_sec)
summary(mean_CNvs_ind_decomp_bags_sec)


#Leaf C stats

leafC_stats<- lmerTest::lmer(perC~weevil+succession+weevil:succession+(1|plot.unq),  data=pri_sec_cn)
summary(leafC_stats)


#leaf C primary 
leafC_stats_pri<- lmerTest::lmer(perC~n.treat+weevil+(1|plot.unq),  data=pri_cn_rep2019)

summary(leafC_stats_pri)


#leaf N
leafN_stats<- lmerTest::lmer(perN~n.treat*weevil*succession+(1|plot.unq),  data=pri_sec_cn)

summary(leafN_stats)


#leaf N primary 
leafN_stats_pri<- lmerTest::lmer(perN~n.treat+weevil+(1|plot.unq),  data=pri_cn_rep2019)

summary(leafN_stats_pri)

#leaf d13c
leafd13c_stats<- lmerTest::lmer(d13c~n.treat*weevil*succession+(1|plot.unq),  data=pri_sec_cn)

summary(leafd13c_stats)

#leaf d13c_pri

leafd13c_stats_pri<- lmerTest::lmer(d13c~n.treat+weevil+(1|plot.unq),  data=pri_cn_rep2019)

summary(leafd13c_stats_pri)


#leaf d15n
leafd15n_stats<- lmerTest::lmer(d15n~n.treat+succession+n.treat:succession+(1|plot.unq),  data=pri_sec_cn)
summary(leafd15n_stats)
emmeans(leafd15n_stats, list(pairwise ~ n.treat:succession), adjust = "tukey")

#leaf d15n_pri
leafd15n_stats_pri<- lmerTest::lmer(d15n~n.treat+weevil+(1|plot.unq),  data=pri_cn_rep2019)

summary(leafd15n_stats_pri)
