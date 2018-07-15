#HW4 - 3rd Proposition
#Schools with Better Heating systems have better achiving students (?)
#------------------------------------------------------
#reading data:
library(ggplot2)
library(dplyr)
library(highcharter)

timss_school = readRDS("/Users/shervin/Downloads/Tims_Data/bcg.rds")
timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")
#------------------------------------------------------
#questions and scores:
timss_student %>% 
  select(c("idschool","bsmmat01","bsmmat02","bsmmat03","bsmmat04","bsmmat05")) %>% 
  mutate(Score = rowSums(.[2:6])/5) %>% 
  select(idschool,Score) -> scores
scores %>% 
  group_by(idschool) %>% 
  summarise(Total_Score_school = sum(Score,na.rm = TRUE), n = n()) %>% 
  mutate(Mean_score_per_school= Total_Score_school/n) %>% 
  select(idschool,Mean_score_per_school) -> scores_heating

timss_school %>% 
  select(idschool,bcbg13ad) -> heating_system

heating_and_scores = cbind(heating_system[scores_heating$idschool,],scores_heating)
heating_and_scores <- as.data.frame(heating_and_scores) 
heating_and_scores <- na.omit(heating_and_scores)
heating_and_scores <- heating_and_scores[,-1]
heating_and_scores <- heating_and_scores[,-2]

h = aov(Mean_score_per_school~as.factor(bcbg13ad), data = heating_and_scores)
h
summary.aov(h) # it is not mush related pvalue = 0.07
#------------------------------------------------------
#ggplot:
p = ggplot(data = heating_and_scores, aes(bcbg13ad, Mean_score_per_school, fill = bcbg13ad)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  xlab("degrees of heating(4 is alot, 1 is not at all") +
  ylab("mean score per school") +
  ggtitle(" mean score per school vs heating systems") 
p

#------------------------------------------------------
#highcharter:

heating_and_scores %>% group_by(bcbg13ad) %>% 
  summarise(sum = sum(Mean_score_per_school,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(bcbg13ad,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x= as.integer(bcbg13ad) ,y= Average), type="column", name = "scores") 
q





