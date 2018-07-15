#HW4 - 2nd Proposition
# teachers who inspire more, have better achieving students (?)
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
  select(idschool,Mean_score_per_school) -> scores_inspiring

timss_school %>% 
  select(idschool,bcbg14e) -> teacher_inspiring

inspiring_and_scores = cbind(teacher_inspiring[scores_inspiring$idschool,],scores_inspiring)
inspiring_and_scores <- as.data.frame(inspiring_and_scores) 
inspiring_and_scores <- na.omit(inspiring_and_scores)
inspiring_and_scores <- inspiring_and_scores[,-1]
inspiring_and_scores <- inspiring_and_scores[,-2]

h = aov(Mean_score_per_school~as.factor(bcbg14e), data = inspiring_and_scores)
h
summary.aov(h) # it is not mush related pvalue = 0.07
#------------------------------------------------------
#ggplot:
p = ggplot(data = inspiring_and_scores, aes(bcbg14e, Mean_score_per_school, fill = bcbg14e)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  xlab("degrees of inspiring(4 is least, 1 is most") +
  ylab("mean score per school") +
  ggtitle(" mean score per school vs inspiring of teachers") 
p

#------------------------------------------------------
#highcharter:

inspiring_and_scores %>% group_by(bcbg14e) %>% 
  summarise(sum = sum(Mean_score_per_school,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(bcbg14e,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x= as.integer(bcbg14e) ,y= Average), type="column", name = "scores") 
q





