#HW4 - 1st Proposition:
# Parents who think math is more important have better acomplished kids (?)
#------------------------------------------------------
#reading data:
library(ggplot2)
library(dplyr)
library(highcharter)

timss_student_background = readRDS("/Users/shervin/Downloads/Tims_Data/bsg.rds")
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
  select(idschool,Mean_score_per_school) -> scores_parents

timss_student_background %>% 
  select(idschool,bsbm20h) -> importance_math

importance_and_scores = cbind(importance_math[scores_parents$idschool,],scores_parents)
importance_and_scores <- as.data.frame(importance_and_scores) 
importance_and_scores <- na.omit(importance_and_scores)
importance_and_scores <- importance_and_scores[,-1]
importance_and_scores <- importance_and_scores[,-2]

h = aov(Mean_score_per_school~as.factor(bsbm20h), data = importance_and_scores)
h
summary.aov(h) # it is not mush related pvalue = 0.07
#------------------------------------------------------
#ggplot:
p = ggplot(data = importance_and_scores, aes(bsbm20h, Mean_score_per_school, fill = bsbm20h)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  xlab("degrees of agree(4 is least, 1 is most") +
  ylab("mean score per school") +
  ggtitle("  mean score per school vs degrees of agree of parents") 
p

#------------------------------------------------------
#highcharter:

importance_and_scores %>% group_by(bsbm20h) %>% 
  summarise(sum = sum(Mean_score_per_school,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(bsbm20h,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x= as.integer(bsbm20h) ,y= Average), type="column", name = "scores") 
q


  


