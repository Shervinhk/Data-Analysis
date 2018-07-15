#HW4-q5:
library(highcharter)
library(ggplot2)
library(dplyr)
#------------------------------------------------------
#reading data:
timss_teacher = readRDS("/Users/shervin/Downloads/Tims_Data/btm.rds")
timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")
#------------------------------------------------------
#finding questions and scores:
timss_student %>% 
  select(c("idschool","bsmmat01","bsmmat02","bsmmat03","bsmmat04","bsmmat05")) %>% 
  mutate(Score = rowSums(.[2:6])/5) %>% 
  select(idschool,Score) -> scores
  scores %>% 
    group_by(idschool) %>% 
    summarise(Total_Score_school = sum(Score,na.rm = TRUE), n = n()) %>% 
    mutate(Mean_score_per_school= Total_Score_school/n) %>% 
    select(idschool,Mean_score_per_school) -> scores_exp
  
#------------------------------------------------------
#finding teacher experience:
timss_teacher %>% 
  select(idschool,btbg01) -> teacher_exp

score_and_experience = cbind(teacher_exp[scores_exp$idschool,] ,scores_exp )
score_and_experience = as.data.frame(score_and_experience)
score_and_experience <- score_and_experience[,-1]
score_and_experience <- score_and_experience[,-2]
score_and_experience <- na.omit(score_and_experience)

#------------------------------------------------------
#AOV test because we have several groups:
h = aov(Mean_score_per_school ~ as.factor(btbg01), data = score_and_experience)
h

summary.aov(h)


#------------------------------------------------------
#ggplot:
p = ggplot(data = score_and_experience, aes(btbg01, Mean_score_per_school, fill = btbg01)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  xlab("number of year of experience") +
  ylab("mean score per school") +
  ggtitle("number of years of experience vs mean score per school") 
p

#------------------------------------------------------
#highcharter:

score_and_experience %>% group_by(btbg01) %>% 
  summarise(sum = sum(Mean_score_per_school,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(btbg01,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x= as.integer(btbg01) ,y= Average), type="column", name = "scores") 
q

