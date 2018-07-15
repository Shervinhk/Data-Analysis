#HW4- q1:
library(highcharter)
library(ggplot2)
library(dplyr)
#------------------------------------------------------
timss_teacher = readRDS("/Users/shervin/Downloads/Tims_Data/btm.rds")
timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")

#------------------------------------------------------
#selecting questions and finding the scores:
timss_student %>% 
  select(c("idschool","bsmmat01","bsmmat02","bsmmat03","bsmmat04","bsmmat05")) %>% 
  mutate(Score = rowSums(.[2:6])) %>% 
  select("idschool","Score") -> scores
#------------------------------------------------------
#finding mean value of scores in every school:
  scores %>%  group_by(idschool) %>% 
  summarise(School_score = sum(Score), n = n()) %>% 
    mutate(Mean_student = School_score/(5*n)) %>% 
    select(idschool,Mean_student)  -> Student_score_school
#------------------------------------------------------
#binding results:

timss_teacher %>%
  select(c(2),"btdgtjs") -> check2

  checkaov = cbind(scores[check2$idschool,],check2) 
  checkaov <- checkaov[,-c(1)]
  checkaov <- checkaov[,-c(2)]
#------------------------------------------------------ 
# Using ANOVA because we have several groups:
  h = aov(Score ~ as.factor(btdgtjs), data = checkaov)
  h
  summary.aov(h)
#------------------------------------------------------  
#we will also use t.test if we consider 1,2 one group and 3,4 another group:
timss_teacher %>%
  select(c(2),"btdgtjs") %>% 
  mutate( Satisfied = (btdgtjs==1 | btdgtjs==2 )) ->teacher_raw
scores = Student_score_school[teacher_raw$idschool,] %>% select(c(2))
teacher_raw <- teacher_raw %>%  select(c(3))
results = cbind( teacher_raw ,scores )

hypo = t.test(Mean_student~Satisfied, data = results)
hypo
#------------------------------------------------------  
#ggplot:
p = ggplot(data = results, aes(x=Mean_student, fill= Satisfied))+
  geom_density(alpha= 0.4) + 
  xlab("Satisfaction") +
  ylab("Density") +
  ggtitle("Difference of densities") 
p
#------------------------------------------------------  
#highcharter:
results %>% group_by(Satisfied) %>% 
  summarise(sum_f = sum(Mean_student ,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum_f/n) %>% 
  select(Satisfied,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x = as.integer(Satisfied),  y =  Average ),type = "column")
q


