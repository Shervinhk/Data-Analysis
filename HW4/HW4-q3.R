#HW4-q3:
library(highcharter)
library(ggplot2)
library(dplyr)
#------------------------------------------------------
#reading data:
timss_student_background = readRDS("/Users/shervin/Downloads/Tims_Data/bsg.rds")
timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")


timss_student_background %>%  
      select(bsbg05 ) ->student_possesion
#------------------------------------------------------
#selecting questions and scores:
timss_student %>% 
  select(c("idstud","bsmmat01","bsmmat02","bsmmat03","bsmmat04","bsmmat05")) %>% 
  mutate(Score = rowSums(.[2:6])/5) %>% 
  select(Score) -> scores

#------------------------------------------------------
#binding and AOV:(this is more correct)
score_posession <- cbind(scores,student_possesion)
h = aov(Score ~ bsbg05, data = score_posession)
h
summary.aov(h)
score_posession <- score_posession %>% na.omit() %>% as.data.frame()


#------------------------------------------------------
# if we look at as a two group with ttest:
score_posession %>%
  mutate(Good_Situation = (bsbg05 == 1 | bsbg05 == 2 | bsbg05 == 3) ) %>% 
  select(Good_Situation,Score) ->score_posession
hypo = t.test(Score~Good_Situation, data = score_posession )
hypo

#------------------------------------------------------
#ggplot:
p = ggplot(data = score_posession, aes(x=Score, fill= Good_Situation))+
  geom_density(alpha= 0.4) + 
  xlab("Posession") +
  ylab("Density") +
  ggtitle("Difference of densities") 
p



#------------------------------------------------------
#highcharter:

score_posession %>% group_by(Good_Situation) %>% 
  summarise(sum = sum(Score,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(Good_Situation,Average) %>% 
  na.omit()-> sums

sums <- as.data.frame(sums)

q = hchart(sums, hcaes(x= as.integer(Good_Situation) ,y= Average), type="column", name = "scores") 
q

