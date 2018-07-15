#HW4-q4:
library(highcharter)
library(ggplot2)
library(dplyr)
#------------------------------------------------------
#reading data:
timss_school = readRDS("/Users/shervin/Downloads/Tims_Data/bcg.rds")

timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")


#------------------------------------------------------
#finding questions and scores:
timss_student %>% 
  select(c("idschool","bsmmat01","bsmmat02","bsmmat03","bsmmat04","bsmmat05")) %>% 
  mutate(Score = rowSums(.[2:6])/5) %>% 
  group_by(idschool) %>% 
  summarise(Total_Score_school = sum(Score,na.rm = TRUE), n = n()) %>% 
  mutate(Mean_score_per_school= Total_Score_school/n) %>% 
  select(idschool,Mean_score_per_school) -> scores_discipline


timss_school %>% 
  select(idschool,bcdgdas) -> discipline
#------------------------------------------------------
#bidning:
 Q = cbind(scores_discipline[discipline$idschool,],discipline)
 Q <- Q[,-1]
 Q <- Q[,-2]
 Q <- na.omit(Q)
 Q <- as.data.frame(Q)
 #------------------------------------------------------
 #Making AOV test:
 h = aov(Mean_score_per_school ~ as.factor(bcdgdas), data = Q)
 h
 summary.aov(h)
 #------------------------------------------------------
#ggplot:
p = ggplot(data = Q, aes(bcdgdas, Mean_score_per_school, fill = bcdgdas)) +
   geom_boxplot() + geom_jitter(width = 0.2) +
   xlab("degree of discipline") +
   ylab("mean score per school") +
   ggtitle("degree of discipline vs mean score per school") 
 p
 #------------------------------------------------------
 #highcharter:
 Q %>% group_by(bcdgdas) %>% 
   summarise(sum = sum(Mean_score_per_school,na.rm = TRUE), n = n()) %>% 
   mutate(Average = sum/n) %>% 
   select(bcdgdas,Average) %>% 
   na.omit()-> sums
 
 sums <- as.data.frame(sums)
 
 q = hchart(sums, hcaes(x= as.integer(bcdgdas) ,y= Average), type="column", name = "scores") 
 q
 
 
 
