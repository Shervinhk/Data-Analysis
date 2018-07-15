#HW4 - Q6:
library(ggplot2)
library(dplyr)
library(Hmisc)
library(highcharter)
#------------------------------------------------------
#Reading:
timss_student = readRDS("/Users/shervin/Downloads/Tims_Data/bsa.rds")

#------------------------------------------------------
#Finding questions:
Geom_qu = timss_student[,c(18,34,37,52,65,89,102,103,119,133,134,150,151,163,164,177,178,209,210,221,224) ]
#------------------------------------------------------
#background of students:
timss_student_id = readRDS("/Users/shervin/Downloads/Tims_Data/bsg.rds")
timss_student_id <- as.data.frame(timss_student_id) 
timss_student_id_gender <- timss_student_id$itsex

Geom_with_gender =cbind(timss_student_id_gender, Geom_qu)

gender <- Geom_with_gender$timss_student_id_gender
#------------------------------------------------------
#correct answers to every question:
q1 <- as.integer(Geom_with_gender$m042271 ==3)
q2 <- as.integer(Geom_with_gender$m062183 ==10)
q3 <- as.integer(Geom_with_gender$m062286 ==20)
q4 <- as.integer(Geom_with_gender$m052417 ==10)
q5 <- as.integer(Geom_with_gender$m062245 ==1)
q6 <- as.integer(Geom_with_gender$m042203 ==3)
q7 <- as.integer(Geom_with_gender$m052042 ==10)
q8 <- as.integer(Geom_with_gender$m052047 ==10)
q9 <- as.integer(Geom_with_gender$m042151 ==10)
q10 <- as.integer(Geom_with_gender$m062174 ==2)
q11 <- as.integer(Geom_with_gender$m062244 ==10)
q12 <- as.integer(Geom_with_gender$m052407 ==1)
q13 <- as.integer(Geom_with_gender$m052036 ==10)
q14 <- as.integer(Geom_with_gender$m062040 ==3)
q15 <- as.integer(Geom_with_gender$m062288 ==20)
q16 <- as.integer(Geom_with_gender$m052048 ==10)
q17 <- as.integer(Geom_with_gender$m052039 ==10)
q18 <- as.integer(Geom_with_gender$m052083 ==3)
q19 <- as.integer(Geom_with_gender$m052082 ==3)
q20 <- as.integer(Geom_with_gender$m062250a ==10)
q21 <- as.integer(Geom_with_gender$m052082 ==1)


gender_geom <- cbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18
      ,q19,q20,q21)
N = nrow(gender_geom)
gender_geom <- as.data.frame(gender_geom)
gender_geom_score = rowSums(gender_geom[1:N,],na.rm = TRUE)
gender_geom_final = cbind(gender , gender_geom_score)
gender_geom_final <- as.data.frame(gender_geom_final)
#------------------------------------------------------
# we have to groups so we will use t.test:
hypo = t.test(gender_geom_score~gender, data = gender_geom_final)
hypo
#------------------------------------------------------
#ggplot of means:

gender_geom_final %>% 
  group_by(gender) %>% 
  summarise(sum = sum(gender_geom_score,na.rm=TRUE),n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(gender,Average) %>% 
  na.omit() -> sums-> final_scores

ggplot(gender_geom_final,aes(x = gender_geom_score,fill = gender)) +geom_density(alpha= 0.4)


p = ggplot(data = final_scores, aes(x=gender,y=Average )) +
    geom_bar(stat = "identity") +
    xlab("Male = 2, Female = 1") +
    ylab("total number of correct answers") +
    ggtitle("total number") 
p
#------------------------------------------------------
#highcharter plot of means:
q = hchart(final_scores, hcaes(x=gender, y = Average), type = "column",name ="total scores")
q


