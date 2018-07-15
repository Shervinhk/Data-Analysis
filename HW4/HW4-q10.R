#HW4-q10:
library(ggplot2)
library(highcharter)
library(dplyr)
#------------------------------------------------------
#reading:
tims_country = readRDS("/Users/shervin/Downloads/Tims_Data/bsr.rds")
timss_question = readRDS("/Users/shervin/Downloads/Tims_Data/bsg.rds")
#------------------------------------------------------
#geting the questions:

timss_question_reapp = timss_question[,c(1,5,350,351,352,353,354,349,348,347,346,345)]
timss_question_reapp_iran = timss_question_reapp[timss_question_reapp$idcntry ==364,]
N = ncol(timss_question_reapp_iran)
timss_question_reapp_iran %>%  select(c(3:N)) -> questions
Nq = ncol(questions)
#------------------------------------------------------
#applying and reassoning questions:
questions %>%
  replace(is.na(.), 0) %>%
  mutate(Reasoning_score = rowSums(.[1:5] ), Applying_score = rowSums(.[6:Nq] ) )  -> scores
scores %>% select(Reasoning_score) -> reasoning_scores
scores %>% select(Applying_score) -> applying_scores
#------------------------------------------------------
#binding the results:
reasoning_scores <- cbind(Q ="Reasoning", Score = reasoning_scores/5)
names(reasoning_scores) <- c("Question_Type","Score")

applying_scores  <- cbind(Q ="Applying", Score = applying_scores/5)
names(applying_scores) <- c("Question_Type","Score")
#------------------------------------------------------
final_scores <- rbind(applying_scores,reasoning_scores)
#------------------------------------------------------
#we have two group so we will use t.test:
hypo = t.test(Score~Question_Type , data =final_scores )
hypo
#------------------------------------------------------
#ggplot:
p = ggplot(data = final_scores, aes(x=Score, fill= Question_Type))+
    geom_density(alpha= 0.4) + 
    xlab("Scores") +
    ylab("Density") +
    ggtitle("Difference of densities") 
p


final_scores %>% group_by(Question_Type) %>% 
  summarise(sum = sum(Score,na.rm = TRUE), n = n()) %>% 
  mutate(Average = sum/n) %>% 
  select(Question_Type,Average) %>% 
  na.omit() -> sums

#highcharter

q = hchart(sums, hcaes(x=Question_Type,y=Average), type="column") 
q
