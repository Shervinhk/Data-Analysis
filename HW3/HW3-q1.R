library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
library(devtools)

data("spain")

                                       

#----------------------------------------------------------
spain %>%  filter(round=="league",tier==1) %>% select(1:8) -> PremierLeague
rbind(
  PremierLeague %>% 
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>% 
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>%  mutate( GD = GF-GA ) %>% 
  group_by( Season,team ) %>% 
  summarize( GP = n(),
            goalsF = sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0),
     ) %>% 
  mutate(score = W*3 + D ) %>% 
  mutate( rank = rank(-score) %>%  as.integer() ) %>% 
  arrange(rank, goaldif) -> ltable
  View(ltable)

  ltable %>% group_by(team) ->check 
  View(check)
  
  
# We use the old law for the ranking:
ltable %>% group_by(team) %>% summarise(number.of.championship = sum(rank==1)) %>% arrange(desc(number.of.championship)) -> table1
View(table1)
p = ggplot(data = table1, aes(x = team , y = number.of.championship )) + geom_bar(stat="identity") + 
      labs(x="Team", y="Number Of Champioships")+ggtitle("Number of Championships vs Teams")
p
q = hchart( table1, type = "column" , hcaes(x = team , y = number.of.championship))
q
#----------------------------------------------------------


