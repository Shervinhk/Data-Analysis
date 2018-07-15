#HW3-Q5:

library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
library(devtools)

data("spain")


spain -> PremierLeague
#rbind two copies of the orignal df, simply reversing home/away team for each match
rbind(
  PremierLeague %>%
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>%
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>% mutate(GD = GF-GA) ->check1
check1 %>% 
  group_by(Season, team) %>% 
  summarize(GP = n(),
            goalsF = sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
  ) %>% 
  mutate(score = W*3 + D) %>%
  mutate(rank = rank(-score) %>% as.integer()) %>%
  arrange(rank) -> ltable 
  ltable %>%  arrange(Season , -score) %>% group_by(Season) %>% 
  summarise( champion = team[which.max(score)] , S = max(score)) %>% filter(S == max(S)) -> H
  H
  
  q = hchart( H, type = "column" , hcaes(x = champion , y = Season))
  q
  

  