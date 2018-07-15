#HW3-Q3:

library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
library(devtools)

data("spain")

spain -> PremierLeague
rbind(
  PremierLeague %>% 
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>% 
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>%  mutate( GD = GF-GA , w = GD>0 , d = GD ==0) -> check
  check %>%   mutate(score =3*w + d) %>% 
  group_by(Season,team) %>%  
  mutate(cummulative = cumsum(score) , n = 1:length(cummulative)) %>% 
  summarise( halftime = cummulative[max(n)/2] , fulltime = cummulative[max(n)]) %>% 
  group_by(Season) %>% 
  summarise( half.champion = team[which.max(halftime)], champion = team[which.max(fulltime)] ) -> check
  H = sum(check$half.champion == check$champion)/nrow(check)
  H
