#HW3-q8:


library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
library(devtools)

data("spain")

spain -> PremierLeague
rbind(
  PremierLeague %>% 
    select(Date,Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>% 
    select(Date,Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>%  mutate( GD = GF-GA, w = (GD > 0), d = (GD == 0) )-> check

check %>%  filter(Season == 1998) %>%   mutate( score = 3*w + d) %>% 
  group_by(team) %>%mutate(cummulative = cumsum(score), Gds = cumsum(GD) , n = 1:length(cummulative)) %>% group_by(n) %>% 
  arrange(-cummulative , -Gds) %>% group_by(n) %>% mutate(rank = 1:n()) %>% 
  select(Date,team,rank,n) %>% group_by(team) %>% arrange(n) -> check2
check2 %>% group_by(team) %>%  hchart(type="line", hcaes(x = Date, y = rank, group= team ))

