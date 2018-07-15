#HW3-q7:


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
) %>%  mutate( GD = GF-GA, w = (GD > 0), d = (GD == 0) )-> check
  
    check %>% mutate( score = 3*w + d ) %>% group_by(Season,team) %>%
    mutate(cummulative = cumsum(score) , n = 1:length(cummulative)) %>%
    group_by(Season) %>% mutate(numbergames = max(n)) %>% 
    group_by(Season,n,numbergames) %>% group_by(Season,n,numbergames) %>%
    summarise( points1  = cummulative[2] , points2 = cummulative[1] , team = team[which.min(cummulative)]) %>% 
    mutate(g = 3*(numbergames - n) - ( points1 - points2)) %>% filter(g < 0) %>% 
    group_by(Season) %>% top_n(1, g) %>% mutate(weeks = numbergames - n) %>% filter(weeks == (max(.$weeks)-2)) %>% 
    select(Season , team , weeks) -> check3
 
