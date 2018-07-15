#HW3-q4:

library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
data("spain")

spain -> PremierLeague
#rbind two copies of the orignal df, simply reversing home/away team for each match
rbind(
  PremierLeague %>%
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>%
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>% mutate(GD = GF-GA)  -> ltable
  ltable %>% filter( Season == 2000:2011, team %in% c("Real Madrid" , "FC Barcelona" , "Valencia CF","Sevilla FC"), !opp%in%c("Real Madrid" , "FC Barcelona" , "Valencia CF","Sevilla FC") ) %>% 
  mutate(L = (GD<0)) %>% group_by(team,opp) %>% filter(n()>4) %>% 
  summarise(Loss = sum(L)/length(L)) %>% group_by(team) %>%  
  summarise(Black =opp[which.max(Loss)] , Losss = Loss[which.max(Loss)]) ->check2
  View(check2)
  
  
  
  
