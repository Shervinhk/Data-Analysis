#HW3 - q6:
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
  check1 %>% mutate( loss = (GD > 0) %>% as.double() - (GD < 0) %>% as.double()) %>%
  group_by(team) %>% 
  do(data.frame(rle(.$loss)[1],rle(.$loss)[2])) %>% filter(values == -1) %>%  group_by(team) %>% 
  summarise(Loss = max(lengths)) %>% filter(Loss == max(Loss)) -> check2.loss
check1 %>% mutate( loss = (GD > 0) %>% as.double() - (GD < 0) %>% as.double()) %>%
    group_by(team) %>% 
    do(data.frame(rle(.$loss)[1],rle(.$loss)[2])) %>% filter(values == 1) %>%  group_by(team) %>% 
    summarise(Loss = max(lengths)) %>% filter(Loss == max(Loss)) -> chec
  check1 %>% mutate( loss = (GD > 0) %>% as.double() - (GD < 0) %>% as.double()) %>%
    group_by(team) %>% 
    do(data.frame(rle(.$loss)[1],rle(.$loss)[2])) %>% filter(values == 0) %>%  group_by(team) %>% 
    summarise(Loss = max(lengths)) %>% filter(Loss == max(Loss)) -> check2.tie




