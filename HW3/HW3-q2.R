#HW3 -q2
#----------------------------------------------------------
library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
#----------------------------------------------------------
# We will first find the most boring Sessons:
data("spain")

spain -> PremierLeague
#rbind two copies of the orignal df, simply reversing home/away team for each match
rbind(
  PremierLeague %>%
    select(Season, team = home, opp = visitor, GF = hgoal, GA = vgoal),
  PremierLeague %>%
    select(Season, team = visitor, opp = home, GF = vgoal, GA = hgoal)
) %>% mutate(GD = GF-GA) %>% 
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



spain %>% group_by(Season) %>% summarise(Goals = sum(hgoal + vgoal) )%>% arrange((Goals)) -> new.spain
View(new.spain)


Year.min.Goals = new.spain$Season[ which.min(new.spain$Goals) ]
Year.min.Goals

p = ggplot(data = new.spain[1:10,] , aes( x = Season , y = Goals )) +geom_bar(stat="identity")+labs(x="Season", y="Number of Goals ")+ggtitle("Number of Goals of most boring vs Season")
p


q = hchart( new.spain[1:10,], type = "column" , hcaes(x = Season , y = Goals))
q
#----------------------------------------------------------
# Now we will find the most boring teams:

ltable %>% group_by(team) %>% summarise( team.goals = sum(goalsF)) %>% arrange(team.goals) -> team.total.goals
View(team.total.goals)

p = ggplot(data = team.total.goals[1:10,] , aes( x = team , y = team.goals )) +geom_bar(stat="identity")+xlab("Team names") +ylab("Number of Goals") + ggtitle("Number of goals of the most boring teams") 
p
q = hchart( team.total.goals[1:10,], type = "column" , hcaes(x = team , y = team.goals))
q
#----------------------------------------------------------