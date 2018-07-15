# HW3-q9:
library(engsoccerdata)

library(dplyr)
library(ggplot2)
library(highcharter)
library(devtools)

data("spain")

spain %>% filter(Season == 2012) %>% mutate(Goal.Sum = hgoal+vgoal ) -> temp
p <- ggplot(data = temp, aes(x = home, y = visitor, label = FT , color = Goal.Sum))
p + geom_label(label.padding = unit(0.1, "lines"),check_overlap = T, label.size = 0.1,  show.legend = FALSE) 

