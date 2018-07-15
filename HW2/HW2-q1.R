# HW2 - Q1
# First we will read our data and bring our library:
library("ggplot2")
library("dplyr")

mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

# Grpouping number of devices in every company:

mobile %>% group_by(company) %>% summarise(Count=n()) %>% arrange(desc(Count))  -> number
#---------------------------------------------
#To see the top ten:
View(number[1:20,])
#---------------------------------------------
# No Will Bar Plot our reslutls
p = ggplot(data = number[1:20,] , aes(x = company , y = Count)) 
p +geom_bar(stat ="identity")
#---------------------------------------------


number = arrange(desc(Count))
