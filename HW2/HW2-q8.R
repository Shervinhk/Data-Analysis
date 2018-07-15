# HW2 - Q8:
# 
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")


p = ggplot( data = mobile, aes(x= battery_mah , y=weight )) 
p + geom_point()


cor(mobile$battery_mah,mobile$weight, use = "pairwise.complete.obs")

