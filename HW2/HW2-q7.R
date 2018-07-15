# HW2 - Q7:
# 
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")
# Finding the density:

density = mobile$weight/(mobile$dim_length*10^-1*mobile$dim_breadth*10^-1*mobile$dim_thickness*10^-1)
#---------------------------------------------


# Finding dose whuch are dense:
Dense = mobile$device[ which (density>1) ]
View(Dense)
#---------------------------------------------

# Plotting the dense one:
p = ggplot( data = mobile, aes(x= device,y = mobile$weight/(mobile$dim_length*mobile$dim_breadth*mobile$dim_thickness)))

p + geom_point() + geom_hline(yintercept = 1)+ xlab("device name")+ylab("density")+ggtitle("density of devices")

#---------------------------------------------

