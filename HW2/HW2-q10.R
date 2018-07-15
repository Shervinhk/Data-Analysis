# HW2 - Q10:
# 
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

mobile %>% group_by(year) %>% summarise(mean.weight = mean(weight, na.rm =T)) -> Weight

p = ggplot( data = Weight, aes(x = year , y = mean.weight))
p + geom_point()+ xlab("year") +ylab("Weight")


mobile %>% group_by(year) %>% summarise(mean.cam = mean(cam_px,na.rm = T))-> Cam

p = ggplot( data = Cam, aes(x = year , y = mean.cam))
p + geom_point()+ xlab("year") +ylab("Camera Quality")



mobile %>% group_by(weight) %>% summarise(mean.battery = mean(battery_mah,na.rm = T))-> Battery

p = ggplot( data = Battery, aes(x = weight , y = mean.battery))
p + geom_point()+ xlab("Weight") +ylab("Battery Life")



