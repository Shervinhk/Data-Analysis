# HW2 - Q2:
library("ggplot2")
library("dplyr")
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")


mobile %>% group_by(year) %>% summarise(dim = mean(mobile$dim_breadth, na.rm =TRUE)) -> breadth

#---------------------------------------------
#For Bradth:

mobile %>% group_by(year) %>% summarise(dim = mean(dim_breadth, na.rm =TRUE)) -> breadth

p = ggplot(data = breadth, aes(x = year, y = dim))
coef = coef(lm(dim ~ year, data = breadth))
p2 = p + geom_point() + xlab("Year") + ylab("Breadth") + ggtitle("Mean Breadth vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
p2

q  = ggplot(data = mobile , aes(x= year , y = dim_breadth))
coef = coef(lm(dim_breadth ~ year, data = mobile))
q2 =q + geom_point()+ xlab("Year") + ylab("Breadth") + ggtitle("Dimension Breadth vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
q2
#---------------------------------------------
# For Length: 

mobile %>% group_by(year) %>% summarise(dim = mean(dim_length, na.rm =TRUE)) -> length

p = ggplot(data = length, aes(x = year, y = dim))
coef = coef(lm(dim ~ year, data = length))
p3 = p + geom_point() + xlab("Year") + ylab("length") + ggtitle("Mean length vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
p3

q  = ggplot(data = mobile , aes(x= year , y = dim_length))
coef = coef(lm(dim_length ~ year, data = mobile))
q3 =q + geom_point()+ xlab("Year") + ylab("length") + ggtitle("Dimension length vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
q3
#---------------------------------------------
# For Thickness:

mobile %>% group_by(year) %>% summarise(dim = mean(dim_thickness, na.rm =TRUE)) -> thickness

p = ggplot(data = thickness, aes(x = year, y = dim))
coef = coef(lm(dim ~ year, data = thickness))
p4 = p + geom_point() + xlab("Year") + ylab("thickness") + ggtitle("Mean thickness vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
p4

q  = ggplot(data = mobile , aes(x= year , y = dim_thickness))
coef = coef(lm(dim_thickness ~ year, data = mobile))
q4 =q + geom_point()+ xlab("Year") + ylab("thickness") + ggtitle("Dimension thickness vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
q4
#---------------------------------------------
# For Camera Quality:

mobile %>% group_by(year) %>% summarise(dim = mean(cam_px, na.rm =TRUE)) -> cam_px

p = ggplot(data = cam_px, aes(x = year, y = dim))
coef = coef(lm(dim ~ year, data = cam_px))
p5 = p + geom_point() + xlab("Year") + ylab("cam_px") + ggtitle("Mean cam_px vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
p5

q  = ggplot(data = mobile , aes(x= year , y = cam_px))
coef = coef(lm(cam_px ~ year, data = mobile))
q5 =q + geom_point()+ xlab("Year") + ylab("camera quality") + ggtitle(" camear quality vs Year") + geom_abline(intercept = coef[1],slope = coef[2])
q5
#---------------------------------------------
