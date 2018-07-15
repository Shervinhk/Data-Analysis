# HW2 - Q9:
# 
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")
Samsung = mobile[mobile$company=="Samsung",]
Samsung %>% group_by(year) %>% summarise(check.price = max(price, na.rm = T)) -> maximum.price

View(maximum.price)

p = ggplot(data = maximum.price, aes(x = year , y = check.price))
p + geom_point() +xlab("Year")+ylab("Maximum Prices of Samsung devices") + ggtitle("Maximum prices of Samsung devices vs year")

