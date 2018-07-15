# HW2 - Q3:
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

mobile %>% group_by(sim_no,LTE) %>% summarise(price = mean(price, na.rm =TRUE)) -> mean.price

# We will View the prices:
View(mean.price)
#---------------------------------------------
# For number of Sim
p = ggplot(data = mean.price, aes(x = sim_no , y =price) ) 
p + geom_bar(stat= "identity")+xlab("Number of Sim cards")+ylab("Average Price")+ ggtitle("Bar Plot for Sim Cards")
#---------------------------------------------
# For having/not having LTE:
q = ggplot(data = mean.price, aes(x = LTE , y =price) ) 
q + geom_bar(stat= "identity")+xlab("LTE")+ylab("Average Price")+ggtitle("Bar Plot for LTE")
#---------------------------------------------

# Final plot with bot of them
z = ggplot(data = mean.price, aes(x = sim_no , y =price) ) 
z + geom_bar(stat= "identity",aes(fill = LTE)) + xlab(" Number of Sim Cards ")+ylab("Average Price")+ ggtitle("Bar Plot for Sim Cards and LTE")
