# HW2 - Q6:
# 
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

# We will first group by device and arrange it by thickness/(pixel size of lc)
mobile %>% group_by(device) %>% summarise(thick = mean(dim_thickness/(px_row*px_col),na.rm=TRUE)) %>% arrange(thick) -> thick.mobile
#---------------------------------------------

# Removing the NANs:
thick.mobile = na.omit(thick.mobile)
#---------------------------------------------

# Finding the first 10 most "Gooshkoob" devices:
n = nrow(thick.mobile)
Thickest = thick.mobile[n:(n-9),]
View(Thickest)
#---------------------------------------------

# If we want to plot the first ten most "Gooshkoob" devices we have:
p = ggplot(data = Thickest  , aes(x = Thickest$device , y = Thickest$thick))
p + geom_point()

#---------------------------------------------
# If we want all the "Gooshkoob"ness of all the devices:
w = ggplot(data = thick.mobile  , aes(x = thick.mobile$device , y = thick.mobile$thick))
w + geom_point()


