# HW2 - Q5:
library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

# First we define PPI =: (Size of pixel)/(Size of display):

PPI = (mobile$px_row^2 + mobile$px_col^2)^(1/2)/mobile$display_size
#---------------------------------------------
# Histogram of PPI:
hist(PPI)

#---------------------------------------------
# Finding Average PPI per Year:
mobile %>% group_by(year) %>% summarise(avePPI = mean((px_row^2+px_col^2)^(1/2)/display_size,na.rm = TRUE)) -> PPI.average

PPI.plot = ggplot(data = PPI.average , aes(x=year, y= PPI.average$avePPI))

PPI.plot + geom_point()+ ylab("Average PPI")+ggtitle("Average PPI per Year")
#---------------------------------------------
# Device with Maximum PPI:

PPI.max = mobile$device[which.max(PPI)]
PPI.max 
#---------------------------------------------
