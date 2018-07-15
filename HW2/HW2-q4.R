library(dplyr)
mobile = read.csv("/Users/shervin/Downloads/mobile_data.csv")

# Finding the ones made in 2017:
index = which(mobile$year==2017)
#---------------------------------------------
# Ploting: 
boxplot(dim_thickness ~ audio_jack, data = mobile[index,], main = "Thickness vs Audio Jack",xlab="Audio Jack",ylab="Thickness")
#---------------------------------------------
