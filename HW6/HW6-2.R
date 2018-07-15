#HW6: 2
#-----------------------------------------
# Libraries:
library(ggplot2)
library(gridExtra)
library(GGally)

#-----------------------------------------
# Reading our data and assuming levels for our factors:
house = read.csv("/Users/shervin/Downloads/house/train.csv")
house_value = as.data.frame(lapply(house,as.numeric))

#-----------------------------------------
#removing id:
house_value$Id <- NULL

#-----------------------------------------
# defining a function to plot all the plots togethers in one plot:

will_plot <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping  = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="green", color="green", ...) +
    geom_smooth(method=lm, fill="red", color="red", ...)
  p
}
z = ncol(house_value)
#ploting: (with factors)
ggpairs(house_value[,1:10], lower = list(continuous = will_plot))
# we will plot the first 10, because it is huge

#-----------------------------------------
# ploting (with/o factors):

house_no_factors = house[,sapply(house, is.numeric)]
house_no_factors$Id <- NULL
house_no_factors[is.na(house_no_factors)] <- 0

z2 = ncol(house_no_factors)
ggpairs(house_no_factors[,1:z2], lower = list(continuous = will_plot))

#-----------------------------------------




