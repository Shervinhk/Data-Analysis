#HW6: Q1
#-----------------------------------------
# Libraries:
library(polycor)
library(corrplot)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(gridExtra)

#-----------------------------------------
#Reading the data:
house = read.csv("/Users/shervin/Downloads/house/train.csv")


#-----------------------------------------
# Values w/o factors:
house_no_factors = house[,sapply(house, is.numeric)]
house_no_factors$Id <- NULL
house_no_factors[is.na(house_no_factors)] <- 0


#-----------------------------------------
#Value of Correlation:
res <- cor(house_no_factors)

#-----------------------------------------
#Plotting the correlations:
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#-----------------------------------------
#Value of P-Values:
res2 <- rcorr(as.matrix(house_no_factors))


#-----------------------------------------
# P-Values:
house_no_factors = res2$P
sorted =sort(house_no_factors[,"SalePrice"], decreasing = FALSE)
sorted = as.data.frame(sorted)
c = rownames(sorted)
sorted %>% 
  mutate(name = c) -> sorted

ss_pvalue <- tableGrob(sorted)
plot(ss_pvalue)

#-----------------------------------------
#Correlation:
house_price = res[,"SalePrice"]
sorted =sort(house_price, decreasing = TRUE)
sorted = as.data.frame(sorted)

c = rownames(sorted)
sorted %>% 
  mutate(name = c) -> sorted

cc = ggplot(data = sorted[1:11,], aes(x = name , y = sorted)) +
  geom_point() +
  theme_bw() 
#-----------------------------------------
#ploting the 10 highest correlated variables: w/o factors
ss <- tableGrob(sorted[1:11,])
plot(ss)




#-----------------------------------------
# Values with factors:

house_value = as.data.frame(lapply(house,as.numeric))
house_value$Id <- NULL
house_value =house_value[colSums(!is.na(house_value)) > 0]
house_value[is.na(house_value)] <- 0

#-----------------------------------------
#Value of Correlation:
res <- cor(house_value)

#-----------------------------------------
#Plotting the correlations:
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#-----------------------------------------
#Value of P-Values:
res2 <- rcorr(as.matrix(house_value))



#-----------------------------------------
# P-Values:
house_p_value = res2$P
sorted =sort(house_p_value[,"SalePrice"], decreasing = FALSE)
sorted = as.data.frame(sorted)
c = rownames(sorted)
sorted %>% 
  mutate(name = c) -> sorted

ss_pvalue <- tableGrob(sorted)
plot(ss_pvalue)

#-----------------------------------------
#Correlation:
house_price = res[,"SalePrice"]
sorted =sort(house_price, decreasing = TRUE)
sorted = as.data.frame(sorted)

c = rownames(sorted)
sorted %>% 
  mutate(name = c) -> sorted

cc = ggplot(data = sorted[1:11,], aes(x = name , y = sorted)) +
  geom_point() +
  theme_bw() 
#-----------------------------------------
#ploting the 10 highest correlated variables:
ss <- tableGrob(sorted[1:11,])
plot(ss)





#-----------------------------------------
