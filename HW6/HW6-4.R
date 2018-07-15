# HW6 -4: 
# We will use the 10 most correlated variables:
library(ggplot2)
library(gridExtra)
library(GGally)

#-----------------------------------------
# from question 1 we have the 10 most correlated variables 
# to SalePrice
house = read.csv("/Users/shervin/Downloads/house/train.csv")
fit <- lm(SalePrice ~ OverallQual +
           GrLivArea +
           GarageCars +
           GarageArea +
           TotalBsmtSF +
           X1stFlrSF +
           FullBath +
           TotRmsAbvGrd +
           YearBuilt +
           YearRemodAdd, data = house)
summary(fit)

#-----------------------------------------
# Predicting using the fit model:
prediction <- predict(fit, newdata = house)

#-----------------------------------------
# Ploting the real vs predicted price:
plot(prediction, house$SalePrice, xlab = "Predicted Price", ylab = "Real Price",main = "Real vs Predicted Price")
abline(a=0, b=1)



#-----------------------------------------
# Q5:
# Finding the Rsqured:
RS <- summary(fit)$r.squared
print(RS)

#f statistic
#-----------------------------------------




