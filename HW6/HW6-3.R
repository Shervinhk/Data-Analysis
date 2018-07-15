# HW6 -3: 
# We will use the 10 most correlated variables:
library(ggplot2)
library(gridExtra)
library(GGally)

#-----------------------------------------
# from question 1 we have the 10 most correlated variables 
# to SalePrice
house = read.csv("/Users/shervin/Downloads/house/train.csv")
fit = lm(SalePrice ~ OverallQual +
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
