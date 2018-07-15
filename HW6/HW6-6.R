# HW6-6:
library(ggplot2)
library(gridExtra)
library(GGally)

#-----------------------------------------
# We will use does which have ** and *** in the p-value:
house = read.csv("/Users/shervin/Downloads/house/train.csv")
fit_new <- lm(SalePrice ~
            OverallQual +
            GrLivArea +
            GarageCars +
            TotalBsmtSF +
            X1stFlrSF +
            YearBuilt +
            YearRemodAdd, data = house)
summary(fit_new)
#-----------------------------------------
# Predicting using the new fit model:
prediction_new <- predict(fit_new, newdata = house)

#-----------------------------------------
# Ploting the real vs predicted price in the new model:
plot(prediction_new, house$SalePrice, xlab = " New Predicted Price", ylab = "Real Price",main = "Real vs New Predicted Price")
abline(a=0, b=1)

#-----------------------------------------
# Q7:

# Mean of resduals must be close to zero:
print(mean(fit_new$residuals))

#-----------------------------------------
#Normality:(top-right plot we see the fitted plot)
par(mfrow=c(2,2))
plot(fit_new)

#-----------------------------------------
# Constant Variance:
# From the top left and bottom left we see patterns that are 
# not flat, there it is no Constance Variance, therefore
# our assumption of constant variance does not apply here

#-----------------------------------------
# Independece or autocorrelation test:
par(mfrow=c(1,1))
acf(fit_new$residuals) # not corollated from the graph

