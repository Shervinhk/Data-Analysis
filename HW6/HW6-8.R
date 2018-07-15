# HW6- Q8-9:
library(ggplot2)
library(gridExtra)
library(GGally)
#-----------------------------------------
#reading data:
house = read.csv("/Users/shervin/Downloads/house/train.csv")

#-----------------------------------------
#train and test data:
train <- sample_frac(house, 0.8, replace =FALSE) 
temp<-as.numeric(rownames(train))
test<-house[-temp,]

#-----------------------------------------
# We will use does which have ** and *** in the p-value:
fit_new_train <- lm(SalePrice ~
                OverallQual +
                GrLivArea +
                GarageCars +
                TotalBsmtSF +
                X1stFlrSF +
                YearBuilt +
                YearRemodAdd, data = train)
summary(fit_new_train)

#-----------------------------------------
#Prediction:
prediction_new_test <- predict(fit_new_train, newdata = test,se.fit = TRUE)

#-----------------------------------------
#Standard Error:
print(sum(prediction_new_test$se.fit))

#-----------------------------------------
# Q9:
exp <- function(x,k){
  res_new = 0
    for(i in 1:k)
    {
      
      res_old = res_new
      res_new = cor(house$SalePrice,(x)^i)
      if(res_old > res_new)
      {
        break
      }
    }
  return(i-1)
}

#-----------------------------------------
# We will use correlation to see which exponential is the best
# for every variable:

i1 <- exp(house$OverallQual,100) 
i1

i2 <- exp(house$GrLivArea,100)
i2

i3 <- exp(house$GarageCars,100)
i3

i4 <- exp(house$TotalBsmtSF,100)
i4

i5 <- exp(house$X1stFlrSF,100)
i5

i6 <- exp(house$YearBuilt,100)
i6

i7 <- exp(house$YearRemodAdd,100)
i7
#-----------------------------------------
# We will update our model:
fit_new_train_updated <- lm(SalePrice ~
                      OverallQual^3 +
                      GrLivArea +
                      GarageCars^2 +
                      TotalBsmtSF +
                      X1stFlrSF +
                      YearBuilt^78 +
                      YearRemodAdd^46, data = train)
summary(fit_new_train_updated)

#-----------------------------------------
prediction_new_test_updated <- predict(fit_new_train_updated, newdata = test,se.fit = TRUE)

