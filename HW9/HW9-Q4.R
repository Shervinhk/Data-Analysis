#HW9
# Question 4:

data_apple =read.csv("/Users/shervin/Downloads/class_data 2/stock_dfs/AAPL.csv") %>%
              select(Date,Open)
#----------------------------------
# we will bind till the 10th day:
nrow = nrow(data_apple)
date = data_apple[-(1:10),1]
day_first = data_apple[-(1:10),2]
day_second = data_apple[c(-(1:9),-nrow),2]
day_third = data_apple[c(-(1:8),-((nrow-1):nrow)),2]
day_fourth = data_apple[c(-(1:7),-((nrow-2):nrow)),2]
day_fifth = data_apple[c(-(1:6),-((nrow-3):nrow)),2]
day_sixth = data_apple[c(-(1:5),-((nrow-4):nrow)),2]
day_seventh = data_apple[c(-(1:4),-((nrow-5):nrow)),2]
day_eight= data_apple[c(-(1:3),-((nrow-6):nrow)),2]
day_nine = data_apple[c(-(1:2),-((nrow-7):nrow)),2]
day_tenth = data_apple[c(-(1:1),-((nrow-8):nrow)),2]

days =as.data.frame( cbind(date,day_first,day_second,
             day_third,day_fourth,day_fifth,
             day_sixth,day_seventh,day_eight,
             day_nine,day_tenth))
#----------------------------------
#Now we will use lm to see which is the best:
one_day = lm(day_first ~ day_second, data = days )
summary(one_day)

two_day = lm(day_first ~ day_second+day_third, data = days )
summary(two_day)

three_day = lm(day_first ~ day_second+day_third+day_fourth, data = days )
summary(three_day)

fourth_day = lm(day_first ~ day_second+day_third+day_fourth+day_fifth, data = days )
summary(fourth_day)

fifth_day = lm(day_first~ day_second+day_third+day_fourth+day_fifth+day_sixth, data = days )
summary(fifth_day)

sixth_day = lm(day_first~ day_second+day_third+day_fourth+day_fifth+day_sixth+day_seventh, data = days )
summary(sixth_day)


seventh_day = lm(day_first~ day_second+day_third+day_fourth+day_fifth+day_sixth+day_seventh+day_eight, data = days )
summary(seventh_day)

eight_day = lm(day_first~ day_second+day_third+day_fourth+day_fifth+day_sixth+day_seventh+day_eight+day_nine, data = days )
summary(eight_day)

nine_day = lm(day_first~ day_second+day_third+day_fourth+day_fifth+day_sixth+day_seventh+day_eight+day_nine+day_tenth, data = days )
summary(nine_day)

mse <- function(sm) 
  mean(sm$residuals^2)

mse(nine_day)
mse(eight_day)
mse(seventh_day)
mse(sixth_day)
mse(fifth_day)
mse(fourth_day)
mse(three_day)
mse(two_day)
mse(one_day)

#As we can see from the values of the summary The best day is using 8 days before with lower Fstatistic and MSE
#----------------------------------

