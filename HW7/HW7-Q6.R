library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(tidyr)
library(e1071)
#----------------
# Our function:
AccuracyCutoffInfo <- function( train, test, predict, actual )
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .25, .75, by = .01 )
  
  
  accuracy <- lapply( cutoff, function(c)
  {
    r1= as.factor( train[[predict]] > c )
    levels(r1) = c("0","1")
    r2= as.factor( test[[predict]] > c )
    levels(r2) = c("0","1")
    # use the confusionMatrix from the caret package
    cm_train <- confusionMatrix(r1, as.factor(train[[actual]]) )
    cm_test  <- confusionMatrix(r2, as.factor(test[[actual]])  )
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  }) %>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" )
  
  return( list( data = accuracy, plot = plot ) )
}
#----------------
#Prediction for train dataset:
acc_predicted = predict(new_myglm_second , Train , type="response")
Train$Predicted = acc_predicted

#----------------
#Finding accuracy:
accuracy_new = AccuracyCutoffInfo( train = Train, 
                                   test = Test, 
                                   predict = "Predicted",
                                   actual = "Suicide" )
#Plotting:
accuracy_new$plot

#----------------
#Cuttoff for train:
temp_data = accuracy_new$data
max_train =max(temp_data[,2])
index_train =which(temp_data[,2]==max_train)
cutt_off_train = temp_data[index_train,1]
cutt_off_train

#Cuttoff for Test:
max_test =max(temp_data[,3])
index_test =which(temp_data[,3]==max_test)
cutt_off_test = temp_data[index_test,1]
cutt_off_test
#----------------

