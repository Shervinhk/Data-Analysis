#HW 07:
#Question 5:
library(dplyr)
library(data.table)
#----------------
# 80 percent for train and 20 percent for Test:
Train <- sample_frac(murder_suicide_levels,
                     size = 0.8, 
                     replace =FALSE)
sid<-as.numeric(rownames(Train))
Test<-murder_suicide_levels[-sid,]

#----------------
#Train our model:
new_myglm_second_train = glm(data = Train,
                Suicide ~ `Age`+
                  `ResidentStatus=2` +
                  `ResidentStatus=3` + `Sex=F` +
                  `Education2003Revision=1` +
                  `Education2003Revision=2` + 
                  `Education2003Revision=3` + 
                  `Education2003Revision=6` + 
                  `Education2003Revision=7` +
                  `Autopsy=N`+
                  `PlaceOfDeathAndDecedentsStatus=1` +
                  `PlaceOfDeathAndDecedentsStatus=2` +
                  `PlaceOfDeathAndDecedentsStatus=3` +
                  `PlaceOfDeathAndDecedentsStatus=4` +
                  `PlaceOfDeathAndDecedentsStatus=5` +
                  `PlaceOfDeathAndDecedentsStatus=6` +
                  `MaritalStatus=M` + `InjuryAtWork=N` +
                  `MonthOfDeath=4` + `MonthOfDeath=2`+
                  `DayOfWeekOfDeath=5`+ `DayOfWeekOfDeath=4` +`DayOfWeekOfDeath=3`+ `DayOfWeekOfDeath=2`+
                  `MethodOfDisposition=C` + `Autopsy=N` +
                  `PlaceOfInjury=1` +
                  `PlaceOfInjury=4` + 
                  `PlaceOfInjury=6` + `Race=2`  + `NumberOfEntityAxisConditions`+
                  `NumberOfRecordAxisConditions`,
                  family = binomial(link = 'logit'))

#----------------
#Predicting:
new_predict = predict(new_myglm_second_train,Test,type="response" )
#----------------
#Combining:
Test %>% 
  mutate(Predicted =new_predict ) -> Test
#----------------
#Using the function:
ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
{   
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in 
  # a two by two confusion matrix   
  predict <- data[[predict]]
  actual  <- relevel( as.factor(data[[actual]]), "1")
  result <- data.table( actual = actual, predict = predict )
  
  # caculating each pred falls into which category for the confusion matrix
  result[ , type := ifelse( predict >= cutoff & actual == 1, "TP",
                            ifelse( predict >= cutoff & actual == 0, "FP", 
                                    ifelse( predict <  cutoff & actual == 1, "FN", "TN" ) ) ) %>% as.factor() ]
  
  # jittering : can spread the points along the x axis 
  plot <- ggplot( result, aes( actual, predict, color = type ) ) + 
    geom_violin( fill = "white", color = NA ) +
    geom_jitter( shape = 1 , alpha = 0.2) + 
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
    scale_y_continuous( limits = c( 0, 1 ) ) + 
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend 
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )
  
  return( list( data = result, plot = plot ) )
}
#----------------
#CM:
Test <- as.data.frame(Test)

cm_info = ConfusionMatrixInfo( data = Test, predict = "Predicted", 
                               actual = "Suicide", cutoff = .5 )

cm_info$plot
#----------------
temp = cm_info$data
#We will use the given definitions to find the values:
TP = sum(temp$type == "TP")
TN = sum(temp$type == "TN")
FP = sum(temp$type == "FP")
FN = sum(temp$type == "FN")
N = TN+FN 
n = nrow(Test)
ACC = (TP+TN)/n

FPR = FP/N
TPR = TP/N
P = TP+FP

TP
TN
FP
FN
ACC
FPR
TPR
P
N


#----------------
