# HW5-q7
mydata = read.csv("/Users/shervin/Downloads/hw_05/data/consumption.csv")


cor.test( ~ A + B, 
          data= mydata,
          method = "spearman",
          continuity = TRUE,
          conf.level = 0.95)

