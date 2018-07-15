#HW5-q5:
mydata = read.csv("/Users/shervin/Downloads/hw_05/data/tv.csv")

friedman.test(mydata,c("March"	,"April"	,"May"	,"Jun"))


Sample<-matrix(c(mydata$April,mydata$March,mydata$May,mydata$Jun),ncol=4)

friedman.test(Sample)