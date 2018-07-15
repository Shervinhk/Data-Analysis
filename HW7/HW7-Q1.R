#----------------
#We will first load the necessary libraries:
library(ggplot2)
library(corrplot)
library(onehot)
#----------------
#HW 07:
#Question1:

#Readcing data:
murder_suicide = read.csv("/Users/shervin/Desktop/Data Analysis/HW7/data 2/murder_suicide.csv")


#----------------
# Columns which are not redundant:
not.redundant = c("Age",
                  "ResidentStatus",
                  "Education2003Revision",
                  "MonthOfDeath",
                  "Sex",
                  "AgeType",
                  "PlaceOfDeathAndDecedentsStatus",
                  "MaritalStatus",
                  "DayOfWeekOfDeath",
                  "InjuryAtWork",
                  "MannerOfDeath",
                  "MethodOfDisposition",
                  "Autopsy",
                  "ActivityCode",
                  "PlaceOfInjury",
                  "Icd10Code",
                  "NumberOfEntityAxisConditions",
                  "NumberOfRecordAxisConditions",
                  "Race"
)

#----------------
#Removing redundants and turning factors to integers 
#We will give every factor a binary with the onehot package to see their influence:
murder_suicide = murder_suicide[,not.redundant]
murder_suicide %>% 
  filter(Education2003Revision!=9,
         PlaceOfInjury!=99,
         PlaceOfDeathAndDecedentsStatus!=9,
         MaritalStatus!="U",
         InjuryAtWork!="U",
         DayOfWeekOfDeath!=9,
         Autopsy!="U", ActivityCode!=99,
         PlaceOfInjury!=9) -> murder_suicide
#----------------
#Changing to factors thos that are sort of factors:
murder_suicide$Sex <- factor(murder_suicide$Sex)
murder_suicide$MaritalStatus <- factor(murder_suicide$MaritalStatus)
murder_suicide$InjuryAtWork <- factor(murder_suicide$InjuryAtWork)
murder_suicide$MethodOfDisposition <- factor(murder_suicide$MethodOfDisposition)
murder_suicide$Autopsy <- factor(murder_suicide$Autopsy)
murder_suicide$PlaceOfInjury <- factor(murder_suicide$PlaceOfInjury)
murder_suicide$Icd10Code <-factor(murder_suicide$Icd10Code)
murder_suicide$ResidentStatus <- factor(murder_suicide$ResidentStatus)
murder_suicide$Education2003Revision <- factor(murder_suicide$Education2003Revision)
murder_suicide$PlaceOfDeathAndDecedentsStatus <- factor(murder_suicide$PlaceOfDeathAndDecedentsStatus)
murder_suicide$Race <-factor(murder_suicide$Race)
murder_suicide$MonthOfDeath <-factor(murder_suicide$MonthOfDeath)
murder_suicide$DayOfWeekOfDeath <-factor(murder_suicide$DayOfWeekOfDeath)
#----------------
#Using onehot to make the binaries:
temp = onehot(murder_suicide , stringsAsFactors = TRUE, max_levels = 10000)
murder_suicide_levels = as.data.frame(predict(temp,murder_suicide))
#----------------
#correlation Matrix:
correlation_ms = cor(murder_suicide_levels)

#----------------
#Plotting Corrleation Matrix:
corrplot(correlation_ms, method="circle")
#Because it is so big we will first show the first 15:
corrplot(correlation_ms[1:15,1:15], method="color")
#All others:
corrplot(correlation_ms, method="color",tl.cex = 0.4)

#----------------
#plotting the scatter plot:
plot(murder_suicide_levels[1:1000,1:10])

#----------------


