#----------------
#We will first load the necessary libraries:
library(ggplot2)
library(corrplot)
#----------------
#HW 07:
#Question 2:

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
#Removing redundants and turning factors to integers:
murder_suicide = murder_suicide[,not.redundant]
murder_suicide_int = as.data.frame(lapply(murder_suicide,as.numeric))

murder_suicide_int %>% 
  mutate(Suicide = (MannerOfDeath==2) ) ->murder_suicide_int

murder_suicide_int$Suicide <- 1*murder_suicide_int$Suicide
#----------------
#Finding P-values:

#  Age - Manner of Death

kruskal.test( Suicide ~ Age, 
              data = murder_suicide_int)


#  Sex - Manner of Death
kruskal.test( Suicide ~ Sex, 
              data = murder_suicide_int)


#  Education - Manner of Death
kruskal.test( Suicide ~ Education2003Revision,
              data = murder_suicide_int)


#  Race - Manner of Death
kruskal.test( Suicide ~ Race, 
              data = murder_suicide_int)


#  Disposition - Manner of Death MethodOfDisposition
kruskal.test( Suicide ~ MethodOfDisposition,
              data = murder_suicide_int)

#----------------


