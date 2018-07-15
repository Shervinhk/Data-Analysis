library(h2o)

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

murder_suicide_int = as.data.frame(lapply(murder_suicide,as.numeric))


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


h2o.init()
h2o_murder_suicide = as.h2o(murder_suicide)
  
  
chglm = h2o.glm(y = "Suicide", 
                x=c( "Age",
                  "ResidentStatus=1" , "ResidentStatus=2" ,
                  "ResidentStatus=3" , "Sex=F" , "Sex=M", "Education2003Revision=1" ,
                  "Education2003Revision=2" , "Education2003Revision=3" , "Education2003Revision=4" , 
                  "Education2003Revision=5" , "Education2003Revision=6" , "Education2003Revision=7" ,
                  "Autopsy=N","Autopsy=Y",
                  "DayOfWeekOfDeath=1","DayOfWeekOfDeath=2",
                  "DayOfWeekOfDeath=3","DayOfWeekOfDeath=4","DayOfWeekOfDeath=5",
                  "MonthOfDeath=1" ,
                  "MonthOfDeath=2","MonthOfDeath=3","MonthOfDeath=4",
                  "MonthOfDeath=5","MonthOfDeath=6","MonthOfDeath=7",
                  "MonthOfDeath=8","MonthOfDeath=9","MonthOfDeath=10",
                  "MonthOfDeath=11","MonthOfDeath=12",
                  "PlaceOfDeathAndDecedentsStatus=1" ,
                  "PlaceOfDeathAndDecedentsStatus=2" ,
                  "PlaceOfDeathAndDecedentsStatus=3" ,
                  "PlaceOfDeathAndDecedentsStatus=4" ,
                  "PlaceOfDeathAndDecedentsStatus=5" ,
                  "PlaceOfDeathAndDecedentsStatus=6" ,
                  "MaritalStatus=D" , "MaritalStatus=M" ,
                  "MaritalStatus=S" ,  "InjuryAtWork=N" ,
                  "MethodOfDisposition=B" ,
                  "MethodOfDisposition=C" , "Autopsy=N" ,
                  "PlaceOfInjury=0" , "PlaceOfInjury=1" ,
                  "PlaceOfInjury=2" , "PlaceOfInjury=3" ,
                  "PlaceOfInjury=4" , "PlaceOfInjury=5" ,
                  "PlaceOfInjury=6" , "PlaceOfInjury=7" ,
                  "Race=1" , "Race=2" , "Race=3" , "Race=4" ,
                  "Race=5" , "Race=6" , "Race=7" , "Race=18" ,
                  "Race=28" , "Race=38" , "Race=48" , "Race=58" ,
                  "Race=68" , "NumberOfEntityAxisConditions",
                  "NumberOfRecordAxisConditions"),
                training_frame = h2o_murder_suicide, 
                family="binomial",nfolds = 5)
chglm

