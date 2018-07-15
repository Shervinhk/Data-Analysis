#HW 07:
#Question 3:
library(boot)
library(dplyr)
#Readcing data:

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


murder_suicide_int$Sex <- factor(murder_suicide_int$Sex)
murder_suicide_int$MaritalStatus <- factor(murder_suicide_int$MaritalStatus)
murder_suicide_int$InjuryAtWork <- factor(murder_suicide_int$InjuryAtWork)
murder_suicide_int$MethodOfDisposition <- factor(murder_suicide_int$MethodOfDisposition)
murder_suicide_int$Autopsy <- factor(murder_suicide_int$Autopsy)
murder_suicide_int$PlaceOfInjury <- factor(murder_suicide_int$PlaceOfInjury)
murder_suicide_int$Icd10Code <-factor(murder_suicide_int$Icd10Code)
murder_suicide_int$ResidentStatus <- factor(murder_suicide_int$ResidentStatus)
murder_suicide_int$Education2003Revision <- factor(murder_suicide_int$Education2003Revision)
murder_suicide_int$PlaceOfDeathAndDecedentsStatus <- factor(murder_suicide_int$PlaceOfDeathAndDecedentsStatus)
murder_suicide_int$Race <-factor(murder_suicide_int$Race)
murder_suicide_int$MonthOfDeath <-factor(murder_suicide_int$MonthOfDeath)
murder_suicide_int <-factor(murder_suicide_int$Race)




#----------------
#Taking the Suicides:
murder_suicide_levels %>% 
  mutate(Suicide = (MannerOfDeath==2) ) ->murder_suicide_levels

murder_suicide_levels$Suicide <- 1*murder_suicide_levels$Suicide


myglm=glm(data= murder_suicide_levels,
    formula =  Suicide~`Age`+`ResidentStatus=1`+
              `ResidentStatus=2`+`ResidentStatus=3`+
              `ResidentStatus=4`+`Education2003Revision=0`+
              `Education2003Revision=1`+`Education2003Revision=2`+
              `Education2003Revision=3`+`Education2003Revision=4`+
              `Education2003Revision=5`+`Education2003Revision=6`+
              `Education2003Revision=7`+`Education2003Revision=8`+
              `Sex=F`+
              `Sex=M`+`AgeType`+
              `PlaceOfDeathAndDecedentsStatus=1`+`PlaceOfDeathAndDecedentsStatus=2`+
              `PlaceOfDeathAndDecedentsStatus=3`+`PlaceOfDeathAndDecedentsStatus=4`+
              `PlaceOfDeathAndDecedentsStatus=5`+`PlaceOfDeathAndDecedentsStatus=6`+
              `PlaceOfDeathAndDecedentsStatus=7`+`MaritalStatus=D`+
              `MaritalStatus=M`+`MaritalStatus=S`+
              `MaritalStatus=W`+
              `InjuryAtWork=N`+`InjuryAtWork=Y`+
              `MethodOfDisposition=B`+
              `DayOfWeekOfDeath=1`+`DayOfWeekOfDeath=2`+
              `DayOfWeekOfDeath=3`+`DayOfWeekOfDeath=4`+`DayOfWeekOfDeath=5`+
              `MonthOfDeath=1` +
              `MonthOfDeath=2`+`MonthOfDeath=3`+`MonthOfDeath=4`+
              `MonthOfDeath=5`+`MonthOfDeath=6`+`MonthOfDeath=7`+
              `MonthOfDeath=8`+`MonthOfDeath=9`+`MonthOfDeath=10`+
              `MonthOfDeath=11`+`MonthOfDeath=12`+
              `MethodOfDisposition=C`+`MethodOfDisposition=D`+
              `MethodOfDisposition=E`+`MethodOfDisposition=O`+
              `MethodOfDisposition=R`+`MethodOfDisposition=U`+
              `Autopsy=N`+`Autopsy=Y`+
              `ActivityCode`+`PlaceOfInjury=0`+
              `PlaceOfInjury=1`+`PlaceOfInjury=2`+
              `PlaceOfInjury=3`+`PlaceOfInjury=4`+
              `PlaceOfInjury=5`+`PlaceOfInjury=6`+
              `PlaceOfInjury=7`+`PlaceOfInjury=8`+
              `Icd10Code=C349`+`Icd10Code=F329`+
              `Icd10Code=K767`+`Icd10Code=V030`+
              `Icd10Code=V476`+`Icd10Code=W10`+
              `Icd10Code=W13`+`Icd10Code=W17`+
              `Icd10Code=W18`+`Icd10Code=W20`+
              `Icd10Code=W29`+`Icd10Code=W32`+
              `Icd10Code=W50`+`Icd10Code=W65`+
              `Icd10Code=W67`+`Icd10Code=W69`+
              `Icd10Code=W73`+`Icd10Code=W74`+
              `Icd10Code=W78`+`Icd10Code=W79`+
              `Icd10Code=W80`+`Icd10Code=W84`+
              `Icd10Code=X00`+`Icd10Code=X09`+
              `Icd10Code=X16`+`Icd10Code=X41`+
              `Icd10Code=X42`+`Icd10Code=X44`+
              `Icd10Code=X47`+`Icd10Code=X599`+
              `Icd10Code=X60`+`Icd10Code=X61`+
              `Icd10Code=X62`+`Icd10Code=X63`+
              `Icd10Code=X64`+`Icd10Code=X65`+
              `Icd10Code=X66`+`Icd10Code=X67`+
              `Icd10Code=X68`+`Icd10Code=X69`+
              `Icd10Code=X70`+`Icd10Code=X71`+
              `Icd10Code=X72`+`Icd10Code=X73`+
              `Icd10Code=X74`+`Icd10Code=X75`+
              `Icd10Code=X76`+`Icd10Code=X78`+
              `Icd10Code=X79`+`Icd10Code=X80`+
              `Icd10Code=X81`+`Icd10Code=X82`+
              `Icd10Code=X83`+`Icd10Code=X84`+
              `Icd10Code=X85`+`Icd10Code=X88`+
              `Icd10Code=X89`+`Icd10Code=X90`+
              `Icd10Code=X91`+`Icd10Code=X92`+
              `Icd10Code=X93`+`Icd10Code=X94`+
              `Icd10Code=X95`+`Icd10Code=X96`+
              `Icd10Code=X97`+`Icd10Code=X98`+
              `Icd10Code=X99`+`Icd10Code=Y00`+
              `Icd10Code=Y01`+`Icd10Code=Y02`+
              `Icd10Code=Y03`+`Icd10Code=Y04`+
              `Icd10Code=Y05`+`Icd10Code=Y08`+
              `Icd10Code=Y09`+`NumberOfEntityAxisConditions`+
              `NumberOfRecordAxisConditions`+`Race=1`+
              `Race=2`+`Race=3`+
              `Race=4`+`Race=5`+
              `Race=6`+`Race=7`+
              `Race=18`+`Race=28`+
              `Race=38`+`Race=48`+
              `Race=58`+`Race=68`+
              `Race=78`,family=binomial(link='logit'))

summary(myglm)
#----------------
#We will ignore some of the vairables:
new_myglm_first = glm(data =murder_suicide_levels,
                 formula = Suicide ~ `Age`+
                `ResidentStatus=1` + `ResidentStatus=2` +
                `ResidentStatus=3` + `Sex=F` + `Sex=M`+ `Education2003Revision=1` +
                `Education2003Revision=2` + `Education2003Revision=3` + `Education2003Revision=4` + 
                `Education2003Revision=5` + `Education2003Revision=6` + `Education2003Revision=7` +
                 `Autopsy=N`+`Autopsy=Y`+
                  `DayOfWeekOfDeath=1`+`DayOfWeekOfDeath=2`+
                  `DayOfWeekOfDeath=3`+`DayOfWeekOfDeath=4`+`DayOfWeekOfDeath=5`+
                  `MonthOfDeath=1` +
                  `MonthOfDeath=2`+`MonthOfDeath=3`+`MonthOfDeath=4`+
                  `MonthOfDeath=5`+`MonthOfDeath=6`+`MonthOfDeath=7`+
                  `MonthOfDeath=8`+`MonthOfDeath=9`+`MonthOfDeath=10`+
                  `MonthOfDeath=11`+`MonthOfDeath=12`+
                `PlaceOfDeathAndDecedentsStatus=1` +
                `PlaceOfDeathAndDecedentsStatus=2` +
                `PlaceOfDeathAndDecedentsStatus=3` +
                `PlaceOfDeathAndDecedentsStatus=4` +
                `PlaceOfDeathAndDecedentsStatus=5` +
                `PlaceOfDeathAndDecedentsStatus=6` +
                `MaritalStatus=D` + `MaritalStatus=M` +
                `MaritalStatus=S` +  `InjuryAtWork=N` +
                `MethodOfDisposition=B` +
                `MethodOfDisposition=C` + `Autopsy=N` +
                `PlaceOfInjury=0` + `PlaceOfInjury=1` +
                `PlaceOfInjury=2` + `PlaceOfInjury=3` +
                `PlaceOfInjury=4` + `PlaceOfInjury=5` +
                `PlaceOfInjury=6` + `PlaceOfInjury=7` +
                `Race=1` + `Race=2` + `Race=3` + `Race=4` +
                `Race=5` + `Race=6` + `Race=7` + `Race=18` +
                `Race=28` + `Race=38` + `Race=48` + `Race=58` +
                `Race=68` + `NumberOfEntityAxisConditions`+
                `NumberOfRecordAxisConditions`,
                family = binomial(link = 'logit'))

summary(new_myglm_first)
#----------------
#Now we will take the most significants (the ones with low p values):
new_myglm_second = glm(data =murder_suicide_levels,
                      formula = Suicide ~ `Age`+
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

summary(new_myglm_second)
#----------------
