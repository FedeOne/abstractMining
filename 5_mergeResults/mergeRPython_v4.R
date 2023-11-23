
library(rstudioapi)
library(tm)
library(tidytext)
library(dplyr)
library(tidyverse)
library(english)
library(stringr)
library(RMySQL)
library(arrow)
library(quanteda)

# Project Name ----
projectName <- "ironCkdLisa"

`%notin%` <- Negate(`%in%`)

# Path ----

currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("mergeRPython_v4.R")

setwd(currentPath2) 

# Load data ----

data <- readRDS(paste0("../2B_RegexInfos/treatedDB/",projectName,".rds") )

## best models seem Roberta, bert_distill

dataPythonORHR <- arrow::read_feather(paste0("../4_Python/1_ORHRLLAMABertTreated/2_outcome/outORHRLlamaBertOutcomeFinal",projectName,".feather"))

dataORHRbefore <-  readRDS(paste0("../3_forPython/2_FullDataFrameORHRRR/ORHRRR2",projectName,".rds") )

dataFreqBefore <-  readRDS(paste0("../3_forPython/3_FullDataFramePrevalence/prevFull",projectName,".rds") )

dataFreqPython <-  arrow::read_feather(paste0("../4_Python/2_prevBert/outPrevBertFinal",projectName,".feather" )) %>% 
  select(doc_id, PMID, ansFreq, ansGroup) %>% 
  mutate(PMID = as.character(PMID))

dataParticipantsPython <- arrow::read_feather(paste0("../4_Python/3_popLlamaBertTreatedDB/popBertFinal",projectName,".feather" )) %>% 
  select(PMID, contains("ans"))




# 1: clean python ORHR answers #### 
stop <- stopwords() %>% append(c( "the", "frequents?", "vs", "relationship", "definitions?", "conclusions?",  "and", "among",
                                 "significantly",  "frequently", "hazard ratio","hr","odd ratio", 
                                 "odd ratios","odds ratio", "univariate","likelihood","review","meta","overview", "documentation",
                                 "in ","even","concentration","concentrations",
                                 "included","significant","adjusted","analyses","analysis","multivariable","model","models","associated"))

stop2 <- paste0("\\b",stop , "\\b")
pasteStop <- paste(stop2,collapse = '|' )



dataPython2 <- dataPythonORHR %>% 
  select(doc_id, PMID, "ansOutcome" =  ansOutcomeRoby, "ansExposure" = ansExposureRoby ) %>% 
  mutate(ansExposure = tolower(ansExposure),
         ansExposure2 = str_remove_all(ansExposure, pasteStop),
         ansExposure2 = str_replace_all(ansExposure2,"(  )"," "),
         ansExposure3 = str_trim(str_remove_all(ansExposure2,"\\'s") ) ) %>% 
  select( doc_id, PMID, ansExposure,ansExposure2, ansExposure3,ansOutcome)


dataPython3 <- dataPython2 %>% 
  mutate(PMID = as.character(PMID)
         ) %>% 
  ##clean Ans outcome 2
  mutate(ansOutcome = tolower(ansOutcome),
         ansOutcome2 = str_remove_all(ansOutcome, pasteStop),
         ansOutcome2 = str_replace_all(ansOutcome2,"(-|  )"," "),
         ansOutcome3 = str_trim(str_remove_all(ansOutcome2,"\\'s"))  ) %>% 
  select( doc_id, PMID, "ansExposure" = ansExposure3, "ansOutcome" = ansOutcome3) %>% 
  left_join(select(data, PMID, contains("acro")), by = "PMID")
  
  
### replace acronyms

dataPython4 <-  dataPython3 %>% 
  select(doc_id, PMID, ansExposure,  acro1, acroComp1, acro2, acroComp2,  acro3, acroComp3, acro4, acroComp4,acro5, acroComp5,acro6, acroComp6,acro7, acroComp7, ansOutcome) %>% 
  mutate(expoAcro1 =str_detect(ansExposure, acro1),
         expoAcro2 =str_detect(ansExposure, acro2),
         expoAcro3 =str_detect(ansExposure, acro3),
         expoAcro4 =str_detect(ansExposure, acro4),
         expoAcro5 =str_detect(ansExposure, acro5),
         expoAcro6 =str_detect(ansExposure, acro6),
         expoAcro7 =str_detect(ansExposure, acro7) ) %>% 
  
  mutate(
    ansExposureReplace1 = ifelse(expoAcro1 == FALSE | is.na(expoAcro1), ansExposure, str_replace(ansExposure, pattern = acro1, replacement = acroComp1) ),
    ansExposureReplace2 = ifelse(expoAcro2 == FALSE | is.na(expoAcro2), ansExposureReplace1, str_replace(ansExposureReplace1, pattern = acro2, replacement = acroComp2)),
    ansExposureReplace3 = ifelse(expoAcro3 == FALSE | is.na(expoAcro3), ansExposureReplace2, str_replace(ansExposureReplace2, pattern = acro3, replacement = acroComp3)),
    ansExposureReplace4 = ifelse(expoAcro4 == FALSE | is.na(expoAcro4), ansExposureReplace3, str_replace(ansExposureReplace3, pattern = acro4, replacement = acroComp4)),
    
    ansExposureReplace5 = ifelse(expoAcro5 == FALSE | is.na(expoAcro5), ansExposureReplace4, str_replace(ansExposureReplace4, pattern = acro5, replacement = acroComp5)),
    ansExposureReplace6 = ifelse(expoAcro6 == FALSE | is.na(expoAcro6), ansExposureReplace5, str_replace(ansExposureReplace5, pattern = acro6, replacement = acroComp6)),
    ansExposureReplace7 = ifelse(expoAcro7 == FALSE | is.na(expoAcro7), ansExposureReplace6, str_replace(ansExposureReplace6, pattern = acro7, replacement = acroComp7))
  
  ) %>% 
  mutate(outAcro1 =str_detect(ansOutcome, acro1),
         outAcro2 =str_detect(ansOutcome, acro2),
         outAcro3 =str_detect(ansOutcome, acro3),
         outAcro4 =str_detect(ansOutcome, acro4),
         outAcro5 =str_detect(ansOutcome, acro5),
         outAcro6 =str_detect(ansOutcome, acro6),
         outAcro7 =str_detect(ansOutcome, acro7) ) %>% 
  
  mutate(
    ansOutReplace1 = ifelse(outAcro1 == FALSE | is.na(outAcro1), ansOutcome, str_replace(ansOutcome, pattern = acro1, replacement = acroComp1) ),
    ansOutReplace2 = ifelse(outAcro2 == FALSE | is.na(outAcro2), ansOutReplace1, str_replace(ansOutReplace1, pattern = acro2, replacement = acroComp2)),
    ansOutReplace3 = ifelse(outAcro3 == FALSE | is.na(outAcro3), ansOutReplace2, str_replace(ansOutReplace2, pattern = acro3, replacement = acroComp3)),
    ansOutReplace4 = ifelse(outAcro4 == FALSE | is.na(outAcro4), ansOutReplace3, str_replace(ansOutReplace3, pattern = acro4, replacement = acroComp4)),
    
    ansOutReplace5 = ifelse(outAcro5 == FALSE | is.na(outAcro5), ansOutReplace4, str_replace(ansOutReplace4, pattern = acro5, replacement = acroComp5)),
    ansOutReplace6 = ifelse(outAcro6 == FALSE | is.na(outAcro6), ansOutReplace5, str_replace(ansOutReplace5, pattern = acro6, replacement = acroComp6)),
    ansOutReplace7 = ifelse(outAcro7 == FALSE | is.na(outAcro7), ansOutReplace6, str_replace(ansOutReplace6, pattern = acro7, replacement = acroComp7))
    
  ) %>% 
  select(doc_id, PMID,  "outcome" =  ansOutReplace7, "EXPOSURE" =  ansExposureReplace7 )
         
## 1.0 get expHigh1, expLow1, expUnit1 ####

units <- c("ng\\s?/\\s?ml",
           "microg\\s?/\\s?l",
           "mg",
           "µg\\s?/\\s?l",
           "g\\s?/\\s?dl",
           "g\\s?/\\s?l",
           "μmol\\s?/\\s?l",
           "μmol\\s?/\\s?ml",
           "servings/week",
           "servings/wk",
           "iu\\s?/\\s?l",
           "u\\s?/\\s?l",
           "μg\\s?/\\s?ml",
           "u\\s?/\\s?ml",
           "ng\\s?/\\s?l",
           "x109\\s?/\\s?l",
           "ml\\s?/\\s?min",
           "dl\\s?/\\s?min",
           "%",
           "years", "months", "days")

unitsRegex = paste0(units, collapse = "|") 

## less than, more than etc

moreLess <- c("<\\s?=",">\\s?=", "<",">","=", "≤","≥",
              "more than", "higher than","greater than",
              "less than", "lower than", "(?<=\\d\\s?)-(?=\\s?\\d)")

moreLessRegex = paste0(moreLess, collapse = "|")


dataPython5 <- dataPython4 %>% 
  mutate(EXPOSURELow = tolower(EXPOSURE),
        expUnit1 = str_extract(EXPOSURELow, pattern = unitsRegex),
        moreLessThan = str_extract(EXPOSURELow, pattern = moreLessRegex ),
        expHigh1 = ifelse(moreLessThan %in% c("<\\s?=,","≤","<", "less than", "lower than"), 
                          str_extract(EXPOSURELow, "\\d+\\.?\\d?"), NA),
        expLow1 = ifelse(moreLessThan %in% c(">\\s?=,",">", "≥","more than", "higher than", "greater than"), 
                          str_extract(EXPOSURELow, "\\d+\\.?\\,?\\d?"), NA),
        EXPOSURE = str_remove(EXPOSURE, paste0("(",moreLessRegex, ")", ".*")),
        EXPOSURE = str_trim(tolower(EXPOSURE) ),
        expType = ifelse(moreLessThan %in%  c("<\\s?=,","≤","<", "less than", "lower than"),"lower than",
                         ifelse(moreLessThan %in% c(">\\s?=,",">", "≥","more than", "higher than", "greater than"),"greater than",
                                ifelse(moreLessThan == "=", "equal to", 
                                       ifelse(moreLessThan == "-", "between",
                                       "no numeric treshold")))),
        outcome = tolower(outcome),

        ) %>% 
  filter(EXPOSURE != "empty", EXPOSURE !="", outcome !="empty") 


## 1.1: join results data ####


results <- dataORHRbefore %>% left_join(
  dataPython5, by = c("doc_id" , "PMID")  
) %>% 
  mutate(idResult = doc_id,
         
         idStudy = PMID,
         idPopulation = idStudy,
         idProject = NA
         ) %>% 
  mutate(
        exp1 = EXPOSURE,
        expType = expType,
        refExp1 = NA,
        result = measureValue2,
        icLow = CiLow2,
        icUpper = CiUp2,
        exp2 = NA,
        adjustment = NA,
        user_id = NA
  ) 
  
  
data$studyPlan

# 2. create study dataframe #### 

study <- data %>% 
  select("title" = TITLE, "author" = AUTHOR,
         "yearPublication" = publicationYear,
         journal, journalShort, "objective" = studyObjective,
         cohortYN, PMID, ABSTRACT,studyPlan) %>% 
  mutate(idStudy = PMID,
         user_id = NA,
         idProject = NA,
         centric = NA,
         startYear = NA,
         endYear = NA,
         discussion = NA) 

# 3. create population dataframe #### 

## 3.1 clean data participants ####

library(wordstonumbers)

for (i in seq_len(length(dataParticipantsPython$ansParticipants))) {
  
  dataParticipantsPython$N2[i] <- words_to_numbers(dataParticipantsPython$ansParticipants[i])  
}


### 3.1.1 clean popSize and diseases ####

particpantsN <-  dataParticipantsPython %>% ungroup() %>% 
  mutate(N3 = ifelse(str_detect(N2,"%"), NA, 
                     ifelse(str_detect(N2,"(n|N)\\s?="), str_extract(N2, "(?<=\\=)(\\s?\\d+)"),
                            ifelse(str_detect(N2, "age|year|\\$"),NA,
                                   ifelse(str_detect(N2,"\\d+\\."),NA,
                     N2)))), ## replacing with NA if % detected
         N4 = str_remove_all(N3, "(\\s+|\\,|\\.)" ),
         popSize2 = str_extract(N4, "\\d+"),
         popSize = ifelse(str_detect(popSize2, "^0"),NA, popSize2),
         PMID = as.character(PMID),
         ansComorbidity = ifelse(ansComorbidity == ansPopDisease,NA,ansComorbidity)
  ) %>% select(PMID, 
               popSize,
               ansPopDisease,ansComorbidity
               )

### 3.1.2 clean age ####

units2 <- c("ng\\s?/\\s?ml",
           "microg\\s?/\\s?l",
           "mg",
           "µg\\s?/\\s?l",
           "g\\s?/\\s?dl",
           "g\\s?/\\s?l",
           "μmol\\s?/\\s?l",
           "μmol\\s?/\\s?ml",
           "servings/week",
           "servings/wk",
           "iu\\s?/\\s?l",
           "u\\s?/\\s?l",
           "μg\\s?/\\s?ml",
           "u\\s?/\\s?ml",
           "ng\\s?/\\s?l",
           "x109\\s?/\\s?l",
           "m(l|L)\\s?/\\s?min",
           "micrograms/g",
           "μ(M|m)",
           "%",
           "(p|P)\\s?(=|<|>)",
           "0\\.",
           "n\\s?="
           )

units2Regex = paste0(units2, collapse = "|") 

participantsAge <- dataParticipantsPython %>% ungroup() %>% 
  select(PMID,  ansAgeNumeric)  %>% 
  mutate(ansAgeNumeric = tolower(ansAgeNumeric),
        ageNumeric2 = ifelse(str_detect(ansAgeNumeric,"(p|P)\\s?(=|<|>)"),NA,
                            ifelse(str_detect(ansAgeNumeric,"^[.-]"),NA,
                                   ifelse(!str_detect(ansAgeNumeric,"[0-9]"),NA,
                                   ansAgeNumeric ))),
         ageNumeric3 = ifelse(str_detect(ageNumeric2, units2Regex),NA, ageNumeric2),
        ageSD = str_extract(ageNumeric3, "(?<=(±|(\\+\\/-)))(\\s?\\d+\\.?\\d?)"),
        ageMin = str_extract(ageNumeric3, "(\\d+\\.?\\d?)(?=\\s?(;|-|to|−))"),
        ageMax = str_extract(ageNumeric3, "(?<=\\s?(-|to|−|;))(\\s?\\d+\\.?\\d?)"),
        ageMax = as.numeric(ageMax),
        ageSD = as.numeric(ageSD),
        ageValue2 = str_remove_all(ageNumeric3,paste0("(",ageSD,"|",ageMin,"|",ageMax,")" )),
        ageValue = as.numeric(str_extract(ageValue2, "\\d+\\.?\\d?") ) ,
        # ageClass = ifelse(is.na(ageNumeric3), ansAgeClass, paste0(ansAgeClass, " or ", ageNumeric3) ),
        ageUnit = str_extract(ansAgeNumeric, "day|month|year"),
        ) %>% 
  select(PMID,ageUnit, ageValue, ageSD, ageMin, ageMax)

sexRegex <- c(
  "man",
  "men",
  "woman",
  "women",
  "male",
  "female",
  "boy",
  "girl"
)

sexReg <- paste0(sexRegex, collapse = "|")

participantsSex <- dataParticipantsPython %>% ungroup() %>% 
  select(PMID, ansMen, ansWomen) %>% 
  mutate(maleFreq = as.numeric(str_extract(ansMen, "(\\d+\\.?\\d?)(?=\\s?%)") ),
         femaleFreq2 = str_extract(ansWomen, "(\\d+\\.?\\d?)(?=\\s?%)"),
         # sexClass = str_remove_all(ansSex, "\\:|-"),
         # sexClass = str_trim(sexClass),
         femaleFreq = ifelse(is.na(femaleFreq2), 100 - maleFreq, femaleFreq2)
         ) %>% 
  select(PMID, # sexClass, 
         femaleFreq)


dataPartic <- particpantsN %>% left_join(participantsAge) %>% 
  left_join(participantsSex)


populations <- data  %>% 
  select(PMID, country, "ageValueReg" = ageValue, "ageUnitReg" = ageUnit , popDisease ) %>% 
  left_join(dataPartic, by = "PMID") %>% 
  mutate(ageUnit = ifelse(is.na(ageUnit), ageUnitReg, ageUnit ),
         ageValue = ifelse(is.na(ageValue), ageValueReg, ageValue)
         ) %>%
  mutate(across(c("ageMin", "ageMax", "ageSD","ageValue"), ~ifelse(.>100,NA,.)) ) %>% 
  mutate(across(c("ageMin", "ageMax", "ageSD","ageValue"), ~as.numeric(.)) ) %>%           
  select(PMID,"popCountry" =  country, popSize, "ageLow" = ageMin, "ageUp" = ageMax, ageUnit,femaleFreq,
         ageValue, ageSD, popDisease,"popDiseaseLlama" = ansPopDisease , "popComorbidity" = ansComorbidity) %>% 
  mutate(
    popDisease = ifelse(popDisease == "", NA, popDisease),
    popDisease = ifelse(is.na(popDisease), popDiseaseLlama, popDisease),
    user_id = NA,
    idProject = NA,
    idStudy = PMID,
    idPopulation = idStudy,
    pNam = NA,
    popMainChar = NA,
    pSecChar = NA,
    inclCrit = NA,
    ageUni = ageUnit,
    ageMean = ageValue,
    ageClass = NA,
    sexClass = NA,
  ) %>% 
  select(-c(ageUnit, ageValue)) 

# 4. Clean Frequencies ####

stop <- stopwords() %>% append(c("s","0", "the", "frequents?", "p", "vs","n", "relationship", "definitions?", "conclusions?", "a", "and", "among",
                                 "significantly", "increase","increased","decrease","decreased", "frequently", "risk", "hazard ratio","hr","odd ratio", 
                                 "odd ratios","odds ratio", "univariate","likelihood","review","meta","overview", "documentation",
                                 "higher", "lower","in ","reduction","combined", "reduced","early","even","concentration","concentrations","inferior","overall",
                                 "included","significant","adjusted","analyses","analysis","multivariable","model","models","associated",
                                 "\\b.*ing\\b"))

stop2 <- paste0("\\b",stop , "\\b")
pasteStop <- paste(stop2,collapse = '|' )



frequencies2 <- dataFreqBefore %>% 
  left_join(dataFreqPython, by = c("doc_id","PMID") ) %>% 
  left_join( data %>% select(PMID, TITLE, country, AUTHOR), by = "PMID" ) %>% 
  mutate(freqMeasure = keywordFinal,
         expFrequencyName = ansFreq,
         freqUnit = "%",
         freqType = NA,
         subpopSize = NA,
         subset1 = ansGroup,
         subset2 = NA,
         subset3 = NA,
         idProject = NA,
         idFreq = NA,
        
         idStudy = PMID,
         idPopulation = idStudy,
         user_id = NA,
         exprTitle1 = NA,
         exprTitle2 = NA,
         exprTitle3 = NA
         ) %>% 
  select(idProject, idFreq, idPopulation, idStudy, user_id, subpopSize , PMID, TITLE,
         subset1, subset2, subset3, expFrequencyName, freqType, freqMeasure, freqUnit,exprTitle1, exprTitle2,exprTitle3, country , AUTHOR)
  


frequencies <-  frequencies2 %>% 
  ## clean Frequency Name
  mutate(expFrequencyName2 = str_replace_all(expFrequencyName, "\\-"," " ),
         expFrequencyName3 = str_remove_all(expFrequencyName2, "[[:digit:][:punct:]]"  ),
         expFrequencyName4 = str_replace_all(expFrequencyName3, pasteStop, " " ),
         expFrequencyName5 = str_replace_all(expFrequencyName4, "\\s{2,6}", " "  ),
         expFrequencyName6 = tolower(str_trim(expFrequencyName5)),
         freqTo = str_detect(freqMeasure,"to"),
         freqLow = ifelse(freqTo == "TRUE", str_extract( tolower(freqMeasure),"\\d+\\.?\\d+.*(?=to)"),NA),
         freqLow = as.numeric(str_extract(freqLow , "\\d+\\.?\\d+?")),
         freqHigh = ifelse(freqTo == "TRUE", str_extract( tolower(freqMeasure),"(?<=to).*\\d+\\.?\\d+"),NA),
         freqHigh = as.numeric(str_extract(freqHigh , "\\d+\\.?\\d+?")), 
         freqNum =  as.numeric(str_extract(freqMeasure, "\\d+\\.?\\d?")) 
  ) %>% 
  filter(freqNum < 100, expFrequencyName6 !="" ) %>% 
  select(idProject, idFreq, idPopulation, idStudy, user_id, PMID, TITLE,
         AUTHOR,
         "expFrequencyName" = expFrequencyName6,
         freqNum, freqLow,freqHigh, freqUnit,  
         subpopSize , 
         subset1, subset2, subset3, 
         freqType, 
         freqMeasure, 
         exprTitle1, exprTitle2,exprTitle3, country )


# To shiny to test the app #### 

saveRDS(study, paste0("1_databasesForLocalApp/studies/studies", projectName,".rds") )
saveRDS(populations, paste0("1_databasesForLocalApp/populations/populations", projectName,".rds") )
saveRDS(results, paste0("1_databasesForLocalApp/results/results",projectName ,".rds") )
saveRDS(frequencies,paste0("1_databasesForLocalApp/frequencies/frequencies", projectName,".rds") )


# 4. PHPMyADMIN #### 

## 4.1 Connection ####

source("../../credentials/connection.r")


## 4.2 get datas from Study ####

runStatement <- dbSendQuery(connection, "SELECT PMID FROM studiesfromcode")

studiesfromDb <- dbFetch(runStatement , n = -1
              )   %>% select(PMID)

dbDisconnect(connection)



## 4.2 get datas from Study ####

source("../../credentials/connection.r")

runStatement <- dbSendQuery(connection, "SELECT PMID FROM frequenciesfromcode")

freqfromDb <- dbFetch(runStatement , n = -1
)   %>% select(PMID)

dbDisconnect(connection)

# format(Sys.Date(), format = "%d-%m-%Y")

## 4.3 Filter PMID not in PHP database ####

study2 <- study %>%  filter(PMID %notin% studiesfromDb$PMID)%>% 
  mutate(idStudy0 = NA) %>% 
  select(idStudy0 , PMID, title, author,ABSTRACT,  yearPublication, journal, objective, studyPlan) %>% 
  mutate(fromCode = 1,
         projectName = projectName,
         uploadDate = format(Sys.Date(), format = "%Y-%m-%d"))

populations2 <- populations %>%   filter(PMID %notin% studiesfromDb$PMID)%>% 
  mutate(idPopulations0 = NA) %>% 
  select(idPopulations0, PMID, pNam, popSize, popCountry, popMainChar, pSecChar,
         inclCrit, ageUni, ageMean, ageSD,ageLow,ageUp,femaleFreq, ageClass, sexClass, popDisease, popComorbidity)%>% 
  mutate(fromCode = 1,
         projectName = projectName,
         uploadDate = format(Sys.Date(), format = "%Y-%m-%d"))

results2 <- results   %>%   filter(PMID %notin% studiesfromDb$PMID)%>%
  mutate(idResults0 = NA) %>% 
  select(idResults0, doc_id, PMID, exp1, expType, expLow1, expHigh1, expUnit1, refExp1,
          outcome, result, icLow, icUpper, measureType, exp2, adjustment)%>% 
  mutate(fromCode = 1,
         projectName = projectName) %>% 
  mutate(across(c("expLow1","expHigh1", "result","icLow","icUpper"), ~as.numeric(.)) ) %>% 
  mutate(expLow1 = ifelse(is.na(expLow1),paste0("NULL"),expLow1  ),
         expHigh1 = ifelse(is.na(expHigh1),paste0("NULL"),expLow1  ),
         result = ifelse(is.na(result),paste0("NULL"),result  ),
         icLow = ifelse(is.na(icLow),paste0("NULL"),icLow  ),
         icUpper = ifelse(is.na(icUpper),paste0("NULL"),icUpper  ),
         uploadDate = format(Sys.Date(), format = "%Y-%m-%d")
         )
  # mutate(across(where(is.numeric), ~replace_na(., "NULL")) )



  frequencies3 <- frequencies  %>%   filter(PMID %notin% freqfromDb$PMID)%>%
  mutate(idFreq0 = NA) %>% 
  select(idFreq0 , PMID, subpopSize, subset1, subset2, subset3, expFrequencyName,
         freqType, freqMeasure, freqUnit)%>% 
  mutate(fromCode = 1,
         projectName = projectName,

         uploadDate = format(Sys.Date(), format = "%Y-%m-%d")
         )


## 4.4 Append new data or write the CSV to append manually ####

write.csv(study2, paste0("2_toPHPmyAdmin/1_studies/studiesToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")

write.csv(populations2, paste0("2_toPHPmyAdmin/2_populations/populationsToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")

write.csv(results2, paste0("2_toPHPmyAdmin/3_results/resultsToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")

write.csv(frequencies3, paste0("2_toPHPmyAdmin/4_frequencies/frequenciesToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")


## 4.5 Append new data From R ####







