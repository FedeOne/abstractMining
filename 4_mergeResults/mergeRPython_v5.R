
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
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(tm::stopwords)
# Project Name ----
projectName <- "ironAnemiaLisa1"

`%notin%` <- Negate(`%in%`)

# Path ----

currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("mergeRPython_v5.R")

setwd(currentPath2) 

# Load data ----

data <- readRDS(paste0("../1_regexInfos/treatedDB/",projectName,".rds") )

## best models seem Roberta, bert_distill


dataPyORHRPVal <- arrow::read_feather(paste0("../3_Python/1_outputspValOR/1_exposure/outORHRExpOutRef",projectName,".feather"))

dataORHRbefore <-  readRDS(paste0("../2_forPython/2_FullDataFrameORHRRR/ORHRRR2",projectName,".rds") ) %>% 
  select(-c(measureType))


dataParticipantsPython <- arrow::read_feather(paste0("../3_Python/2_outputPop/outPopFinal",projectName,".feather" )) %>% 
  select(PMID, contains("ans"))

dataTrialsPython <- arrow::read_feather(paste0("../3_Python/3_outputTrial/outTrial",projectName,".feather" )) %>% 
  select(PMID, contains("ans"))


# 1: clean python ORHR answers #### 

# clean regex
# stpWrds <- stop_words %>% 
#   as.data.frame() %>% 
#   select(1)

  stpWrds <- data.frame(word = 
                     c("compared","associated","was", "a", "overall", "\\,",
                       "combination", "patients?", "with", "those")
  )  %>% 
  mutate(word = paste0("\\b(?i)", word, "\\b"))

cleanReg <-  paste0(stpWrds$word, collapse = "|" )


dataPython2 <- dataPyORHRPVal  %>% 
  # select(doc_id, PMID, ansOutcome , ansExposure, ansReference, measureType, result ) %>% 
   mutate(
           ansExposure = str_remove_all(ansExposure, cleanReg) %>% str_trim(),
           ansOutcome = str_remove_all(ansOutcome, cleanReg ) %>% str_trim(),
           ansReference = str_remove_all(ansReference, cleanReg) %>% str_trim(),
           pValue = str_extract(result, "(p|P).*")
          ) %>% 
  select(doc_id, PMID, measureType , ansExposure,ansOutcome, ansReference, pValue) %>% 
  left_join(select(data, PMID, contains("acro")), by = "PMID")
  
  
### replace acronyms

dataPython3 <-  dataPython2 %>% 
  select(doc_id, PMID, measureType, pValue, ansExposure,  acro1, acroComp1, acro2, acroComp2,  acro3, acroComp3, acro4, acroComp4,acro5, acroComp5,acro6, acroComp6,acro7, acroComp7, ansOutcome, ansReference) %>% 
  mutate(across(c(acro1,acro2, acro3, acro4, acro5, acro6, acro7), ~str_remove_all(.,"\\(|\\)") )) %>% 
  mutate(
    ansExposure2 = ifelse(str_detect(ansExposure, acro1), str_replace(ansExposure, acro1, acroComp1), 
                             ifelse(str_detect(ansExposure, acro2), str_replace(ansExposure, acro2, acroComp2), 
                                    ifelse(str_detect(ansExposure, acro3), str_replace(ansExposure, acro3, acroComp3), 
                                           ifelse(str_detect(ansExposure, acro4), str_replace(ansExposure, acro4, acroComp4), 
                                                  ifelse(str_detect(ansExposure, acro5), str_replace(ansExposure, acro5, acroComp5), 
                                                         ifelse(str_detect(ansExposure, acro6), str_replace(ansExposure, acro6, acroComp6), 
                                                                ifelse(str_detect(ansExposure, acro7), str_replace(ansExposure, acro7, acroComp7), 
                                                                       ansExposure  ))))))),
    ansOutcome2 = ifelse(str_detect(ansOutcome, acro1), str_replace(ansOutcome, acro1, acroComp1), 
                          ifelse(str_detect(ansOutcome, acro2), str_replace(ansOutcome, acro2, acroComp2), 
                                 ifelse(str_detect(ansOutcome, acro3), str_replace(ansOutcome, acro3, acroComp3), 
                                        ifelse(str_detect(ansOutcome, acro4), str_replace(ansOutcome, acro4, acroComp4), 
                                               ifelse(str_detect(ansOutcome, acro5), str_replace(ansOutcome, acro5, acroComp5), 
                                                      ifelse(str_detect(ansOutcome, acro6), str_replace(ansOutcome, acro6, acroComp6), 
                                                             ifelse(str_detect(ansOutcome, acro7), str_replace(ansOutcome, acro7, acroComp7), 
                                                                    ansOutcome  ))))))),
    ansExposure = ifelse(!is.na(ansExposure2), ansExposure2, ansExposure),
    ansOutcome = ifelse(!is.na(ansOutcome2), ansOutcome2, ansOutcome),
    ansReference = ifelse(str_detect(ansReference, "ANSWERNOTFOUND"), " ", ansReference)
    
  ) %>% 
  relocate(ansExposure2, .after = ansExposure) %>% 
  relocate(ansOutcome2, .after= ansOutcome) %>% 
  select(doc_id, PMID, measureType,  "outcome" =  ansOutcome, "EXPOSURE" =  ansExposure, ansReference, pValue ) 
         
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


dataPython4 <- dataPython3 %>% 
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


results <- dataPython4  %>% left_join(
  dataORHRbefore, by = c("doc_id" , "PMID")  
) %>% 
  mutate(idResult = doc_id,
         
         idStudy = PMID,
         idPopulation = idStudy,
         idProject = NA
         ) %>% 
  mutate(
        exp1 = EXPOSURE,
        expType = expType,
        refExp1 = ansReference,
        result = measureValue2,
        icLow = CiLow2,
        icUpper = CiUp2,
        exp2 = NA,
        adjustment = NA,
        user_id = NA
  ) 
  
  

# 2. create study dataframe #### 

study <- data %>% 
  left_join(dataTrialsPython, by = "PMID") %>% 
  select("title" = TITLE, "author" = AUTHOR,
         "yearPublication" = publicationYear,
         journalShort, "objective" = studyObjective,
         PMID, ABSTRACT,studyPlan, keywords, "mainIntervention" = ansIntervention, "mainEndpoint" = ansEndpoint) %>% 
  mutate(idStudy = PMID,
         user_id = NA,
         journal = NA,
         idProject = NA,
         centric = NA,
         startYear = NA,
         endYear = NA,
         discussion = NA) 

# 3. create population dataframe #### 

dataParticipantsPython2 <- dataParticipantsPython %>% 
  mutate(across(
    c(ansParticipants, ansCases, ansControls),tolower
  ), 
  ansParticipants = str_remove_all(ansParticipants, "\\,|\\."),
  ansCases = str_remove_all(ansCases, "\\,|\\."),
  ansControls = str_remove_all(ansControls, "\\,|\\."),
  
  ansParticipants = str_trim(str_squish(str_replace_all(ansParticipants, 
                                             pattern = "answernotfound|[\\p{P}\\p{S}]",
                                             replacement = " "))),
  ansCases = str_trim(str_squish(str_replace_all(ansCases,
                                      pattern =  "answernotfound|[\\p{P}\\p{S}]",
                                      replacement = " ") ) ),
  ansControls = str_trim(str_squish(str_replace_all(ansControls,
                                         pattern =  "answernotfound|[\\p{P}\\p{S}]",
                                         replacement = " ") ) ),
  )


## 3.1 clean data participants ####

library(wordstonumbers)

for (i in seq_len(length(dataParticipantsPython$ansParticipants))) {
  
  dataParticipantsPython2$popSize[i] <- words_to_numbers(dataParticipantsPython2$ansParticipants[i])  
  dataParticipantsPython2$nCases[i] <- words_to_numbers(dataParticipantsPython2$ansCases[i])
  dataParticipantsPython2$nControls[i] <- words_to_numbers(dataParticipantsPython2$ansControls[i])
}


### 3.1.1 clean popSize and diseases ####

particpantsN <-  dataParticipantsPython2 %>% ungroup() %>% 
  mutate(
    popSize = str_extract(popSize, "\\d+"),
    nCases = str_extract(nCases, "\\d+"),
    nControls = str_extract(nControls, "\\d+"),
    PMID = as.character(PMID),
    ansPopDiseaseAbstract = str_replace(ansPopDiseaseAbstract,"ANSWERNOTFOUND", ""),
    ansPopDiseaseTitle = str_replace(ansPopDiseaseTitle,"ANSWERNOTFOUND", "")
    )  %>% select(PMID, 
               popSize,
               nCases,
               nControls,
               ansPopDiseaseTitle,
               ansPopDiseaseAbstract
               )

### 3.1.2 create pop DB ####



populations <- data  %>% 
  select(PMID, country ) %>% 
  left_join(particpantsN, by = "PMID") %>% 
  mutate(pSecChar = ansPopDiseaseAbstract) %>% 
  select(PMID,"popCountry" =  country, popSize,  "popDisease" = ansPopDiseaseTitle, pSecChar, nCases, nControls ) %>% 
  mutate(
    user_id = NA,
    idProject = NA,
    idStudy = PMID,
    idPopulation = idStudy,
    pNam = NA,
    popMainChar = NA,
    inclCrit = NA,
    ageUni = NA,
    ageMean = NA,
    ageSD = NA,
    ageClass = NA,
    sexClass = NA,
    ageLow = NA,
    ageUp = NA,
    femaleFreq = NA,
    popComorbidity = NA
  ) 


# To shiny to test the app #### 

saveRDS(study, paste0("1_databasesForLocalApp/studies/studies", projectName,".rds") )
saveRDS(populations, paste0("1_databasesForLocalApp/populations/populations", projectName,".rds") )
saveRDS(results, paste0("1_databasesForLocalApp/results/results",projectName ,".rds") )



# 4. PHPMyADMIN #### 

## 4.1 Connection ####

source("../../credentials/connection.r")


## 4.2 get existing PMIDS from Study ####

runStatement <- dbSendQuery(connection, "SELECT PMID FROM studiesfromcode")

studiesfromDb <- dbFetch(runStatement , n = -1
              )   %>% select(PMID)

dbDisconnect(connection)




# format(Sys.Date(), format = "%d-%m-%Y")

## 4.3 Filter PMID not in PHP database ####

study2 <- study %>%  filter(PMID %notin% studiesfromDb$PMID)%>% 
  mutate(idStudy0 = NA) %>% 
  select(idStudy0 , PMID, title, author,ABSTRACT,  yearPublication, journal,journalShort, objective, studyPlan, 
         keywords, mainIntervention, mainEndpoint) %>% 
  mutate(fromCode = 1,
         projectName = projectName,
         uploadDate = format(Sys.Date(), format = "%Y-%m-%d"))

populations2 <- populations %>%   filter(PMID %notin% studiesfromDb$PMID)%>% 
  mutate(idPopulations0 = NA) %>% 
  select(idPopulations0, PMID, pNam, popSize, nCases, nControls, popCountry, popMainChar, pSecChar,
         inclCrit, ageUni, ageMean, ageSD,ageLow,ageUp,femaleFreq, ageClass, sexClass, popDisease, popComorbidity)%>% 
  mutate(fromCode = 1,
         projectName = projectName,
         uploadDate = format(Sys.Date(), format = "%Y-%m-%d"))

results2 <- results  %>% filter(PMID %notin% studiesfromDb$PMID)%>%
  mutate(idResults0 = NA) %>% 
  select(idResults0, doc_id, PMID, exp1, expType, expLow1, expHigh1, expUnit1, refExp1,
          outcome, result, icLow, icUpper, measureType, pValue, exp2, adjustment)%>% 
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





## 4.4 Append new data or write the CSV to append manually ####

write.csv(study2, paste0("2_toPHPmyAdmin/1_studies/studiesToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")

write.csv(populations2, paste0("2_toPHPmyAdmin/2_populations/populationsToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")

write.csv(results2, paste0("2_toPHPmyAdmin/3_results/resultsToAppendFromCode",projectName,".csv" ),
          row.names = FALSE,  na = "")





c <- results2 %>% 
  mutate(idRes = paste0(result, icLow, icUpper, PMID)) 


results %>% 
  filter(PMID=="34339746")


study2 %>% 
  filter(PMID == "34339746")
  