
projectName <- "vacNemus2"



library(rstudioapi)
library(quanteda) # for stopwords function

library(dplyr)
library(tidyverse)
library(stringr)
library(conflicted)
library(readxl)
library(feather) # to Export data in feather format

`%notin%` <- Negate(`%in%`)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("forPythonCreateQuestionsTestRegex.R")

setwd(currentPath2) 

dataOri <- readRDS(paste0("../1_regexInfos/treatedDB/",projectName,".rds")) %>% 
  mutate(PMID = str_trim(PMID),
         ABSTRACT = str_replace_all(ABSTRACT,"\U00B7", "\\.")
         )

data <- dataOri %>% 
  mutate(containsHR = str_detect(ABSTRACT, "(\\bHR\\b)|(?i)hazard ratio|(?i)cox model|(?i)survival"), ## ?i is for case insensitive
         containsOR = str_detect(ABSTRACT, "(\\bOR\\b)|(?i)odd ratio|(?i)logistic"),
         containsRR = str_detect(ABSTRACT, "(\\bRR\\b)|(?i)relative risk"),
         containsIncidence = str_detect(ABSTRACT, regex("incidence",ignore_case=TRUE) ), 
         containsPrevalence = str_detect(ABSTRACT, regex("prevalence",ignore_case=TRUE) ),
  ) 


# 1: OR HR RR ####

## 1.a get all rows with OR HR RR ####

ORHR <- data %>% 
  filter(!is.na(PMID) | !is.na(ABSTRACT) ) %>% 
  filter(containsOR ==TRUE | containsHR == TRUE| containsRR == TRUE) %>% 
  mutate(doc_id = as.character(PMID)) %>% 
  arrange(PMID)


corp <- corpus(ORHR, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)


### 1.a.1 kwic on text

ORHRPhrase <-  kwic(toks, pattern =   c("\\bOR\\b","\\bAOR\\b","\\bHR\\b","\\bRR\\b",
                                          phrase("hazard ratio"), phrase("odd ratio"), phrase("relative risk")),valuetype = "regex",
               window = 95, case_insensitive = FALSE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>% 
  mutate(measureId = paste0(PMID,"_", keyword)) %>% 
  mutate(reg = "ORHRKwic")


### 1.a.2 str_extract on numbers without text and filter the ones which are not above

listORHRRegex <- data.frame()

for (z in 1:length(ORHR$ABSTRACT)) {
  
  PMID <- ORHR$PMID[z]
  ORHR$ABSTRACT[z] <- str_remove_all(ORHR$ABSTRACT[z], "((?i)confidence\\(|\\b(?i)IC\\b95|95\\s?%|\\b(?i)CI\\b\\s?95|\\bCI\\b|\\bIC\\b|interval)")
  ORHR$ABSTRACT[z] <- str_squish(ORHR$ABSTRACT[z])
  temp <- str_extract_all(ORHR$ABSTRACT[z],"\\d+\\.?\\d+\\s?(,|;|\\[|\\().{0,15}\\d+(\\.\\d+)\\s?(-|to|;)\\s?\\d+(\\.\\d+)" ) %>% # extracting measure values and confidence intervals
    unlist() %>% 
    as.data.frame() %>% 
    rename(post = 1) %>% 
    mutate(PMID = PMID)
  
  listORHRRegex <- listORHRRegex %>% 
    rbind(temp)
  
  
}

listORHRRegex2 <- listORHRRegex %>% 
  mutate(from = NA, 
         to = NA,
         pre = NA, 
         keyword = NA,
         pattern = NA, 
         measureId = NA,
         reg = "ORHRReg") %>% 
  select(names(ORHRPhrase))



joinRegex <- ORHRPhrase %>% 
  rbind(listORHRRegex2) %>% 
  group_by(PMID) %>% 
  fill(keyword, .direction = "downup") %>% 
  filter(!is.na(keyword))





### 1.a.2  join and mutate some

ORHRPhraseB <- joinRegex  %>% 
  select(-c(measureId)) %>% 
  mutate(reducedPost = str_extract(post,".{20,250}"),
         contextORHR = paste0(pre, " " , keyword, " " , reducedPost)) %>% 
  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank,"_OR"),
         doc_id = str_remove_all(doc_id," ")) %>% 
  ungroup() %>% 
  select(doc_id, PMID, keyword, post,contextORHR, reg)  




ORHRPhrase2 <- ORHRPhraseB %>%

  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(
    measureType2 = str_remove_all(keyword, "(?<=(OR|HR|RR)).*"),
    measureType2 = str_remove_all(measureType2, ".*(?=(OR|HR|RR))"),
    measureType = ifelse(str_detect(measureType2, "OR|odds? ratios?"), paste0("odd ratio (OR)"),
                         ifelse( str_detect(measureType2, "HR|hazards? ratios?") ,paste0("hazard ratio (HR)"),
                                ifelse(str_detect(measureType2, "RR|relative risks?") ,paste0("relative risk (RR)"),
                                                          NA))) 
    ) %>% 
  fill(measureType, .direction = "down")
  


## 1.b get value and CI ####


getMeasures <- ORHRPhrase2 %>% ungroup() %>% 
  mutate(post2 = post,
         post2 = str_remove_all(post2, "(95\\s?%|95\\s?percent)"),
         measureValue = str_extract(post2, "(?<=^.{0,5})(\\d+(\\.\\d+)?)") ) %>% 
  filter(!is.na(measureValue)) ## normally I check that all measures are considered
                           
### 1b2: get CI ####

getMeasuresKwic <- getMeasures %>% 
  filter(reg == "ORHRKwic") %>% 
  mutate(CiLow = ifelse(str_detect(post, paste0(measureValue,".{0,15}?(\\(|IC 95|95\\s?%|CI\\s?95|CI|IC|interval)")) |str_detect(post,paste0("(IC 95|95\\s?%|CI\\s?95|CI|IC|interval){0,15}?",measureValue)),
                        str_extract(post2, paste0("(?<=",measureValue,").*?(\\d+(\\.\\d+)?)")),NA),
         CiLow = str_remove_all(CiLow, "(95\\s?%|CI\\s?95|\\bCI\\b|\\bC\\b\\s?\\.?|\\bI\\b\\s?\\.?|\\:|\\;)"),
         CiLow = str_squish(CiLow),
         CiLow2 = ifelse(str_detect(CiLow, "(?i)\\bp\\b\\s?=|(?i)\\bp\\b\\s?<|(?i)\\bp\\b\\s?>|\\bOR\\b\\s?=|\\bHR\\b\\s?=|\\bRR\\b\\s?="), 
                         NA, CiLow),
         
         CiLow2 = str_remove_all(CiLow2, "[^\\d.]+"),
         CiUp = str_extract(post, paste0("(?<=", CiLow2, ").{10}")),
         CiUp = str_remove_all(CiUp, "[^\\d.]+"),
         CiUp = str_remove(CiUp, "\\.$"),
         CiUp2 = as.numeric(CiUp),
         CiLow2 = as.numeric(CiLow2),
         measureValue2 = as.numeric(measureValue),
         pValue = ifelse(str_detect(post2, paste0(measureValue, ".{0,15}(\\b(?i)p\\b\\s?=|\\b(?i)p\\b\\s?<|\\b(?i)p\\b\\s?>)")), 
                                    str_extract(post2, paste0("(?<=", measureValue, ".{0,15})(\\b(?i)p\\b\\s?=|\\b(?i)p\\b\\s?<|\\b(?i)p\\b\\s?>)\\s+(\\d+(\\.\\d+)?)") ),
                                    NA )
  ) %>% 
  filter(!is.na(CiLow2)|!is.na(pValue)) %>% 
  filter(!is.na(CiUp2)|!is.na(pValue)) %>% 
  select(doc_id, PMID, keyword, measureType, measureValue2,CiLow2, CiUp2,pValue)
         


getMeasuresReg <- getMeasures %>% 
  filter(reg == "ORHRReg") %>%    
  mutate(post2 = str_remove(post2,"confidence"),
         CiLow = str_extract(post2, paste0("(?<=",measureValue,").{0,15}?(\\d+(\\.\\d+)?)" ) ),
         CiLow = str_remove(CiLow, "(95 %|95%|CI\\s?95)"),
         CiLow = str_remove_all(CiLow, "[^\\d.]+") ,
         CiUp =  str_extract(post, "(\\d+(\\.\\d+))$"),
         CiUp = str_remove_all(CiUp, "[^\\d.]+"),
         CiUp = str_remove(CiUp, "\\.$"),
         CiUp2 = as.numeric(CiUp),
         CiLow2 = as.numeric(CiLow),
         measureValue2 = as.numeric(measureValue),
         pValue = NA
  ) %>% 
  select(doc_id, PMID, keyword, measureType,  measureValue2,CiLow2, CiUp2,pValue)


measuresConfint <- rbind(getMeasuresReg, getMeasuresKwic) %>% 
  mutate(idResult = ifelse(is.na(pValue), paste0(PMID,"_",measureValue2,"_",CiLow2,"_",CiUp2),
                           paste0(PMID,"_",measureValue2,"_",pValue))
                           ) %>% 
  group_by(idResult) %>% 
  arrange(idResult) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(idResult)) 

## 1.c filter potential problems ####

ORHRPhrase4 <-measuresConfint  %>% 
  mutate(
         problemConfint =  ifelse(!is.na(pValue),0,
                                  ifelse(measureValue2>CiUp2 |measureValue2<CiLow2, 1, 0) ) 
         )  %>% 
  filter( problemConfint == 0 ) %>% 
  mutate(result = ifelse(is.na(pValue), 
                         paste0(measureValue2, " [", CiLow2, ";", CiUp2,"]"),
                         paste0(measureValue2," ",pValue)
                         ) ) %>% 
  select(doc_id, PMID, measureType, measureValue2, CiLow2, CiUp2, result, pValue) 



## 1.e questions Exposure Outcome for trained Bert ####


orHrRrBert <- ORHRPhrase4 %>%  
  left_join(select( data,PMID,TITLE, ABSTRACT ), by = "PMID"   ) %>% 
  mutate(questExposure =  paste0("Which exposure, risk factor or diagnostic factor is associated with the ", measureType," :", result, " ?"),
         questOutcome = ifelse(str_detect(measureType, "HR|RR"),
                               paste0("The risk or odds for which disease, endpoint, outcome or event are associated with the ", measureType, ":", result, "?" ),
                               paste0("The risk or odds for which disease, endpoint, outcome or event are associated with the ", measureType, ":", result, "?" )
                               ),
         questReference = paste0("The odds or risk expressed in the ",measureType,": ",result,
                                 " are expressed in comparison to which reference group ? if you dont know return: ANSWERNOTFOUND"
         ),
         context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT, " .Other: ANSWERNOTFOUND")
  ) %>% 
  select( doc_id,PMID,result,measureType, context, questExposure, questOutcome, questReference) %>% 
  mutate(questionType = "ORHRExpoOutRef")

### 1.2 export ORHR For Bert ####


feather::write_feather(orHrRrBert, paste0("1_toPythonDB/1_ORHRBert/ORHRBert",projectName,".feather") )



### 1.3 Export Full DataFrame ORHR ####

ORHRRR2 <- ORHRPhrase4 %>% 
  left_join(select( data,PMID,TITLE, ABSTRACT), by = "PMID"   ) %>% 
  select(doc_id, PMID,  measureType,measureValue2, CiLow2, CiUp2, result )

saveRDS(ORHRRR2,paste0("2_FullDataFrameORHRRR/ORHRRR2",projectName,".rds" ) )





# orHrRrBert %>% distinct(PMID) %>% nrow()
# pValsBert %>% distinct(PMID) %>% nrow()
# pValORHRBert %>% distinct(PMID) %>% nrow()

# 3: N, population ####

## 3.1: population Bert ####

idsTreated <- orHrRrBert  %>% select(PMID) %>% distinct(PMID)

idsTrials <- data %>% 
  filter(str_detect(tolower(studyPlan), "trial")) %>% 
  select(PMID) %>% distinct(PMID)

idPop <- idsTreated %>% 
  rbind(idsTrials) %>% 
  distinct(PMID)


popInfos <-  data %>% 
 # filter(PMID %in% idPop $PMID) %>% 
  mutate(trialYN = ifelse(str_detect(studyPlan, "trial"), 1, 0)) %>% 
  select(PMID, TITLE,ABSTRACT, trialYN) %>% 
  mutate(questParticipants = "How many people or patients participated or were included in this study?",
         questCases = paste0("How many people or patients are in the cases or treatment or intervention group in this study? if you don't know return: ANSWERNOTFOUND."),
         questControls = paste0("How many people or patients are in the control or placebo group in this study? if you don't know return: ANSWERNOTFOUND."),
         
         questPopDiseaseTitle = "In this scientific study, what is the main disease, pathology or condition affecting the population?",
         questPopDiseaseAbstract = "What are the main diseases or medical conditions affecting the people included in the study and not being mentioned in the study title?",
         context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT, " .Other: ANSWERNOTFOUND"),
         contextTitle = paste0("STUDY TITLE:",TITLE, " .Other: ANSWERNOTFOUND"),
         )  %>% 
  select(PMID, context,contextTitle, questParticipants, questPopDiseaseTitle , questPopDiseaseAbstract, questCases, questControls )




feather::write_feather(popInfos, paste0("1_toPythonDB/2_PopDataForBert/popInfos",projectName,".feather") )



## 4: Trial infos

trialsInfos <- data %>% 
  filter(str_detect(tolower(studyPlan), "trial")) %>% 
  select(PMID, TITLE, ABSTRACT) %>% 
  filter(!is.na(ABSTRACT)) %>% 
  mutate(
    context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT, " .Other: ANSWERNOTFOUND"),
    questIntervention = paste0("In this clinical trial what is the main exposure, treatment or intervention that is being tested ?"),
    questEndpoint = paste0("In this clinical trial what is the main endpoint or outcome?"),
    
  )


feather::write_feather(trialsInfos , paste0("1_toPythonDB/3_trialInfosForBert/trialInfos",projectName,".feather") )

