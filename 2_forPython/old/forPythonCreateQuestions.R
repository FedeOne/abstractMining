
projectName <- "projTutoApneaStridor"



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
  str_remove("forPythonCreateQuestions.R")

setwd(currentPath2) 

dataOri <- readRDS(paste0("../1_regexInfos/treatedDB/",projectName,".rds")) %>% 
  mutate(PMID = str_trim(PMID))

data <- dataOri %>% 
  mutate(containsHR = str_detect(ABSTRACT, "(\\bHR\\b)|(?i)hazard ratio"), ## ?i is for case insensitive
         containsOR = str_detect(ABSTRACT, "(\\bOR\\b)|(?i)odd ratio"),
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

ORHRPhrase <-  kwic(toks, pattern =   c("\\bOR\\b","\\bHR\\b","\\bRR\\b",
                                          phrase("hazard ratio"), phrase("odd ratio"), phrase("relative risk")),valuetype = "regex",
               window = 95, case_insensitive = FALSE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>% 
  mutate(measureId = paste0(PMID,"_", keyword)) %>% 
  mutate(reg = "ORHR")


### 1.a.2 kwic on numbers without text and filter the ones which are not above


db1<- ORHR %>%
  slice(1:round(length(ORHR$PMID)/2,0) )


corp1 <- corpus(db1, text_field = "ABSTRACT", docid_field = "doc_id")

toks1 <- tokens(corp1)


ORHRnum <- ORHR %>% 
  mutate(
    num = str_extract_all(ABSTRACT, "\\d(\\.\\d+)\\s(,|;|\\[)\\s\\d(\\.\\d+)\\s?(-|to|;)\\s?\\d(\\.\\d+)")
  ) %>% 
  select(PMID, ABSTRACT, num)


toksComp1 <- toks1 %>% ## we modify some tokens into group of tokens that we'll match later to speed up the process
  tokens_compound("\\d(\\.\\d+)\\s(,|;|\\[)\\s\\d(\\.\\d+)(-)\\d(\\.\\d+)", valuetype = "regex")

ORHRNum1 <- kwic(toks1, pattern = phrase(c("\\d(\\.\\d+) (,) \\d(\\.\\d+)(-)\\d(\\.\\d+)") ## remember that kwic matches freaking tokens, not sentences
                                  ),
                valuetype = "regex",
                window = 95, case_insensitive = FALSE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>%
  mutate(keyMeasure = str_extract(keyword, "^\\d(\\.\\d+)" )
  ) %>%
  mutate(measureId = paste0(PMID,"_", keyMeasure)) %>%
  filter(measureId %notin% ORHRPhrase$measureId) %>%
  select(-c(measureId, keyMeasure  ) ) %>%
  mutate(reg = "num")
# 
# db2<- ORHR %>% 
#   slice(round(length(ORHR$PMID)/2,0): length(ORHR$PMID) )
# 
# corp2 <- corpus(db2, text_field = "ABSTRACT", docid_field = "doc_id")
# 
# toks2 <- tokens(corp2)
# 
# 
# toksComp2 <- toks2 %>% ## we modify some tokens into group of tokens that we'll match later to speed up the process
#   tokens_compound(phrase("\\d(\\.\\d+) (,|\\[) \\d(\\.\\d+)(-|;)\\d(\\.\\d+)"), valuetype = "regex", concatenator = " " ) 
# 
# 
# ORHRNum2 <- kwic(toksComp2, pattern = c("\\d(\\.\\d+) (,|\\[) \\d(\\.\\d+)(-|;)\\d(\\.\\d+)" ## remember that kwic matches freaking tokens, not sentences
# ), 
# valuetype = "regex",
# window = 95, case_insensitive = FALSE) %>% 
#   rename("PMID"  = 1) %>% as.data.frame() %>% 
#   mutate(keyMeasure = str_extract(keyword, "^\\d(\\.\\d+)" ) 
#   ) %>% 
#   mutate(measureId = paste0(PMID,"_", keyMeasure)) %>% 
#   filter(measureId %notin% ORHRPhrase$measureId) %>% 
#   select(-c(measureId, keyMeasure  ) ) %>% 
#   mutate(reg = "num")
# 
# 
# 
# ORHRNum <- rbind(ORHRNUm1, ORHRNum2)

## this part will need updating when figuring out other ways of commonly presented results

### 1.a.2  join and mutate some

ORHRPhraseB <- ORHRPhrase %>% 
  select(-c(measureId)) %>% 
  # rbind(ORHRNum) %>% 
  mutate(reducedPost = str_extract(post,".{20,250}"),
         contextORHR = paste0(pre, " " , keyword, " " , reducedPost)) %>% 
  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank,"_OR"),
         doc_id = str_remove_all(doc_id," ")) %>% 
  ungroup() %>% 
  select(doc_id, PMID, keyword, pre, post,contextORHR, reg)  




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

ORHRPhrase3 <- ORHRPhrase2 %>% ungroup() %>% 
  mutate(post2 = post,
         post2 = str_remove_all(post2, "(95\\s?%|95\\s?percent)"),
         measureValue = ifelse(reg == "ORHR",
                               str_extract(post2, "(?<=.{0,2})(\\d+(\\.\\d+)?)") , 
                               str_extract(keyword,"^\\d+(\\.\\d+)?") ),
         CiLow = ifelse(reg == "ORHR", 
                        ifelse(str_detect(post, paste0(measureValue,".*(\\(|IC 95|95\\s?%|CI\\s?95|CI|IC|interval)")) |str_detect(post,paste0("(IC 95|95\\s?%|CI\\s?95|CI|IC|interval).*",measureValue)),
                        str_extract(post2, paste0("(?<=",measureValue,").*?(\\d+(\\.\\d+)?)")),NA),
                        str_extract(keyword, paste0("(?<=",measureValue,".{1,3})\\d+(\\.\\d+)") )
                         
         )
                             
                        ,
         # CiLow = str_extract(post, "(?:IC 95|95\\s?%|CI\\s?95|CI|IC|interval).*?(\\d+(\\.\\d+)?)"),
         CiLow = str_remove(CiLow, "(95 %|95%|CI\\s?95)"),
         CiLow = str_remove_all(CiLow, "[^\\d.]+"),
         CiUp = ifelse(reg == "ORHR", 
                       str_extract(post, paste0("(?<=", CiLow, ").{10}")),
                       str_extract(keyword, paste0("(?<=",CiLow,".{1,3})\\d+(\\.\\d+)") ) 
         ),
         CiUp = str_remove_all(CiUp, "[^\\d.]+"),
         CiUp = str_remove(CiUp, "\\.$"),
         CiUp2 = as.numeric(CiUp),
         CiLow2 = as.numeric(CiLow),
         measureValue2 = as.numeric(measureValue)
  ) %>% 
  mutate(idResult = paste0(PMID,"_",measureValue,"_",CiLow2,"_",CiUp2)) %>% 
  group_by(idResult) %>% 
  arrange(idResult) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c(idResult)) 

## 1.c filter potential problems ####

ORHRPhrase4 <- ORHRPhrase3 %>% 
  mutate(noGoodMeasure = ifelse(is.na(CiUp2) & !is.na(CiUp),1,0 ),
         problemConfint = ifelse(measureValue2>CiUp2 |measureValue2<CiLow2, 1, 0)) %>% 
  filter(noGoodMeasure ==0 & problemConfint == 0) %>% 
  mutate(result = paste0(measureValue2, " [", CiLow2, ";", CiUp2,"]")) %>% 
  select(doc_id, PMID, measureType, measureValue2, CiLow2, CiUp2, result, contextORHR) 



## 1.e questions Exposure Outcome for trained Bert ####


orHrRrBert <- ORHRPhrase4 %>%  
  left_join(select( data,PMID,TITLE, ABSTRACT ), by = "PMID"   ) %>% 
  mutate(questExposure =  paste0("Which exposure, risk factor or diagnostic factor is associated with the ", measureType," :", result, " ?"),
         contextORHR = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", contextORHR)
  ) %>% 
  select(doc_id,PMID,result,measureType, "context" = contextORHR, "question" = questExposure) %>% 
  mutate(questionType = "ORHRExpo")

### 1.2 export ORHR For Bert ####


# feather::write_feather(orHrRrBert, 
#               paste0("1_toPythonDB/1_pValORHRBert/orHrRrBertExpo", projectName, ".feather"))



### 1.3 Export Full DataFrame ORHR ####

ORHRRR2 <- ORHRPhrase4 %>% 
  left_join(select( data,PMID,TITLE, ABSTRACT), by = "PMID"   ) %>% 
  select(doc_id, PMID,  measureType,measureValue2, CiLow2, CiUp2, result )

saveRDS(ORHRRR2,paste0("2_FullDataFrameORHRRR/ORHRRR2",projectName,".rds" ) )

# 2: P values in clinical trials ####

## 2.1: select trials and check that mostly corresponds to PubMed ####

trials <- data %>% 
  filter(str_detect(tolower(studyPlan), "trial")) %>% 
  select(PMID, TITLE, ABSTRACT) %>% 
  filter(!is.na(ABSTRACT)) %>% 
  mutate(doc_id = as.character(PMID)) %>% 
  arrange(PMID)


corp2 <- corpus(trials, text_field = "ABSTRACT", docid_field = "doc_id")

toks2 <- tokens(corp2)


## 2.2 kwic, regex to add ####

pVals <-  kwic(toks2, pattern =   c(
          phrase("(\\bp\\b|\\bP\\b)")
         # phrase("(\\bp\\b|\\bP\\b) . \\.")
        ),
        valuetype = "regex",
        window = 95, case_insensitive = FALSE) %>% 
          rename("PMID"  = 1) %>% 
  as.data.frame() %>% 
  mutate(value = str_extract(post, "^.{1,10}(0|\\.)\\s?\\d+"),
         value = ifelse(str_detect(value, "(:|\\[|;)" ), NA, value)
  ) %>% 
  filter(!is.na(value))
  
           
         



## 2.3: correct pvalues ####

# otherPmids <- trials %>% filter(PMID %notin% pVals$PMID) %>% 
#   select(PMID, TITLE, ABSTRACT)

pVals2 <- pVals %>% 
  mutate(keyword = paste0(keyword, value),
         keyword = str_remove_all(keyword, "NA"),
           keyword = str_trim(keyword)
         ) %>% 
  select(-c(value)) %>% 
  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank,"_pVal"),
         doc_id = str_remove_all(doc_id," ")) %>% 
  ungroup() 


## 2.4: Create questions ####

pValsBert <-  pVals2 %>% 
  left_join(select(trials, PMID, TITLE, ABSTRACT ), by = "PMID" ) %>% 
  # full_join(otherPmids)  %>% 
  mutate(question =  ifelse(!is.na(keyword), paste0("Which treatment or intervention is related to the p value: ", keyword," ?"),
                            paste0("What is the main treatment, intervention or exposure that is being tested in this clinical trial?") 
                            ),
         context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT),
         result = keyword,
         measureType = "pValue",
         ) %>% 
  select( doc_id,PMID,result,measureType, context, question) %>% 
  mutate(questionType = "pValExpo")


pValORHRBert <-  orHrRrBert %>% 
  rbind(pValsBert)

feather::write_feather(pValORHRBert, paste0("1_toPythonDB/1_pValORHRBert/pValORHRBert",projectName,".feather") )

# orHrRrBert %>% distinct(PMID) %>% nrow()
# pValsBert %>% distinct(PMID) %>% nrow()
# pValORHRBert %>% distinct(PMID) %>% nrow()

# 3: N, population ####

## 3.1: population Bert ####

idsTreated <- pValORHRBert  %>% select(PMID) %>% distinct(PMID)


popInfos <-  data %>% 
  filter(PMID %in% idsTreated$PMID) %>% 
  mutate(trialYN = ifelse(str_detect(studyPlan, "trial"), 1, 0)) %>% 
  select(PMID, TITLE,ABSTRACT, trialYN) %>% 
  mutate(questParticipants = "How many people or patients participated or were included in this study? Here we only expect a numeric value, if you don't know leave it empty.",
         questPopDisease = "What are the main diseases or medical conditions affecting the people included in the study? If you don't know leave it empty.",
         questCases = ifelse(trialYN == 1, paste0("How many people or patients are in the cases or treatment or intervention group in this trial?"), NA ),
         questControls = ifelse(trialYN == 1, paste0("How many people or patients are in the placebo or control group in this trial?"), NA ),
         context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT),
         )  %>% 
  select(PMID, context, questParticipants, questPopDisease , questCases, questControls )




feather::write_feather(popInfos, paste0("1_toPythonDB/2_PopDataForBert/popInfos",projectName,".feather") )



## 4: Trial infos

trialsInfos <- data %>% 
  filter(str_detect(tolower(studyPlan), "trial")) %>% 
  select(PMID, TITLE, ABSTRACT) %>% 
  filter(!is.na(ABSTRACT)) %>% 
  mutate(
    context = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", ABSTRACT),
    questIntervention = paste0("In this clinical trial what is the main treatment or intervention that is being tested ?"),
    questEndpoint = paste0("In this clinical trial what is the main endpoint or outcome?")
  )


feather::write_feather(trialsInfos , paste0("1_toPythonDB/3_trialInfosForBert/trialInfos",projectName,".feather") )

