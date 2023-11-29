
projectName <- "allChronicFatigue"



library(rstudioapi)
library(quanteda) # for stopwords function

library(dplyr)
library(tidyverse)
library(stringr)

library(readxl)
library(feather) # to Export data in feather format
`%notin%` <- Negate(`%in%`)


currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("forPythonCreateQuestionsV3.R")

setwd(currentPath2) 

data <- readRDS(paste0("../2B_RegexInfos/treatedDB/",projectName,".rds")) %>% 
  mutate(PMID = str_trim(PMID))

# 1: OR HR RR ####

## 1.a get all rows with OR HR RR ####

ORHR <- data %>% 
  filter(!is.na(PMID) | !is.na(ABSTRACT) ) %>% 
  filter(containsOR ==TRUE | containsHR == TRUE| containsRR == TRUE) %>% 
  mutate(doc_id = as.character(PMID)) %>% 
  arrange(PMID)


corp <- corpus(ORHR, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)



ORHRPhrase <-  kwic(toks, pattern =   c("\\bOR\\b","\\bHR\\b","\\bRR\\b",
                                          phrase("hazard ratio"), phrase("odd ratio"), phrase("relative risk")),valuetype = "regex",
               window = 95, case_insensitive = FALSE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>% 
  mutate(reducedPost = str_extract(post,".{20,250}"),
         contextORHR = paste0(pre, keyword, reducedPost)) %>% 
  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank,"_",keyword),
         doc_id = str_remove_all(doc_id," ")) %>% 
  ungroup() %>% 
  select(doc_id, PMID, keyword, pre, post,contextORHR)  




ORHRPhrase2 <- ORHRPhrase %>%

  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(
    measureType2 = str_remove_all(keyword, "(?<=(OR|HR|RR)).*"),
    measureType2 = str_remove_all(measureType2, ".*(?=(OR|HR|RR))"),
    measureType = ifelse(measureType2 == "OR" | measureType2 == "odd ratio", paste0("odd ratio (OR)"),
                         ifelse(measureType2=="HR" | measureType2 == "hazard ratio",paste0("hazard ratio (HR)"),
                                                          paste0("relative risk (RR)"))) 
    ) 
  


## 1.b get value and CI ####

ORHRPhrase3 <- ORHRPhrase2 %>% ungroup() %>% 
  mutate(post2 = post,
         post2 = str_remove_all(post2, "(95\\s?%|95\\s?percent)"),
         measureValue = str_extract(post2, "(?<=.{0,2})(\\d+(\\.\\d+)?)"),
         CiLow = ifelse(str_detect(post, paste0(measureValue,".*(\\(|IC 95|95\\s?%|CI\\s?95|CI|IC|interval)")) |str_detect(post,paste0("(IC 95|95\\s?%|CI\\s?95|CI|IC|interval).*",measureValue)),
                        str_extract(post2, paste0("(?<=",measureValue,").*?(\\d+(\\.\\d+)?)")), NA),
         # CiLow = str_extract(post, "(?:IC 95|95\\s?%|CI\\s?95|CI|IC|interval).*?(\\d+(\\.\\d+)?)"),
         CiLow = str_remove(CiLow, "(95 %|95%|CI\\s?95)"),
         CiLow = str_remove_all(CiLow, "[^\\d.]+"),
         CiUp = str_extract(post, paste0("(?<=", CiLow, ").{10}")),
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


## 1.d questions Exposure for Llama ####




ORHRRRLLAMA <- ORHRPhrase4 %>%  
  left_join(select( data,PMID,TITLE, ABSTRACT ), by = "PMID"   ) %>% 
  mutate(questExposure =  paste0("Which exposure, risk factor or diagnostic factor is associated with the ", measureType," :", result, " ?"),
         contextLLAMAExposure = paste0("STUDY TITLE:",TITLE,
                                       ". CONTEXT:" , contextORHR, 
                                       ". QUESTION: ", questExposure
                                     ),
         contextORHR = paste0("STUDY TITLE:",TITLE,"CONTEXT: ", contextORHR)
         ) %>% 
  select(doc_id, PMID,result,measureType, contextORHR, contextLLAMAExposure)

### 1.1 export ORHR For LLAMA ####


write_feather(ORHRRRLLAMA, 
              paste0("1_toPythonDB/1_ORHRExposureForLlama/ORHRRRLLAMAExposure", projectName, ".feather"))


### 1.1 Export Full DataFrame ORHR ####

ORHRRR2 <- ORHRPhrase4 %>% 
  left_join(select( data,PMID,TITLE, ABSTRACT), by = "PMID"   ) %>% 
  select(doc_id, PMID,  measureType,measureValue2, CiLow2, CiUp2, result )

saveRDS(ORHRRR2,paste0("2_FullDataFrameORHRRR/ORHRRR2",projectName,".rds" ) )

# 2: prevalence Incidence ####

dataPrev <- data %>% select(PMID, ABSTRACT) %>% 
  mutate(containsIncidence = str_detect(tolower(ABSTRACT), "incidence" ),
         containsPrevalence = str_detect(tolower(ABSTRACT), "prevalence" ),
         containsFrequency = str_detect(tolower(ABSTRACT), "frequency" ),
         ) %>% 
  filter(containsIncidence==TRUE | containsPrevalence == TRUE | containsFrequency == TRUE) 

percent <- dataPrev %>% 
  filter(!is.na(PMID) | !is.na(ABSTRACT) ) %>% 
  mutate(doc_id = as.character(PMID) ) %>% 
  filter(grepl("%", ABSTRACT)) %>% 
  arrange(PMID)


corp <- corpus(percent, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)

freqs <- kwic(toks, pattern =   c(phrase("\\d+ \\%") ),valuetype = "regex",
              window = 60, case_insensitive = TRUE) %>%
  rename("PMID"  = 1) %>% as.data.frame()



### create a full phrase with results and a ranked ID for each phrase inside a study

frequencies <- freqs %>% mutate(
  # pre = tolower(pre),
  # post= tolower(post),
  fullPhrase = paste(pre,keyword,post)) %>% 
  group_by(PMID) %>% arrange(PMID) %>% 
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank) ) %>% 
  mutate(shortPost = str_remove(post, "(?<!\\d)\\.(?!\\d).*?(?=(\\.\\d|\\.$|$))"),
         shortPre = str_extract(pre, "(?<= \\.\\s?).[:alpha:].*"),
         shortFullPhrase = paste(shortPre,keyword,shortPost))


## 2.1 filter CI freqs and doubles ####

frequencies2 <- frequencies   %>%  ### some sentence could have been created in double because prevalence is mentioned twice
  select(doc_id, PMID,shortPre,  keyword, shortPost, shortFullPhrase, fullPhrase ) %>% 
  group_by(PMID) %>% 
  distinct(keyword, .keep_all = TRUE) %>%  ## we filter here the doubles
  ungroup() %>% 
  mutate(isKeyword95CI =  ifelse(tolower(str_extract(shortPost,"..") ) == "ci", "yes",
                                 ifelse(tolower(str_extract(shortPost,"....") ) == "conf", "yes",
                                        ifelse(str_detect(tolower(shortFullPhrase),paste0("ci\\s?", keyword))   ,"yes", "no") ) ) )  %>%    ## filter percentages that are confidence intervals
  filter(isKeyword95CI == "no" | is.na(isKeyword95CI))%>% 
  mutate(
    isKeyCILow1 = str_detect( tolower( shortFullPhrase), paste0("(\\bci\\b|\\binterval\\b)\\s*", keyword)),
    isKeyCILow2 = str_detect( tolower(shortFullPhrase), paste0("(\\bci\\b|\\binterval\\b).{1,20}", keyword,".{1,20}\\)")),  # excepting the parenthesis from the match
    isKeyCILow3 = ifelse(str_detect( tolower(keyword), "\\-"), str_detect(tolower(shortFullPhrase), paste0("(\\bci\\b|\\binterval\\b).{1,20}", keyword) ),FALSE ),
    keyword2 = ifelse(str_detect(keyword, "\\-"), str_extract(shortFullPhrase, paste0( "\\d+?\\.?\\d+\\s?\\%\\s?",keyword) ), keyword )
    ) %>% 
  filter(!is.na(keyword2)) %>% 
  filter(isKeyCILow1 == FALSE & isKeyCILow2 == FALSE &  isKeyCILow3 == FALSE) %>% 
  select(doc_id,PMID, keyword2,shortPost, shortFullPhrase, fullPhrase ) %>% 
  left_join(select(data, TITLE, ABSTRACT,PMID)) %>% 
  ungroup() %>% 
  mutate(keywordDash = ifelse(str_detect(shortPost,"^\\s?\\-\\s?\\d+\\.?\\d+?"), paste0(keyword2,str_extract(shortPost,"^\\s?\\-\\s?\\d+\\.?\\d+?\\s?\\%?" )), NA ),
        keywordTo = ifelse(str_detect(shortPost,"^\\s?to\\s?\\d+\\.?\\d+?"), paste0(keyword2,str_extract(shortPost,"^\\s?to\\s?\\d+\\.?\\d+?\\s?\\%?")), NA ),
        confInt = str_extract(shortPost, "(^\\s?(\\(|\\[)\\s??95\\s?%.{35})|^(\\s?\\,?95\\s?%.{35})" ),
        keywordFinal = ifelse(!is.na(keywordDash), keywordDash,
                              ifelse(!is.na(keywordTo), keywordTo, keyword2)) 
        )


## 2.2 filter comparaison of frequencies ####


Vss <- c("\\bv\\b","\\bV\\b","\\bvs\\b","\\bVs\\b","\\bVS\\b","\\bversus\\b",
         "compared to", "compared with")
pasteVss <- paste0(Vss, collapse = "|")

frequencies3 <- frequencies2 %>% 
  mutate(
    isVS = str_detect(shortFullPhrase, paste0(keywordFinal, "\\s*(",pasteVss,")"))
  ) %>% 
  filter(isVS == FALSE)

## 2.3 filter freq in range ####

## frequencies are expressed in a Range (difference in formulation where increases or decreases (talking about subgroups) and ranging from to giving sort of confint)

frequencies4 <- frequencies3 %>% 
  mutate(freqIsRange = str_detect(shortFullPhrase, paste0("from\\s*",keywordFinal) )
  ) 
  # filter(freqIsRange == FALSE)   ## to eliminate the doubles rows of the range

## 2.4 filter freq is Confint value ####

frequencies5 <- frequencies4 %>% 
  mutate(
    freqIsCIValue2 = str_detect(tolower(shortFullPhrase), paste0(keyword2, "\\s?\\-") ),
    freqIsCIValue3 = str_detect(tolower(shortFullPhrase), paste0("\\-\\s?",keyword2 ) )
  ) %>% 
  filter(freqIsCIValue2==FALSE)

frequencies6 <-  frequencies5 %>% 
  select(-c(isVS, contains('freq')) )


percentagesBert <- frequencies6 %>% 
  select(doc_id, PMID, TITLE, ABSTRACT,fullPhrase , keywordFinal, confInt, shortFullPhrase) %>% 
  left_join(select(data, keywords, OT, PMID), by = "PMID") %>% 
  mutate(Bertcontext = paste0("Study Title: ", TITLE, ". ABSTRACT: " , ABSTRACT, "keywords: ",
                          keywords, ", ",OT,"." ),
         BertShortContext = paste0("This text comes from the abstract of a medical study: TEXT: " , shortFullPhrase),
         questBertPercExposure = paste0(keywordFinal, " corresponds to the percentage of what? "),
         questPercGroup = paste0(keywordFinal, "corresponds to a frequency measure in a group defined by which characteristic?")
         ) %>% select(doc_id, PMID, keywordFinal,Bertcontext, BertShortContext, questBertPercExposure)

write_feather(percentagesBert, 
              paste0("1_toPythonDB/2_PrevalenceForBert/prevBert", projectName, ".feather"))


### 2.5 export full dataframe for app ####

percentagesPython2 <- frequencies6 %>% 
  select(doc_id, PMID, keywordFinal, keywordDash, keywordTo, confInt, shortFullPhrase)

saveRDS(percentagesPython2 , paste0("3_FullDataFramePrevalence/prevFull",projectName ,".rds"))



# 3: N, population ####

## 3.1: population Bert ####

idsFreq <- percentagesBert %>% select(PMID) %>% distinct(PMID)
idsORHR <- ORHRRRLLAMA %>% select(PMID) %>% distinct(PMID)

ids <- rbind(idsFreq, idsORHR) %>% distinct(PMID)

findNPython <-  data %>% 
  filter(PMID %in% ids$PMID) %>% 
  select(PMID, ABSTRACT) %>% 
  mutate(questParticipants = "How many people or patients participated or were included in this study? Here we only expect a numeric value, if you don't know leave it empty.",
         questPopDisease = "What is the main disease or medical condition affecting the people included in the study? If you don't know leave it empty.",
         questComorbidity =  "What is the main comorbidity affecting the participants or patients included in the study?. If you don't know leave it empty.",
         questAgeNumeric  = "What is the mean or median population age? If the information is not available write: information not available.",
         questWomen = "How many women were included in the study? If the information is not available write: information not available.",
         questMen = "How many men were included in the study? If the information is not available write: information not available.",
         )  %>% 
  mutate(shortContext = str_extract(ABSTRACT, ".{1,1000}"),
         questAgeClass  = paste0("CONTEXT: ",shortContext,". ANALYSIS: ", "from the context before mentioned I want to determine the age class of the study participants. The possible choices are: children, adult,  elderly, mixed, information not available. After reading I can say the age class in the given context is   "),
         questSex = paste0("CONTEXT: ",shortContext,". ANALYSIS: ", "from the context before mentioned I want to determine the sex of the study participants. The possible choices are: mostly men, mostly women, man and women, information not available. After reading I can say the sex of the participants is  "),
         
  )  %>% select(-c(shortContext))




write_feather(findNPython, paste0("1_toPythonDB/3_PopDataForBert/participants",projectName,".feather") )

## 3.2: population Llama ####


findNPythonLlama <-  data %>% 
  filter(PMID %in% ids$PMID) %>% 
  select(PMID, ABSTRACT) %>% 
  mutate(shortContext = str_extract(ABSTRACT, ".{1,1000}"),
        questAgeClass  = paste0("CONTEXT: ",shortContext,". ANALYSIS: ", "from the context before mentioned I want to determine the age class of the study participants. The possible choices are: children, adult,  elderly, mixed, information not available. After reading I can say the age class in the given context is   "),
         questSex = paste0("CONTEXT: ",shortContext,". ANALYSIS: ", "from the context before mentioned I want to determine the sex of the study participants. The possible choices are: mostly men, mostly women, man and women, information not available. After reading I can say the sex of the participants is  "),

         ) 




write_feather(findNPythonLlama, paste0("1_toPythonDB/3_PopDataForLlama/participantsLlama",projectName,".feather") )




