
projectName <- "allChronicFatigue"


library(rstudioapi)
library(quanteda) # for stopwords function
library(dplyr)
library(tidyverse)
library(stringr)
library(readxl)

library(doParallel)
library(foreach)


currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("createNewVariablesRegex.R")

setwd(currentPath2) 

data  <-  readRDS(paste0("../2_dataManageFromPmed/2a_Rdatabases/",projectName,".rds")) %>% 
  filter(!is.na(PMID))


# 1. Author for citation ####

columns <- data %>% select(contains("auth")) %>% 
  select(-c(authorn)) %>% 
  colnames()

# Count non-NA cells across specific columns
data$authorNumber <- rowSums(!is.na(data[, columns]))

data2 <- data %>% 
  mutate(AUTHOR =  ifelse(authorNumber == 1, paste0(author1,"."),
                          ifelse(authorNumber == 2, paste0(author1,". and ", authorn,"."),
                                 paste0(author1, ". et al." ))),
         AuthorYear = paste(AUTHOR, publicationYear)
  ) %>% 
  select(-c(author1,author2,author3, author4, author5, author6, authorn) )


# 2: country ####


countrylist2 <- read_xlsx("countryNationality.xlsx") %>% 
  mutate(country = tolower(country),
         nationality = tolower(nationality))

countryRegexLow <- paste0(countrylist2$country, collapse = "|")
nationalityRegexLow <- paste0(countrylist2$nationality, collapse = "|")

dataCountry <- data2 %>% 
  select(PMID, TITLE,ABSTRACT, OT, keywords, studyPlan) %>% 
  mutate(textCountry = tolower(paste0(TITLE,", ",ABSTRACT," ,", OT, ", ", keywords)) ) %>% 
  ungroup() %>% 
  mutate(country1 = str_extract_all(textCountry, countryRegexLow),
         country2 = str_extract_all(textCountry, nationalityRegexLow),
         rowID = row_number())  %>% 
  group_by(rowID) %>% 
  mutate(country3 = paste0(unique(unlist(country1 ) ),collapse = ", " ),
         country4 = paste0(unique(unlist(country2 ) ),collapse = ", " ) 
         ) %>% 
  ungroup()

dataCountry2 <- dataCountry %>% 
  select(PMID, country3, "nationality" =   country4) %>% 
  left_join(countrylist2, by = "nationality") %>% 
  mutate(COUNTRY = ifelse(country3 =="", country,country3),
         COUNTRY = ifelse(is.na(COUNTRY), nationality, COUNTRY)) %>% 
  select(PMID, "country" = COUNTRY)
  

data3 <- data2 %>% 
  left_join(select(dataCountry2 ,PMID, country))

# 3: Study Objective ####


studyObjective <- data3 %>% 
  select(PMID, ABSTRACT,TITLE) %>% 
  filter(!is.na(ABSTRACT)) %>% 
  mutate(doc_id = as.character(PMID) )

corp <- corpus(studyObjective, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)

## Objectives

kw_object <- kwic(toks, pattern =  c("objectiv*", "aim*", "scope",
                                     phrase("we investigated"),"purpose*",
                                     phrase("study was designed to"),
                                     phrase("this article reviews"),
                                     phrase("we studied"),
                                     phrase("this study reviews"),phrase("we review"),phrase("we sought to"),
                                     phrase("not understood"), phrase("not weel understood"), 
                                     phrase("is unknown"),phrase("little is known"),
                                     phrase("to investigate")), window = 50) %>% as.data.frame()


kw_object2 <- kw_object %>% 
  mutate(pre2 = str_remove_all(pre, ".*(\\.|\\:)"),
         post2 = str_remove(post, "(\\.)(.*)"),
         studyObjective = paste(pre2, keyword, post2))


kw_object3 <- kw_object2 %>% group_by(docname) %>% slice(1) %>% select( "PMID" = docname, studyObjective)

data4 <- data3 %>% left_join(kw_object3)


# 4: Definitions ####


corpdefinition <- corpus(data4, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokenize_sentence(corpdefinition) %>% tokens()

define<- kwic(toks, pattern =   c(phrase("was defined"), 
                                  phrase("defined as"),
                                  phrase("defined by")) ,valuetype = "regex",
              window = 1, case_insensitive = TRUE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>% 
  group_by(PMID) %>% 
  slice(1) %>% 
  ungroup() %>% select(PMID, keyword) %>% 
  rename( "definition" = keyword)

data5 <- data4 %>% 
  left_join( select(define, definition, PMID) )

# 5: Cohort and RCT and meta ####

cohort <- dataCountry %>% 
  select(PMID, textCountry, studyPlan) %>% 
  mutate(cohortYN =  str_detect(textCountry, "(cohort|Cohort|COHORT)"),
         clinicalTrialYN =  str_detect(tolower(textCountry), "(clinical trial|\\brct\\b|trial)"),
         metaAnalysisYN =  str_detect(tolower(textCountry), "(meta analysis|meta-analysis)"),
         literatureReviewYN = str_detect(tolower(textCountry), "(review|Review)"),
         studyPlan = ifelse(studyPlan == "other" & metaAnalysisYN == TRUE, "meta analysis",
                             ifelse(studyPlan == "other" & clinicalTrialYN == TRUE, "clinical trial",
                                    ifelse(studyPlan == "other" & cohortYN == TRUE, "cohort", 
                                           ifelse(studyPlan == "other" & literatureReviewYN == TRUE, "review" , 
                                                  studyPlan ))))
         ) %>% select(-c(textCountry) ) 


data6 <- data5 %>% 
  left_join(cohort)


# 6: Acronyms ####

acronyms <- data %>% 
  select(PMID, ABSTRACT) %>% 
  group_by(PMID) %>% 
  mutate(ABSTRACT2 = ABSTRACT,
         ABSTRACT2 = str_replace_all(ABSTRACT2, pattern = "-|/", replacement = " "),
         ABSTRACT2 = str_remove_all(ABSTRACT2, pattern = "'"),
         acronymsParent = str_extract_all(ABSTRACT2,"\\(\\s?[A-Z]+\\s?\\)"),
         acro1 = acronymsParent [[1]][1],
         acro2 = acronymsParent [[1]][2],
         acro3 = acronymsParent [[1]][3],
         acro4 = acronymsParent [[1]][4] ,
         acro5 = acronymsParent [[1]][5] ,
         acro6 = acronymsParent [[1]][6] ,
         acro7 = acronymsParent [[1]][7] ,
         lengthacro1 = ifelse(is.na(acro1),0, str_count(str_remove_all(acro1, "\\(|\\)| " ) ) ),
         lengthacro2 = ifelse(is.na(acro2),0, str_count(str_remove_all(acro2, "\\(|\\)| " ) ) ),
         lengthacro3 = ifelse(is.na(acro3),0, str_count(str_remove_all(acro3, "\\(|\\)| " ) ) ),
         lengthacro4 = ifelse(is.na(acro4),0, str_count(str_remove_all(acro4, "\\(|\\)| " ) ) ),
         lengthacro5 = ifelse(is.na(acro5),0, str_count(str_remove_all(acro5, "\\(|\\)| " ) ) ),
         lengthacro6 = ifelse(is.na(acro6),0, str_count(str_remove_all(acro6, "\\(|\\)| " ) ) ),
         lengthacro7 = ifelse(is.na(acro7),0, str_count(str_remove_all(acro7, "\\(|\\)| " ) ) ),
         acroComp1 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro1,"}\\(" ,acro1) ),
         acroComp2 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro2,"}\\(" ,acro2) ),
         acroComp3 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro3,"}\\(" ,acro3) ),
         acroComp4 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro4,"}\\(" ,acro4) ),
         acroComp5 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro5,"}\\(" ,acro5) ),
         acroComp6 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro6,"}\\(" ,acro6) ),
         acroComp7 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro7,"}\\(" ,acro7) ),
         acroComp1 = tolower(str_remove(acroComp1, "\\(.*") ),
         acroComp2 = tolower(str_remove(acroComp2, "\\(.*") ),
         acroComp3 = tolower(str_remove(acroComp3, "\\(.*") ),
         acroComp4 = tolower(str_remove(acroComp4, "\\(.*") ),
         acroComp5 = tolower(str_remove(acroComp5, "\\(.*") ),
         acroComp6 = tolower(str_remove(acroComp6, "\\(.*") ),
         acroComp7 = tolower(str_remove(acroComp7, "\\(.*") ),
         acroComp1 = str_trim(tolower(str_remove(acroComp1, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp2 = str_trim(tolower(str_remove(acroComp2, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp3 = str_trim(tolower(str_remove(acroComp3, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp4 = str_trim(tolower(str_remove(acroComp4, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp5 = str_trim(tolower(str_remove(acroComp5, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp6 = str_trim(tolower(str_remove(acroComp6, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp7 = str_trim(tolower(str_remove(acroComp7, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) )
  )

acronyms2 <- acronyms %>% 
  select(PMID,
         acro1,acro2,acro3,acro4,acro5,acro6,acro7,
         acroComp1,acroComp2,acroComp3,acroComp4,acroComp5,acroComp6,acroComp7) 

data7 <- data6 %>% 
  left_join(acronyms2, by = "PMID")


# 7: population age ####

SDExtract <- c(
  "(?<=\\+\\-.{1,5})\\d+\\.?\\d+?",
  "(?<=\\+\\/\\-.{1,5})\\d+\\.?\\d+?",
  "(?<=sd\\s?=?\\s?)\\d+\\.?\\d+?"
)

SDExtractRegex <- paste0(SDExtract, collapse = "|")

meanAgeExtract <- c(
  "(?<=mean age.{1,20})\\d+\\.?\\d+?",
  "(?<=mean age.{1,20}was\\s?)\\d+\\.?\\d+?",
  "(?<=mean.{1,4}sd.{1,4}age\\s?)\\d+\\.?\\d+?",
  "\\d+\\.?\\d+?(?=\\s?\\+\\/?\\-.{1,5}year)",
  "(?<=mean age and standard deviation )\\d+\\.?\\d+?"
)

meanAgeExtractRegex <- paste0(meanAgeExtract, collapse = "|")


medianAgeExtract <- c(
  "(?<=median age.{1,20})\\d+\\.?\\d+?",
  "(?<=median age.{1,20}was\\s?)\\d+\\.?\\d+?",
  "\\d+\\.?\\d+?(?=\\s?\\+\\/?\\-.{1,5}year)"
)

medianAgeExtractRegex <- paste0(medianAgeExtract, collapse = "|")

studyMeanAge <- data7 %>% 
  select(PMID, ABSTRACT) %>% 
  filter(!is.na(ABSTRACT)) %>% 
  mutate(doc_id = as.character(PMID) )


corpdefinition <- corpus(studyMeanAge, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokenize_sentence(corpdefinition) %>% tokens()

meanAge <- kwic(toks, pattern =   c("median age\\b",
                                    "mean age\\b",
                                    "aged \\d+",
                                    "mean.{1,4}sd.{1,4}age") ,
                valuetype = "regex",
              window = 1, case_insensitive = TRUE) %>%
  as.data.frame() %>% 
  rename("PMID" = 1) %>% 
  group_by(PMID) %>% slice(1) %>% 
  select(PMID, keyword, pattern ) 

meanAge2 <- meanAge %>% 
  mutate(keyword = tolower(keyword),
         ageMeasure = ifelse(str_detect(pattern, "mean"), "mean", 
                             ifelse(str_detect(pattern, "median"), "median","range" )),
         ageUnit = ifelse(str_detect(keyword, "year"), "years",
                          ifelse(str_detect(keyword, "month"), "months",
                                 ifelse(str_detect(keyword, "day"), "days",NA))),
         ageSD = ifelse(ageMeasure == "mean", str_extract(keyword, SDExtractRegex), NA),
         ageMean = ifelse(ageMeasure == "mean", str_extract(keyword, meanAgeExtractRegex), NA),
         ageMedian = ifelse(ageMeasure == "median", str_extract(keyword, medianAgeExtractRegex), ageMean),
         ageRange1 = str_extract(keyword, "(?<=years.{1,15}range.{1,5}) \\d+(\\s?to\\s?|\\s?\\-\\s?)\\d+"),
         ageRange2 = ifelse(ageMeasure == "range", str_extract(keyword,"(?<=aged.{1,15})\\d+(.{1,15}to\\s?|\\s?\\-\\s?)\\d+"),"bla" ),
         ageRange =ifelse(is.na(ageRange2), ageRange1, ageRange2),
         ageRangeLow = str_extract(ageRange, "^\\d+") %>% as.numeric(),
         ageRangeUp = str_extract(ageRange, "\\d+$") %>% as.numeric(),
         ageValue = ifelse(!is.na(ageMean),ageMean,ageMedian),
         ageValue = as.numeric(ageValue),
         ageSD = as.numeric(ageSD),
         ageLow = ifelse(!is.na(ageSD), ageValue - 1.96*ageSD, ageRangeLow),
         ageUp = ifelse(!is.na(ageSD), ageValue + 1.96*ageSD, ageRangeUp),
  ) %>% 
  select(PMID, keyword,ageMeasure,ageValue, ageUnit, ageSD, ageLow,ageUp)

data8 <- data7 %>% 
  left_join(meanAge2, by = "PMID")


# 11: Diseases Title and Abstract ####


library(dplyr)
library(stringr)
library(readxl)

commonDiseases <- readRDS("commonDiseases.rds") 
rareDiseases <- readRDS("rareDiseases.rds") %>% select(disease) 
disChatGpt <- read_xlsx("diseasesChatGpt.xlsx")

diseaseList <- rbind(commonDiseases, rareDiseases) %>% 
  rbind(disChatGpt) %>% 
  mutate(disease = tolower(disease),
         disease = str_replace_all(disease, "/|-", " "),
         disease = str_replace_all(disease, "\\s+", " "),
         disease = str_replace_all(disease, "\\(.*\\)", replacement = ""),
         str_trim(disease),
         disease =   str_replace_all(disease, " ", "\\\\b \\\\b"),
         # disease2 = paste0("\\\\b",  disease2, "\\\\b" )
  ) %>% 
  filter(nchar(disease) > 3 ) %>% arrange(disease) %>% 
  group_by(disease) %>% slice(1) %>% 
  filter(!grepl("clinical trials", disease))



diseaseRegex <- paste0(diseaseList$disease, collapse = "\\b|\\b")



disease <- data %>% 
  select(PMID,TITLE, ABSTRACT) %>% 
  mutate(
    TITLE = tolower(TITLE),
    TITLE = str_replace_all(TITLE, "-", " ")
  ) 


cl <- makeCluster(12)
registerDoParallel(cl)


res <- foreach(i = 1:nrow(disease),
               .combine = rbind,
               .packages = c("dplyr", "stringr")) %dopar% {
                 diseaseTitle <- str_extract(disease$TITLE[i], diseaseRegex)
                 data.frame(diseaseTitle)
               }

disease2 <- disease %>%  cbind(res)

stopCluster(cl)  #

cl <- makeCluster(12)
registerDoParallel(cl)


res2 <- foreach(i = 1:nrow(disease), 
                .packages = c("dplyr", "stringr")) %dopar% {
                  diseaseAbstract <- str_extract_all(disease$ABSTRACT[i], diseaseRegex)
                  return(diseaseAbstract)
                }


for (z in 1: length(res2)) {
  disease2$diseaseAbstract[z] <-  paste0(res2[[z]], collapse = ", ")
  
}

stopCluster(cl)  #

disease3 <- disease2 %>% 
  mutate(diseaseAbstract2 = str_remove_all(diseaseAbstract, '(c\\(\\")|(\\"\\))|(\\")'),
         )

# for (i in 1:length(disease3$diseaseAbstract2)) {
#   disease3$diseaseAbstract3[i] <- unique(unlist(strsplit(disease3$diseaseAbstract2[i], ", ")))
# }



disease4 <- disease3 %>%
  mutate(diseaseAbstract3 = strsplit(diseaseAbstract2, ", ")) %>%
  unnest(diseaseAbstract3) %>%
  group_by(PMID) %>%
  mutate(variable = paste0("Var_", row_number())) %>%
  pivot_wider(names_from = variable, values_from = diseaseAbstract3) %>% 
  ungroup() %>% 
  mutate(across(c("Var_2","Var_3","Var_4","Var_5","Var_6","Var_7","Var_8","Var_9","Var_10"), ~ ifelse(str_detect(.,Var_1), NA, .) ) ) %>% 
  mutate(across(c("Var_3","Var_4","Var_5","Var_6","Var_7","Var_8","Var_9","Var_10"), ~ ifelse(str_detect(.,Var_2), NA, .) ) ) %>% 
  mutate(across(c("Var_4","Var_5","Var_6","Var_7","Var_8","Var_9","Var_10"), ~ ifelse(str_detect(.,Var_3), NA, .) ) ) %>% 
  mutate(across(c("Var_1", "Var_2","Var_3","Var_4","Var_5","Var_6","Var_7","Var_8","Var_9","Var_10"), ~ ifelse(str_detect(.,diseaseTitle), NA, .) ) ) %>% 
  select(-c(diseaseAbstract)) %>% 
  unite(diseaseAbstract, c("Var_1","Var_2","Var_3","Var_4","Var_5","Var_6","Var_7","Var_8","Var_9","Var_10"), sep = ", ", remove = FALSE, na.rm = TRUE) %>% 
  unite(popDisease, c("diseaseTitle", "diseaseAbstract"), sep = ", ", remove = FALSE, na.rm = TRUE) %>% 
  mutate(popDisease = str_remove_all(popDisease, "(\\bNA\\b)|(character\\(0\\))|(clinical trials)|(\\bhelp\\b)|(\\bsearch\\b)"))

  
data9 <- data8 %>% 
  left_join(select(disease4, PMID, popDisease))

data9 %>% select(PMID, popDisease)

saveRDS(data9, paste0("treatedDB/",projectName,".rds") )

