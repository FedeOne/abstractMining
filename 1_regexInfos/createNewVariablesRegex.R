
projectName <- "ironAnemiaLisa1"


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

data  <-  readRDS(paste0("../0_dataFromApi/1_dbTibble/db",projectName,".rds")) %>% 
  filter(!is.na(PMID))


# 1. Author for citation ####



data2 <- data %>% 
  mutate(AUTHOR =  str_extract(authors, "^[^;]+"),
         AuthorYear = paste0(AUTHOR,", ", publicationYear)
  ) 


# 2: country ####


# countrylist2 <- read_xlsx("countryNationality.xlsx") %>% 
#   mutate(country = tolower(paste0("\\b",country, "\\b" ) ),
#          nationality = tolower(paste0("\\b",nationality, "\\b"))
#   )

countrylist2 <- read_csv("countryNationalityUpdated.csv") %>% 
  select(4,5) %>% 
  rename("country" = 1 , "nationality" = 2) %>% 
  mutate(country = str_remove_all(country, "\\(.*"),
         country = str_remove_all(country, "\\,.*"),
         country = str_trim(country),
         
         nationality = str_remove_all(nationality, "\\(.*"),
         nationality = str_remove_all(nationality, "\\,.*"),
         nationality = str_trim(nationality) ,
         
         country = tolower(paste0("\\b",country, "\\b" ) ),
         nationality = tolower(paste0("\\b",nationality, "\\b"))
         
  )

countryRegexLow <- paste0(countrylist2$country, collapse = "|")
nationalityRegexLow <- paste0(countrylist2$nationality, collapse = "|")

dataCountry <- data2 %>% 
  select(PMID, TITLE,ABSTRACT,  keywords) %>% 
  mutate(textCountry = tolower(paste0(TITLE,", ",ABSTRACT, ", ", keywords)) ) %>% 
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



# 4: Cohort and RCT and meta ####

sPlan <- data %>% 
  select(PMID, studyPlan) %>% 
  mutate(studyPlan = tolower(studyPlan),
    studyPlan2 = ifelse(str_detect(studyPlan, "meta analysis|meta-analysis"), "meta analysis",
                             ifelse(str_detect(studyPlan, "clinical trial"), "clinical trial",
                                    ifelse(str_detect(studyPlan, "randomized controlled trial"), "randomized controlled trial",
                                           ifelse(str_detect(studyPlan, "comparative study"), "comparative study",
                                                  ifelse(str_detect(studyPlan, "observational study"), "observational study", 
                                                         ifelse(str_detect(studyPlan, "systematic review"), "systematic review",
                                                                ifelse(str_detect(studyPlan, "review"), " review", "other"
                                                                )))))))  
    ) %>% 
      select(PMID, "studyPlan" = studyPlan2)
        


data5 <- data4 %>% 
  select(-c(studyPlan)) %>% 
  left_join(sPlan)


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

data6 <- data5 %>% 
  left_join(acronyms2, by = "PMID")


# 7: population age ####


meanAge2 <- data %>% 
  mutate(
         ageMeasure = NA,
         ageUnit = NA,
         ageSD = NA,
         
         ageValue = NA,
         ageLow = NA,
         ageUp = NA,
  ) %>% 
  select(PMID, ageMeasure,ageValue, ageUnit, ageSD, ageLow,ageUp)

data7 <- data6 %>% 
  left_join(meanAge2, by = "PMID") %>% 
  mutate(popDisease = " ")



saveRDS(data7, paste0("treatedDB/",projectName,".rds") )




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

  
data8 <- data7 %>% 
  left_join(select(disease4, PMID, popDisease))


saveRDS(data8, paste0("treatedDB/",projectName,".rds") )

