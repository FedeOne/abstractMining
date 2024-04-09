


projectName <- "projetTutoCkd"

library(rstudioapi)
library(readtext)
library(stringr)
library(dplyr)
library(tidyverse)
library(stringi)
library(quanteda)
library(readxl)

`%notin%` <- Negate(`%in%`)

currentPath <- rstudioapi::getSourceEditorContext()$path 

currentPath2 <- currentPath %>% 
  str_remove("getting_datas_V3.R")

setwd(currentPath2) 


plain_text <- readtext(paste0("0_rawData/",projectName,".txt") ,encoding = "utf-8") 


plain_text2 <-  plain_text[,-1] %>% as.data.frame() %>% 
  rename("txt" = 1)

noNewLine <- str_replace_all(plain_text2, pattern = "\n",replacement = " ")
### split studies

str_count(plain_text2, "PMID-")

studies$text
studies <- str_extract_all(noNewLine, "(PMID-.*)(?=PMID)") %>% 
  unlist() %>% as_tibble()   %>% rename("text" = 1)

studies <- str_extract_all(noNewLine, "(PMID-.*?)\\s*(?=(PMID-|SO\\s*-))") %>% 
  unlist() %>% as_tibble()   %>% rename("text" = 1)

corp <- corpus(noNewLine)
toks <- tokens(corp)

kwic

studies <- str_extract_all(noNewLine, "(PMID-\\s+\\d+?)\\s*(?=(PMID-|SO\\s*-))") %>% 
  unlist() %>% as_tibble()   %>% rename("text" = 1)

str_view(noNewLine,"(PMID-\\s+\\d+)"  )

str_view(p,"(PMID-\\s+\\d+)"  )

data <- data.frame(studies) %>% 
  mutate(text = str_squish(text))

### Pubmed ID ####

data2 <- data %>% mutate(
  PMID = str_extract(text, pattern = "(?<=PMID-)\\s*\\d+") ,
  PMID = str_trim(PMID)
  )  ## accept only numbers
# ends with OWN, without including it in the match

## Titles ####

data3 <- data2 %>% mutate(
  TITLE = str_extract(text, "TI\\s*-\\s(.*?)\\s*(PG|LI)"),
  TITLE = ifelse(is.na(TITLE),str_extract(text, "BTI\\s*-\\s(.*?)\\s*(PG|LI)"), TITLE ),
  TITLE = str_remove(TITLE, "^TI\\s*-"), ## Remove all TI - at beginning of string
  TITLE = str_remove(TITLE, "PG$"),  ## remove all PG at end of the string
  TITLE = str_replace(TITLE, "\\s{2,}",' '), ## replace any number of spaces grater than 2 with one space
) %>% 
  relocate(TITLE, .before = text)

## abstracts ####

data4 <- data3 %>% 
  mutate(ABSTRACT = str_extract(text, "AB\\s*(.*?)\\s*(?=(FAU\\s*-|CI\\s*-|AU\\s*-|AD\\s*-))"),
         ABSTRACT = str_remove(ABSTRACT, "AB\\s*-"),
         ABSTRACT = str_replace(ABSTRACT, "\\s*",' '),
         ABSTRACT = stri_enc_toutf8(ABSTRACT),
         ABSTRACT = str_squish(ABSTRACT),
        #ABSTRACTS = str_remove_all(ABSTRACTS, "‰|â|€|Â"), ## need unicode or start with UTF8
         ABSTRACT = gsub(ABSTRACT,pattern = "\u00B7", replacement = ".")   ### gsub fonctionne mieux avec unicodes
         ) %>% 
  relocate(ABSTRACT, .after = TITLE)

## year of publication ####

data5 <- data4 %>% 
  mutate(publicationDate = str_extract(text, "(?<=DP)(.*?)\\s*(?=TI)"),
         publicationDate = str_remove(publicationDate, "-"),
         publicationDate = str_remove(publicationDate, "^\\s"),
         publicationYear = str_extract(publicationDate,"\\d{4}")
         # ABSTRACTS = str_replace(ABSTRACTS, "\\s{2,}",' ') 
         ) %>% 
  relocate(publicationDate, .before = TITLE)

## Mesh terms ####

data6 <- data5 %>% 
    mutate(keywords = str_extract_all(text, "(?<=(MH\\s.-|MH\\s-))(.*?)(?=(MH|PMC) )") ) %>% 
  # group_by(PMID) %>% 
  # mutate(
  #   keyword1 = keywords[[1]][1], 
  #   keyword2 = keywords[[1]][2],
  #   keyword3 = keywords[[1]][3],
  #   keyword4 = keywords[[1]][4],
  #   keyword5 = keywords[[1]][5],
  #   keyword6 = keywords[[1]][6],
  #   keyword7 = keywords[[1]][7], 
  #   keyword8 = keywords[[1]][8],
  #   keyword9 = keywords[[1]][9],
  #   keyword10 = keywords[[1]][10],
  #   keyword11 = keywords[[1]][11],
  #   keyword12 = keywords[[1]][12],
  #   keyword13 = keywords[[1]][13],
  #   keyword14 = keywords[[1]][14], 
  #   keyword15 = keywords[[1]][15],
  #   keyword16 = keywords[[1]][16],
  #   keyword17 = keywords[[1]][17],
  #   keyword18 = keywords[[1]][18],
  #   keyword19 = keywords[[1]][19]) %>% 
  # ungroup()
mutate(keywords = sapply(keywords, function(x) paste(x, collapse = ", ")),
       keywords = str_replace_all(keywords, "/", " "),
       keywords = str_squish(keywords)
       )


## protocole type ####

data7 <- data6 %>% 
  mutate(protocolType = str_extract_all(text, "(?<=(PT\\s.-|PT\\s-) )(.*?)(?=(PT\\s*-|DEP\\s*-) )")) %>% 
  # group_by(PMID) %>% 
  # mutate(
  #   protocol1 = protocolType[[1]][1], 
  #   protocol2 = protocolType[[1]][2],
  #   protocol3 = protocolType[[1]][3],
  #   protocol4 = protocolType[[1]][4],
  #   protocol5 = protocolType[[1]][5],
  #   protocol6 = protocolType[[1]][6],
  #   protocol7 = protocolType[[1]][7], 
  #   protocol8 = protocolType[[1]][8],
  #   protocol9 = protocolType[[1]][9],
  #   protocol10 = protocolType[[1]][10] ) %>% 
  # ungroup() %>% 
  mutate(protocolType = tolower(protocolType),
         protocolType = str_replace_all(protocolType, "\\-", " "),
         protocolType = str_squish(protocolType),
         studyPlan = ifelse(str_detect(protocolType, "meta analysis|metaanalysis"), "meta analysis",
                            ifelse(str_detect(protocolType, "clinical trial"), "clinical trial",
                                   ifelse(str_detect(protocolType, "randomized controlled trial"), "randomized controlled trial",
                                        ifelse(str_detect(protocolType, "comparative study"), "comparative study",
                                               ifelse(str_detect(protocolType, "observational study"), "observational study", 
                                                      ifelse(str_detect(protocolType, "observational study"), "observational study",
                                                             ifelse(str_detect(protocolType, "systematic review"), " systematic review", "other"
                                                      )))))))
         )
    

## Author

data8 <- data7 %>% 
  mutate(FAU_yes = ifelse(str_detect(text, "FAU\\s")==FALSE,0,1) ,   
         Authors = str_extract_all(text, "(?<=(FAU\\s.-|FAU\\s-))(.*?)(?=(AU\\s*-|AD\\s*-) )")) %>% 
  group_by(PMID)  %>% 
  mutate(
    author1 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][1]), 
    author2 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][2]),
    author3 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][3]),
    author4 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][4]),
    author5 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][5]),
    author6 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][6]),
    authorn = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][length(Authors[[1]])] ) ) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_squish )) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_remove, "," )) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_remove, "(?<=(\\s.|\\s\\s.))(.*)" )) %>% 
  ungroup()

## Other keywords ####

data9 <- data8 %>%  # "(?<=(MH\\s.-|MH\\s-))(.*?)(?=(MH|PMC) )"
  mutate(OT = str_extract_all(text, "(?<=(OT\\s-))(.*?)(?=(OT\\s*-|EDAT\\s*-|MH\\s*-|COIS\\s*-) )") ) %>%  
  # group_by(PMID) %>% 
  # mutate(
  #   OT1 = OT[[1]][1], 
  #   OT2 = OT[[1]][2],
  #   OT3 = OT[[1]][3],
  #   OT4 = OT[[1]][4],
  #   OT5 = OT[[1]][5],
  #   OT6 = OT[[1]][6],
  #   OT7 = OT[[1]][7],
  #   OT8 = OT[[1]][8]
  #   ) %>% 
mutate(OT = sapply(OT, function(x) paste(x, collapse = ", ")),
       OT = str_replace_all(OT, "/", " "),
       OT = str_squish(OT)
)
  


## journal ####

data10 <- data9 %>% 
  mutate(journal = str_extract(text,"(?<=(JT\\s-|JT\\s\\s-))(.*?)(?=(JID\\s+-))"),
         journal = str_squish(journal),
         journalShort = str_extract(text,"(?<=TA(\\s{1,3})\\-)(.*?)(?=(JT\\s+\\-))")) %>% 
  select( PMID,journal ,journalShort,TITLE, publicationYear, studyPlan, protocolType , keywords ,publicationDate,ABSTRACT, contains("Author"),contains("key"),contains("OT"), -c(text, Authors))


data11 <- data10 %>% 
  mutate(containsHR = str_detect(ABSTRACT, "(\\bHR\\b)|(hazard ratio) "),
         containsOR = str_detect(ABSTRACT, "(\\bOR\\b)|(odd ratio)"),
         containsRR = str_detect(ABSTRACT, "(\\bRR\\b)|(relative risk)"),
         containsIncidence = str_detect(ABSTRACT, regex("incidence",ignore_case=TRUE) ), 
         containsPrevalence = str_detect(ABSTRACT, regex("prevalence",ignore_case=TRUE) ),
  )



saveRDS(data11, paste0("2a_Rdatabases/",projectName,".rds") )


