
library(dplyr)
library(arrow)

setwd("C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes")


dbPath <- "C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome/"
data <-  arrow::read_feather(paste0(dbPath, "outORHRLlamaOutcomePartialstridorIAh2010.feather"))


## Check Outcome from Llama

setwd("C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome")

ORHROutcomeLlama <- arrow::read_feather( "outORHRLlamaBertOutcomePartialstridorIAh2010.feather")
ORHROutcomeLlama$ansOutcomeLlama
ORHRRRLLAMA$contextLLAMAExposure
### Check Outcome from Bert

setwd("C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1b_ORHRBertAfterLLAMADB")

ORHROutcomeBert <-  arrow::read_feather( "outORHRBertOutcomeFinalckdIron.feather")

rowi <- 3

ORHROutcomeBert$context[rowi]

ORHROutcomeBert$ansOutcome[rowi]


### check treated ORHR from Bert

setwd("C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1b_ORHRBertAfterLLAMADB")

bertPath <- "C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1b_ORHRBertAfterLLAMADB/"

ORHRBert <-  arrow::read_feather(paste0(bertPath, "outORHRBertFinalckdIron.feather"))

list.files()




## Check treated PREV DB from LLAMA

dbPrevPath <- "C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/2_prevBert/"
data2 <-  arrow::read_feather(paste0(dbPrevPath, "outPrevBertFinalstridorIAh2010.feather"))

data$questPercMistral[3]

## Check treated populations for Bert

dbPopPath <- "C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/3_popLlamaBertTreatedDB/"
data2 <-  arrow::read_feather(paste0(dbPopPath, "popBertFinalstridorIAh2010.feather")) %>% 
  select(ABSTRACT, contains("ans"))
  

## Check treated populations for Llama

dbPopPath <- "C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/3_popLlamaBertTreatedDB/"
data2 <-  arrow::read_feather(paste0(dbPopPath, "popLlamaBertFinalMSA.feather")) # %>% 
  select(ABSTRACT, 
         # contains("ans")
         )



data2$ABSTRACT
findNPythonLlama$ABSTRACT[11]
