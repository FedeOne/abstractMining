
library(gtsummary)
library(ggplot2)
library(tidyverse)

# studies

con <-  dbConnect(MySQL(),
                  dbname = "u551391167_sciresults",
                  host = "srv934.hstgr.io",
                  port = 3306,
                  user = "u551391167_fedesci2",
                  password = "Scire132!")


runStatement <- dbSendQuery(con, "SELECT * FROM studiesfromcode")

studiesfromDb <- dbFetch(runStatement , n = -1
)   

dbDisconnect(con)


# populations

con <-  dbConnect(MySQL(),
                  dbname = "u551391167_sciresults",
                  host = "srv934.hstgr.io",
                  port = 3306,
                  user = "u551391167_fedesci2",
                  password = "Scire132!")


runStatement <- dbSendQuery(con, "SELECT * FROM populationsfromcode")

popfromDb <- dbFetch(runStatement , n = -1
)   

dbDisconnect(con)

# results

con <-  dbConnect(MySQL(),
                  dbname = "u551391167_sciresults",
                  host = "srv934.hstgr.io",
                  port = 3306,
                  user = "u551391167_fedesci2",
                  password = "Scire132!")


runStatement <- dbSendQuery(con, "SELECT * FROM resultsfromcode")

resultsFromDb <- dbFetch(runStatement , n = -1
)   

dbDisconnect(con)


plotsDb <- resultsFromDb %>% 
  left_join(select(studiesfromDb,PMID, studyPlan, journalShort, yearPublication))



### measure Type by journal Barplot

plotsDb %>% 
  filter(!is.na(measureType), measureType!= "pValue") %>% 
  filter(str_detect(projectName, "Fatigue")) %>% 
  ggplot( aes(x = journalShort, fill= measureType)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


### measure Type by journal Barplot, 20 most publishing journals, this is Ok !!!

get20Journals <-  resultsFromDb %>% 
  left_join(select(studiesfromDb,PMID,  journalShort, yearPublication)) %>% 
  filter(!is.na(measureType), measureType!= "pValue") %>% 
  filter(str_detect(projectName, "Fatigue")) %>% 
  select(journalShort, measureType) %>% 
  group_by(journalShort) %>% 
  summarise(countMeasure = n() ) %>% 
  arrange(desc(countMeasure) ) %>% 
  slice(1:20)

plotsDb %>% 
  filter(journalShort %in% get20Journals$journalShort) %>% 
  filter(!is.na(measureType), measureType!= "pValue") %>% 
  filter(str_detect(projectName, "Fatigue")) %>% 
  ggplot( aes(x = journalShort, fill= measureType)) +
  geom_bar( position= "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 12)) +
  labs(title = "Regression analysis by 20 most publishing journals")



## 1: Plot method of analysis across time

# measureTypePerYear  <- plotsDb %>% 
#   filter(!is.na(measureType), measureType!= "pValue") %>% 
#   filter(str_detect(projectName, "Apnea")) %>% 
#   mutate(measureType = as.factor(measureType)) %>% 
#   group_by(yearPublication) %>% 
#   summarise(n())


## 1: Plot method of analysis across time This is also very good

  pubsPerYear <-  resultsFromDb %>% 
    select(PMID, measureType) %>% 
    left_join(select(studiesfromDb,PMID, yearPublication), by = "PMID") %>% 
    filter(measureType != "pValue") %>% 
    group_by(yearPublication) %>% 
    count(yearPublication) %>% 
    rename("nPubsYear" = n)
    

  yearByMeasure <-  resultsFromDb %>% 
  left_join(select(studiesfromDb,PMID, studyPlan, journalShort, yearPublication), by = "PMID") %>% 
    filter(measureType != "pValue") %>% 
  group_by(yearPublication) %>% 
  count(measureType) %>% 
    left_join(pubsPerYear, by = "yearPublication"  ) %>% 
    mutate(measureTypeOnPubsPerYear = n / nPubsYear)

  
  yearByMeasure %>% 
  ggplot(aes(x = yearPublication, y = measureTypeOnPubsPerYear, fill = measureType, colour = measureType)) +
    geom_col() +
    geom_text(aes(x = yearPublication, y = 0.95, label = nPubsYear), color = "black", angle = 90) +
    scale_x_continuous(breaks = seq(min(yearByMeasure$yearPublication), max(yearByMeasure$yearPublication), 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
    labs(title = "Evolution of type of analysis over time adjusted on publications/year")

  
    studiesfromDb %>% 
      filter(yearPublication == 1998) %>% 
      filter(PMID %in% resultsFromDb$PMID)
    
  ## plot measure type per journal
  
  
  ## 3: statistically significant results per journal or measureType
  
  ## 4: Number of published study per journal
  
  ## 5: Classification of exposures per journal (markers, genetic, sociodemo, environment...)
  
  