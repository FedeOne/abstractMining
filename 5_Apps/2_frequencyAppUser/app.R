# Frequency app users

# Libraries ---- 

library(stringr)
library(RMySQL)
library(data.table)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(tidyr)
library(readxl)
library(shinyWidgets)
library(shinyBS) 
library(flextable)

## 4.2 get datas from PHP ####

load_all <- function(){
     
      source("../../../credentials/connection.r")
      
      
      runStatementStud <- dbSendQuery(connection, "SELECT * FROM study")
      
      studies <- dbFetch(runStatementStud, n = -1)   
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementPop <- dbSendQuery(connection, "SELECT * FROM populations;")
      
      populations <- dbFetch(runStatementPop, n = -1)  
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementFreq <- dbSendQuery(connection, "SELECT * FROM frequencies")
      
      frequencies <- dbFetch(runStatementFreq, n = -1)  
      
      dbDisconnect(connection)
      
      return(list(studies,populations, frequencies)) 
      
}

## set some parameters

studies <- load_all()[[1]] 
populations <- load_all()[[2]]
frequencies <- load_all()[[3]]


set_flextable_defaults(big.mark = "")

`%notin%` <- Negate(`%in%`)  

freq2 <- frequencies %>% 
  left_join( select(populations, -c(user_id, idProject, idStudy, fromCode, projectName) ), 
                    by = c("idPopulation" , "PMID") )  %>% 
  left_join(select(studies ,-c(user_id, idProject, idStudy0, fromCode) ) ,
            by =  c("idStudy", "PMID") ) %>% 
  rename("Author" = author) %>% 
  rename("Title" = title) %>% 
  rename("Country" = popCountry) %>% 
  rename("Disease" = popDisease) %>% 
  rename("Comorbidity" = popComorbidity) %>% 
  mutate(PMID = as.factor(PMID),
         Country = ifelse(str_detect(Country, "^\\s?$"),"Information not available", Country),
         expFrequencyName = tolower(expFrequencyName)
         )

server = function(input, output, session) { 

  # Frequency tbl output ----
  
  freq_reac <- reactive({
    freq2 %>%
      filter(str_detect(expFrequencyName, input$freqExposure)) %>%
      filter(if (nzchar(input$freqDisease)) str_detect(Disease, input$freqDisease) else TRUE) %>% 
      filter(if (nzchar(input$subset1)) str_detect(subset1, input$subset1) else TRUE) %>% 
      filter(if (nzchar(input$userID)) user_id %in% input$userID  else TRUE) %>% 
      mutate(PMID_Fr = paste0("fr",idFreq,"_",PMID),
             Author_Fr = paste0("fr",idFreq,"_",Author)) 
  })
  
  output$tableFreq <-  DT::renderDataTable(
    DT::datatable(freq_reac()  %>% 
                    # filter(str_detect(expFrequencyName, input$freqExposure) ) %>% 
                    select(PMID, Author, Title ,"Exposure" =  expFrequencyName,
                           "Frequency"= freqMeasure, "Unit" = freqUnit, Country)
    ))
  
  output$tableAbstract <-  DT::renderDataTable(
    DT::datatable(freq_reac()  %>% 
                    group_by(PMID) %>% slice(1) %>%  
                    select(PMID, Author, ABSTRACT) %>% group_by(PMID) %>% 
                    slice(1) %>% ungroup()
    ))
  
  # Frequency plot ####
  
  output$freqPlot <- renderPlot({
    freq_reac() %>%
      ggplot(aes_string(x = input$freqPlotX, fill = input$freqPlotFill  )) +
      geom_col(aes(y= freqMeasure), position = "dodge" )+
      geom_text(aes(label = subset1, y = freqMeasure), angle = 0, vjust = 1, hjust = 1) +
      #  geom_errorbar(aes(ymin = freqLow, ymax = freqHigh) ) +
      theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1) ) +
      labs(y = paste0("%"))  +
      theme(legend.position = "top") +
      coord_flip()
    
  })
  
  output$freqPlotDim <- renderUI(
    if(nrow(freq_reac())<=10){
      plotOutput("freqPlot",width = "100%",height = "400px")
    } else if(nrow(freq_reac())<=20){
      plotOutput("freqPlot",width = "95%",height = "640px")
    } else {
      plotOutput("freqPlot",width = "95%",height = "800px")
    }
  )
 
}

# UI ####

ui = navbarPage(
  theme =shinytheme("sandstone"),  ### navbar pour la barre de navigation
  title =(div(h1("SCI Results", style = 'font-size:30px;'))),
  
  
        tabPanel(h5("Prevalence & incidence"),
                 column(4,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "freqExposure",
                                 label = "Looking for the frequency of:"
                       ),
                       
                       selectInput(inputId = "freqPlotFill",label = "Fill",
                                   choices = c("Country","Disease" ,"Comorbidity" ) ),
                       selectInput(inputId = "freqPlotX",label = "X axis",choices = c("Author_Fr" ,"PMID_Fr")  ),
                       style = "padding: 2px; ")
                                      
                 )),
                 column(4,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "freqDisease",
                                 label = "Population pathology:"
                       ),
                       textInput(inputId = "subset1",
                                 label = "Subgroup:"
                       )
                       ))),
                 column(4,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "userID",
                                   label = "User ID"
                                   # choices = "Names" ,
                                   # width = '80px',
                                   # multiple = TRUE 
                                 )
                       ))),
                 uiOutput("freqPlotDim"),
                 #plotOutput("freqPlot", width = "100%"),
        ),
        tabPanel(h5("Frquency data"),
                 DT::dataTableOutput("tableFreq"),
                 DT::dataTableOutput("tableAbstract"),
                 downloadButton(outputId = "tbl_freq_download", label = "Download Table")
                 )
  
  
  )



shinyApp(ui = ui, server = server) # perform app launch

# What should be shown : 
# A table, for each outcome which exposures have been studied and how many times, 

## For each outcome A table allow to check value of results + if result is significative and ratio of significative results

### for the global project: ratio of significative results


# Same to be done for each exposures, think if it can be done as a plot
