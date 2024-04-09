# Frequency From AI

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
      
      
      
      runStatementStud <- dbSendQuery(connection, "SELECT * FROM studiesfromcode")
      
      studies <- dbFetch(runStatementStud, n = -1)   
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementPop <- dbSendQuery(connection, "SELECT * FROM populationsfromcode WHERE ageClass IS NOT NULL;")
      
      populations <- dbFetch(runStatementPop, n = -1)  
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementFreq <- dbSendQuery(connection, "SELECT * FROM frequenciesfromcode")
      
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
  left_join(populations, by = c("PMID", "projectName" ) ) %>% 
  left_join(studies, by = c("PMID", "projectName"  ) ) %>% 
  rename("Author" = author) %>% 
  rename("Title" = title) %>% 
  rename("Country" = popCountry) %>% 
  rename("Disease" = popDisease) %>% 
  rename("Comorbidity" = popComorbidity) %>% 
  mutate(PMID = as.factor(PMID),
         Country = ifelse(str_detect(Country, "^\\s?$"),"Information not available", Country)
         )%>% 
  mutate(subset1 = str_extract(subset1, "^.{1,30}"))



server = function(input, output, session) { 

  # Frequency tbl output ----
  
  freq_reac <- reactive({
    freq2 %>%
      filter(str_detect(expFrequencyName, input$freqExposure)) %>%
      filter(if (nzchar(input$freqDisease)) str_detect(Disease, input$freqDisease) else TRUE) %>% 
      filter(if (nzchar(input$subset1)) str_detect(subset1, input$subset1) else TRUE) %>% 
      mutate(PMID_Fr = paste0("fr",idFreq0,"_",PMID),
             Author_Fr = paste0("fr",idFreq0,"_",Author)) 
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
      geom_col(aes(y= freqMeasure))+
      geom_text(aes(label = subset1, y = freqMeasure), angle = 0, vjust = 1, hjust = 0) +
      theme_classic() +
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

ui = navbarPage(
  theme =shinytheme("sandstone"),  ### navbar pour la barre de navigation
  title =(div(h1("SCI Results", style = 'font-size:30px;'))),
  
  
        tabPanel(h5("Prevalence & incidence"),
                 column(6,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "freqExposure",
                                 label = "Looking for the frequency of:"
                       ),
                       
                       selectInput(inputId = "freqPlotFill",label = "Fill",
                                   choices = c("Country","Disease" ,"Comorbidity" ) ),
                       selectInput(inputId = "freqPlotX",label = "X axis",choices = c("Author_Fr" ,"PMID_Fr")  ),
                       style = "padding: 2px; ")
                                      
                 )),
                 column(6,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "freqDisease",
                                 label = "Population pathology:"
                       ),
                       textInput(inputId = "subset1",
                                 label = "Subgroup:"
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
