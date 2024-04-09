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

library(gt)
library(gtsummary)
library(ggpubr)

## 4.2 get datas from PHP ####


con <- dbConnect(MySQL(),
                 dbname = "u551391167_sciresults",
                 host = "srv934.hstgr.io",
                 port = 3306,
                 user = "u551391167_fedesci2",
                 password = "Scire132!")



runStatementStud <- dbSendQuery(con, "SELECT * FROM studiesfromcode")

studies <- dbFetch(runStatementStud, n = -1)   



# con <- dbConnect(MySQL(),
#                  dbname = "u551391167_sciresults",
#                  host = "srv934.hstgr.io",
#                  port = 3306,
#                  user = "u551391167_fedesci2",
#                  password = "Scire132!")

runStatementPop <- dbSendQuery(con, "SELECT * FROM populationsfromcode WHERE ageClass IS NOT NULL;")

populations <- dbFetch(runStatementPop, n = -1)  

# dbDisconnect(con)

# con <- dbConnect(MySQL(),
#                  dbname = "u551391167_sciresults",
#                  host = "srv934.hstgr.io",
#                  port = 3306,
#                  user = "u551391167_fedesci2",
#                  password = "Scire132!")

runStatementFreq <- dbSendQuery(con, "SELECT * FROM frequenciesfromcode")

frequencies <- dbFetch(runStatementFreq, n = -1)  

dbDisconnect(con)


## set some parameters



`%notin%` <- Negate(`%in%`)  




server = function(input, output, session) { 

  # Frequency tbl output ----
  
  
  
  data_reac <- reactive({
    
    proj <- input$projectName
    abs <- input$abstractSearch
    
  
    # data_reac <- studies %>% filter( ifelse(is.null(input$projectName), grepl(input$titleSearch, title),
    #                                          projectName %in% input$projectName  ) )   
    # 
    if(input$projectName == "" ){
      data_reac <- studies %>% filter(grepl(input$titleSearch, title) )   
      
    } else {
      data_reac <- studies %>% filter(projectName %in% input$projectName  )
    }
    
    data_reac
    
  })
  
  nStudies <- reactive({ 
    nStudies <- data_reac() %>% distinct(PMID) %>% nrow()
    nStudies 
  
  })
  
  output$textStudies <- renderText({ paste0("Your selection contains ",nStudies() , " articles") })

  output$tableStudies <-  DT::renderDataTable(DT::datatable(data_reac() %>%
                                                              select(PMID, author, title, journal)))
  
  # year Plot plot ####
  
  # output$yearPlot <- renderPlot({
  #   data_reac() %>% 
  #     ggplot(aes(x = yearPublication) )  +
  #     geom_bar() +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
  #     labs(x = paste0("Year of publication"))
  #   
  #   
  # })
  # 
  # 
  # # Studies plot ####
  # 
  # output$studiesPlot <- renderPlot({
  #   data_reac() %>% 
  #     ggplot(aes(x = str_wrap(studyPlan, width = 15) ) ) +
  #     geom_bar() +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
  #     labs(x = paste0("Study type"))
  #   
  # 
  # })
  
  studyYearPlot <- function(){
    study <-  data_reac() %>% 
      ggplot(aes(x = studyPlan , fill = studyPlan) ) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
      labs(y = paste0("Study type")) +
      theme_classic()
    
    year <- data_reac() %>% 
      ggplot(aes(x = yearPublication) )  +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
      labs(x = paste0("Year of publication"))+
      theme_classic()
    
    ggarrange(study, year, ncol = 2)
    
    }
  
  
  output$sutdyYearPlot <- renderPlot({ 
    req(data_reac())
    studyYearPlot()
  } )
    
    
 
}

ui = navbarPage(
  theme =shinytheme("sandstone"),  ### navbar pour la barre de navigation
  title =(div(h1("SCI Results", style = 'font-size:30px;'))),
  
  
        tabPanel(h5("Research equation"),
                 column(6,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "titleSearch",
                                 label = "Search in Title:"
                       ),
                       
                       # textInput(inputId = "abstractSearch",
                       #           label = "Search in abstract:"
                       #           ),
                       style = "padding: 2px;")
                                      
                 )),
                 column(6,wellPanel( 
                   div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                       textInput(inputId = "projectName",
                                 label = "Project name:"
                       )))),
                textOutput("textStudies"),
                 br(),

               DT::dataTableOutput("tableStudies"),
               br(),
               plotOutput("sutdyYearPlot", width = "100%"),
               
        ),
  tabPanel(h5("Studies plot"), 
           plotOutput("studiesPlot", width = "100%"),
           )
       
  )



shinyApp(ui = ui, server = server) # perform app launch

# What should be shown : 
# A table, for each outcome which exposures have been studied and how many times, 

## For each outcome A table allow to check value of results + if result is significative and ratio of significative results

### for the global project: ratio of significative results


# Same to be done for each exposures, think if it can be done as a plot
