# Risk factors from AI

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
library(ggpubr)

## 4.2 get datas from PHP ####


loadProjectName <- function(){
  
  con <- dbConnect(MySQL(),
                   dbname = "u551391167_sciresults",
                   host = "srv934.hstgr.io",
                   port = 3306,
                   user = "u551391167_fedesci2",
                   password = "Scire132!")
  
  
  
  runStatementProjName <- dbSendQuery(con, "SELECT DISTINCT projectName FROM studiesfromcode")
  
  projName <- dbFetch(runStatementProjName, n = -1)   
  
  dbDisconnect(con)
  
  return(projName)
}

load_all <- function(projectNameInput){
      con <- dbConnect(MySQL(),
                       dbname = "u551391167_sciresults",
                       host = "srv934.hstgr.io",
                       port = 3306,
                       user = "u551391167_fedesci2",
                       password = "Scire132!")
      
      
      
      runStatementStud <- dbSendQuery(con, paste0("SELECT PMID, author,title ,studyPlan, yearPublication, projectName FROM studiesfromcode WHERE projectName IN ", projectNameInput() )  )
      
      general <- dbFetch(runStatementStud, n = -1)   
      
      dbDisconnect(con)
      
      con <- dbConnect(MySQL(),
                       dbname = "u551391167_sciresults",
                       host = "srv934.hstgr.io",
                       port = 3306,
                       user = "u551391167_fedesci2",
                       password = "Scire132!")
      
      runStatementPop <- dbSendQuery(con, "SELECT * FROM populationsfromcode WHERE ageClass IS NOT NULL;")
      
      populations <- dbFetch(runStatementPop, n = -1)  
      
      dbDisconnect(con)
      
      con <- dbConnect(MySQL(),
                       dbname = "u551391167_sciresults",
                       host = "srv934.hstgr.io",
                       port = 3306,
                       user = "u551391167_fedesci2",
                       password = "Scire132!")
      
      runStatementRes <- dbSendQuery(con, "SELECT * FROM resultsfromcode")
      
      results <- dbFetch(runStatementRes, n = -1)  
      
      dbDisconnect(con)
      
      ## set some parameters
      
      set_flextable_defaults(big.mark = "")
      
      `%notin%` <- Negate(`%in%`)  
      
      general <- general %>% 
        mutate(studyPlan = tolower(studyPlan))
      
      results2 <- results %>% 
        mutate(across(c("result","icLow","icUpper"), ~as.numeric(.)),
               exp1 = tolower(exp1),
               outcome = tolower(outcome),
               Association = paste0(round(result,2), " [", round(icLow,2),";", round(icUpper,2),"]"),
               resultSignif = ifelse((icLow >1 & icUpper>1)| (icLow<1 & icUpper<1), 1,0 ),
               MEASURE = ifelse(str_detect(measureType,"HR"),"HR",
                                ifelse(str_detect(measureType,"OR"),"OR",
                                       ifelse(str_detect(measureType,"RR"),"RR","Other")))
        ) %>%  
        left_join(select(populations, PMID,  popDisease) , by = "PMID") %>% 
        left_join(select(general, PMID, author), by = "PMID") %>% 
        rename( "Exposure" = exp1)
      
      
      
      return(list(general,populations, results2)) 

}






server = function(input, output, session) { 
  
  
  # Load project Names
  projectNames <- loadProjectName()
  
  
  # show project Names in input bar
  
  observe({
    updateSelectInput(session, "projectName", choices = projectNames$projectName, selected = head(projectNames$projectName, 1))   ### to have possible inputs updating according to data
  })
  
  
  # # creat
  # 
  # projectNameInput <-  reactive({
  #   projectNameInput <- input$projectName
  #   projectNameInput
  # })
  
  dbName <- "u551391167_sciresults"
  user <- "u551391167_fedesci2"
  pwd <- "Scire132!"
  
  con <- dbConnect(MySQL(),
                   dbname = dbName,
                   host = "srv934.hstgr.io",
                   port = 3306,
                   user = user ,
                   password = pwd )
  
 
   query <- reactive({
    paste0("SELECT PMID, author, title, studyPlan, yearPublication, projectName 
          FROM studiesfromcode 
          WHERE projectName IN ('", paste(input$projectName, collapse = "','"), "')")
  })
  
  runStatementStud <-  reactive({ 
  runStatementStud <- dbSendQuery(con, query()  )
  runStatementStud
  
  })

  general <- reactive({
    data <- dbFetch(runStatementStud(), n = -1)
    data
  })
  
  observe({
    # Trigger the query when input$projectName changes
    general()
  })
  
  observe({
    # Disconnect from the database when the session ends
    onSessionEnded(function() {
      dbDisconnect(con)
    })
  })
  

  
  # 1: Data Select ----

  
  ## 1.1: First page messages ----
  
  nStudies <- reactive({ 
    nStudies <- general() %>% distinct(PMID) %>% nrow()
    nStudies 
    
  })
  

  output$textStudies <- renderText({ paste0("Your selection contains ",
                                            nStudies() , " studies.") })
  

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
                 style = "padding: 2px;")
             
           )),
           column(6,wellPanel( 
             div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                 selectInput(inputId = "projectName",
                             label = "Project name:",
                             "names",
                             multiple = TRUE
                 )))),
           br(),
           column(12,
                  textOutput("textStudies"),
                  textOutput("textAnalysis"),
                  br(),
           ),
           br(),
           DT::dataTableOutput("tableStudies"),
           br(),
           plotOutput("sutdyYearPlot", width = "100%"),
           
  ),# end tab panel
  
  
  tabPanel(h5("Risk factors"),
           column(6,
           h3("Risk factors for a given outcome"),
           p("Be careful with spaces they count as match, use regular expressions.")
           ),
           column(6,
                  textInput(
                     inputId = "outcome",
                     label = "outcome",
                     value = "mortality"),
          
            selectInput(inputId = "table1Variables",
                       label = "Select variables in table",
                       choices = c("PMID" ,"author" ,"Association","Exposure","outcome", "popDisease","projectName", "MEASURE"  ),
                       selected = c("PMID", "Exposure", "Association" , "outcome"),
                       multiple = TRUE
                       ) 
            ),
           # column(6,
           #        textInput(inputId = "exposureMatch",
           #                  label = "How many times has this expoisure been studied?") 
           #        ),
           textOutput("exposureTimes"),
           
           DT::dataTableOutput("table"),
           br(),
           br(),
           plotOutput("pValPlot",width = "60%")
           ),   ### end first tab panel

  tabPanel(h5("Outcomes"),
           h3("Outcome for a given exposure"),
           p("Be careful with spaces they count as match, use regular expressions."),
           textInput(inputId = "exposure",
                     label = "exposure",
                     value = "iron"),
           
           selectInput(inputId = "table2Variables",
                       label = "Select variables in table",
                       choices = c("PMID" , "author" ,"Association","Exposure","outcome", "popDisease","projectName", "MEASURE"   ),
                       selected = c("PMID", "outcome", "Association" , "Exposure"),
                       multiple = TRUE
           ),
           DT::dataTableOutput("tableOutcome"),
           br(),
           br(),
           plotOutput("pValPlot2",width = "60%")
  )
  
  )



shinyApp(ui = ui, server = server) # perform app launch

# What should be shown : 
# A table, for each outcome which exposures have been studied and how many times, 

## For each outcome A table allow to check value of results + if result is significative and ratio of significative results

### for the global project: ratio of significative results


# Same to be done for each exposures, think if it can be done as a plot
