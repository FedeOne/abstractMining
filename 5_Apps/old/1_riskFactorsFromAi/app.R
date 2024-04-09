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

# 1 func Load proj Name ####


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

# 2 func Load all ####



dbName <- "u551391167_sciresults"
user <- "u551391167_fedesci2"
pwd <- "Scire132!"

con <- dbConnect(MySQL(),
                 dbname = dbName,
                 host = "srv934.hstgr.io",
                 port = 3306,
                 user = user ,
                 password = pwd )



# 3: server ####

server = function(input, output, session) { 
  
  
  ## 3.1 Load project Names ####
  projectNames <- loadProjectName()
  
  
  # show project Names in input bar
  
  observe({
    updateSelectInput(session, "projectName", choices = projectNames$projectName, selected = head(projectNames$projectName, 1))   ### to have possible inputs updating according to data
  })
  
  
  ## 3.2 react query study
  
  con <- dbConnect(MySQL(),
                   dbname = "u551391167_sciresults",
                   host = "srv934.hstgr.io",
                   port = 3306,
                   user = "u551391167_fedesci2",
                   password = "Scire132!")
  
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
    data <- dbFetch(runStatementStud(), n = -1) %>% 
      mutate(studyPlan = tolower(studyPlan))
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
  
  ## 3.3 react query pop
  
  con <- dbConnect(MySQL(),
                   dbname = "u551391167_sciresults",
                   host = "srv934.hstgr.io",
                   port = 3306,
                   user = "u551391167_fedesci2",
                   password = "Scire132!")
  
  queryPop <- reactive({
    paste0("SELECT * FROM populationsfromcode 
          WHERE projectName IN ('", paste(input$projectName, collapse = "','"), "')")
  })
  
  runStatementPop <-  reactive({ 
    runStatementPop <- dbSendQuery(con, queryPop()  )
    runStatementPop
    
  })
  
  populations <- reactive({
    populations <- dbFetch(runStatementPop(), n = -1)
    populations
  })
  
  observe({
    # Trigger the query when input$projectName changes
    populations()
  })
  
  observe({
    # Disconnect from the database when the session ends
    onSessionEnded(function() {
      dbDisconnect(con)
    })
  })
  
  ## 3.3 react query results 
  
  con <- dbConnect(MySQL(),
                   dbname = "u551391167_sciresults",
                   host = "srv934.hstgr.io",
                   port = 3306,
                   user = "u551391167_fedesci2",
                   password = "Scire132!")
  
  queryRes <- reactive({
    paste0("SELECT * FROM resultsfromcode 
          WHERE projectName IN ('", paste(input$projectName, collapse = "','"), "')")
  })
  
  runStatementRes <-  reactive({ 
    runStatementRes <- dbSendQuery(con, queryRes()  )
    runStatementRes
    
  })
  
  results <- reactive({
    results <- dbFetch(runStatementRes(), n = -1)
    results
  })
  
  observe({
    # Trigger the query when input$projectName changes
    results()
  })
  
  observe({
    # Disconnect from the database when the session ends
    onSessionEnded(function() {
      dbDisconnect(con)
    })
  })
  
  
  results2 <- reactive({ 
    results2 <- results() %>% 
      mutate(across(c("result","icLow","icUpper"), ~as.numeric(.)),
             exp1 = tolower(exp1),
             outcome = tolower(outcome),
             Association = paste0(round(result,2), " [", round(icLow,2),";", round(icUpper,2),"]"),
             resultSignif = ifelse((icLow >1 & icUpper>1)| (icLow<1 & icUpper<1), 1,0 ),
             MEASURE = ifelse(str_detect(measureType,"HR"),"HR",
                              ifelse(str_detect(measureType,"OR"),"OR",
                                     ifelse(str_detect(measureType,"RR"),"RR","Other")))
      ) %>%  
      left_join(select(populations(), PMID,  popDisease) , by = "PMID") %>% 
      left_join(select(general(), PMID, author, studyPlan), by = "PMID") %>% 
      rename( "Exposure" = exp1)
    
    results2
  } )
  # 1: Data Select ----
  
  selectedPMID <- reactive({
    
    proj <- input$projectName
    abs <- input$abstractSearch
    
    if(length(proj) == 0){
      selectedPMID <- general() %>% filter(grepl(input$titleSearch, tolower(title) ) )   
      
    } else {
      selectedPMID <- general() %>% filter(projectName %in% proj)
    }
    
    selectedPMID
    
  })
  
  
  ## 1.1: First page messages ----
  
  
  
  nStudies <- reactive({ 
    nStudies <- selectedPMID() %>% distinct(PMID) %>% nrow()
    nStudies 
    
  })
  
  nHR <- reactive({ 
    nHR <- results2() %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "HR" ) %>% 
      nrow()
    nHR 
    
  })
  
  nOR <- reactive({ 
    nOR <- results2() %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "OR" ) %>% 
      nrow()
    nOR 
    
  })
  
  nRR <- reactive({ 
    nRR <- results2() %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "RR" ) %>% 
      nrow()
    nRR 
    
  })
  
  output$textStudies <- renderText({ paste0("Your selection contains ",
                                            nStudies() , " studies.") })
  
  output$textAnalysis <- renderText({ paste0("In the abstracts of the selections are mentioned ",
                                             nHR() , " Hazard Ratios, ",
                                             nOR(), " Odd Ratios and ",
                                             nRR(), " Relative Risks.") })
  
  ## 1.2: Selected PMID Plot ####
  
  studyYearPlot <- function(){
    study <-  selectedPMID() %>% 
      ggplot(aes(x = studyPlan , fill = studyPlan) ) +
      geom_bar() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
      labs(y = paste0("Study type")) 
    
    year <- selectedPMID() %>% 
      filter(yearPublication > 1900) %>% 
      ggplot(aes(x = yearPublication) )  +
      geom_bar() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(x = paste0("Year of publication"))
    
    ggarrange(study, year, ncol = 2)
    
  }
  
  
  output$sutdyYearPlot <- renderPlot({ 
    req(selectedPMID())
    studyYearPlot()
  } )
  
  
  # 2: Risk factors ####
  
  ## 2.1: results reactive
  
  dataReac <- reactive({ 
    
    dataReac <-  results2() %>% 
      filter(PMID %in%selectedPMID()$PMID) %>% 
      filter(str_detect(outcome , input$outcome) )  %>% 
      filter(if (input$trialFilter == TRUE) str_detect(studyPlan, "trial") else
        studyPlan %in% general()$studyPlan
      )
    dataReac 
  })
  
  
  ## 2.2: Risk factors table ----
  
  output$table <- DT::renderDataTable(DT::datatable(dataReac()%>% 
                                                      select(input$table1Variables) ) )
  
  
  pValPlot <- function(){
    
    dataReac() %>%
      filter(measureType != "pValue") %>% 
      mutate(resultSignif2 = recode(resultSignif, `1` = "pValue <= 0.05" ,`0` = "pValue > 0.05"  )) %>% 
      ggplot() +
      geom_bar(aes(y = sum(resultSignif), x = measureType, fill =resultSignif2), position="fill", stat="identity") + 
      labs(x = "measureType", y = " ") +
      theme_classic() +
      guides(fill=guide_legend(title=" ")) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 14))
  }
  
  
  output$pValPlot = renderPlot({ 
    req(dataReac())
    pValPlot()
  } )
  
  
  ## Outcomes ####
  
  dataReacOut <- reactive({ 
    
    results2() %>% 
      filter(PMID %in%selectedPMID()$PMID) %>% 
      filter(str_detect(Exposure , input$exposure) )  
  })
  
  # render table outcome ####
  
  output$tableOutcome <- DT::renderDataTable(DT::datatable(dataReacOut()%>% 
                                                             select(input$table2Variables) ) )
  
  # render p value plot
  
  pValPlot2 <- function(){
    
    dataReacOut() %>%
      filter(measureType != "pValue") %>% 
      mutate(resultSignif2 = recode(resultSignif, `1` = "pValue <= 0.05" ,`0` = "pValue > 0.05"  )) %>% 
      ggplot() +
      geom_bar(aes(y = sum(resultSignif), x = measureType, fill =resultSignif2), position="fill", stat="identity") + 
      labs(x = "measureType", y = " ") +
      theme_classic() +
      guides(fill=guide_legend(title=" ")) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 14))
  }
  
  
  output$pValPlot2 = renderPlot({ 
    req(dataReacOut())
    pValPlot2()
  } )
  
} # end Server

# UI ####

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
                  p("Be careful with spaces they count as match, use regular expressions."),
                  checkboxInput("trialFilter", "Filter for clinical trails", FALSE)
           ),
           column(6,
                  textInput(
                    inputId = "outcome",
                    label = "outcome",
                    value = "mortality"),
                  
                  selectInput(inputId = "table1Variables",
                              label = "Select variables in table",
                              choices = c("PMID" ,"author" ,"Association","Exposure","outcome", "popDisease","projectName", "MEASURE", "pValue", "journalShort"  ),
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
                       choices = c("PMID" , "author" ,"Association","Exposure","outcome", "popDisease","projectName", "MEASURE", "pValue", "journalShort"   ),
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
