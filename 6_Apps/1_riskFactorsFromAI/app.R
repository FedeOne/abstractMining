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



load_all <- function(){
      
      source("../../../credentials/connection.r")
      
      runStatementStud <- dbSendQuery(connection, "SELECT PMID, author,title ,studyPlan, yearPublication, projectName FROM studiesfromcode")
      
      general <- dbFetch(runStatementStud, n = -1)   
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementPop <- dbSendQuery(connection, "SELECT * FROM populationsfromcode WHERE ageClass IS NOT NULL;")
      
      populations <- dbFetch(runStatementPop, n = -1)  
      
      dbDisconnect(connection)
      
      source("../../../credentials/connection.r")
      
      runStatementRes <- dbSendQuery(connection, "SELECT * FROM resultsfromcode")
      
      results <- dbFetch(runStatementRes, n = -1)  
      
      dbDisconnect(connection)
      
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
  
  
  general <- load_all()[[1]]
  populations <- load_all()[[2]]
  results2 <- load_all()[[3]]
  
  observe({
    updateSelectInput(session, "projectName", choices = general$projectName, selected = head(general$projectName, 1))   ### to have possible inputs updating according to data
  })
  
  
  # 1: Data Select ----

  selectedPMID <- reactive({
    
    proj <- input$projectName
    abs <- input$abstractSearch
    
    if(length(proj) == 0){
      selectedPMID <- general %>% filter(grepl(input$titleSearch, tolower(title) ) )   
      
    } else {
      selectedPMID <- general %>% filter(projectName %in% proj)
    }
    
    selectedPMID
    
  })
  
  ## 1.1: First page messages ----
  
  nStudies <- reactive({ 
    nStudies <- selectedPMID() %>% distinct(PMID) %>% nrow()
    nStudies 
    
  })
  
  nHR <- reactive({ 
    nHR <- results2 %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "HR" ) %>% 
      nrow()
    nHR 
    
  })
  
  nOR <- reactive({ 
    nOR <- results2 %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "OR" ) %>% 
      nrow()
    nOR 
    
  })
  
  nRR <- reactive({ 
    nRR <- results2 %>% 
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
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
      labs(y = paste0("Study type")) +
      theme_classic()
    
    year <- selectedPMID() %>% 
      ggplot(aes(x = yearPublication) )  +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) ) +
      labs(x = paste0("Year of publication"))+
      theme_classic()
    
    ggarrange(study, year, ncol = 2)
    
  }
  
  
  output$sutdyYearPlot <- renderPlot({ 
    req(selectedPMID())
    studyYearPlot()
  } )
  
  # 2: Risk factors ####
  
  ## 2.1: results reactive
    
  dataReac <- reactive({ 
    
    results2 %>% 
      filter(PMID %in%selectedPMID()$PMID) %>% 
      filter(str_detect(outcome , input$outcome) )  
  })
  

  ## 2.2: Risk factors table ----
  
  output$table <- DT::renderDataTable(DT::datatable(dataReac()%>% 
                                                      select(input$table1Variables) ) )
  
  

  
  pValPlot <- function(){
 
     dataReac() %>%
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
    
    results2 %>% 
      filter(PMID %in%selectedPMID()$PMID) %>% 
      filter(str_detect(Exposure , input$exposure) )  
  })
  
  # render table outcome ####
  
  output$tableOutcome <- DT::renderDataTable(DT::datatable(dataReacOut()%>% 
                                                      select(input$table2Variables) ) )
  
  # render p value plot
  
  pValPlot2 <- function(){
    
    dataReacOut() %>%
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
