# Forest plot from AI

# Libraries ---- 

library(RMySQL)
library(data.table)
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinythemes)
library(gridExtra)
library(googlesheets4)
library(tidyr)
library(readxl)
library(shinyWidgets)
library(shinyBS) 
library(mathjaxr)
library(ggpubr)
## for pop ups

# Functons ----

`%notin%` <- Negate(`%in%`)    ### creation operateur "notin



load_all <- function(){

  source("../../../credentials/connection.r")
  
  runStatementStud <- dbSendQuery(connection, "SELECT * FROM studiesfromcode")
  
  general <- dbFetch(runStatementStud, n = -1)  

  dbDisconnect(connection)
  
  source("../../../credentials/connection.r")
  
  runStatementPop <- dbSendQuery(connection, "SELECT * FROM populationsfromcode")
  
  population2 <- dbFetch(runStatementPop, n = -1)  
  
  dbDisconnect(connection)
  
  source("../../../credentials/connection.r")
  
  runStatementRes <- dbSendQuery(connection, "SELECT * FROM resultsfromcode")
  
  result <- dbFetch(runStatementRes, n = -1)  
  
  dbDisconnect(connection)

  source("../../../credentials/connection.r")
  
  runStatementFreq <- dbSendQuery(connection, "SELECT * FROM frequenciesfromcode")
  
  frequencies2 <- dbFetch(runStatementFreq, n = -1)  
  
  dbDisconnect(connection)

  

frequencies <- frequencies2 %>% 
  mutate(measureTypeUnit = paste0(freqType, " (",freqUnit, ")"),
         All_subsets = paste0(subset1," ",subset2," ",subset3 )) %>% 
  left_join(select(general,"AUTHOR" =  author, PMID), by ="PMID") %>% 
  left_join(select(population2, PMID, popCountry, popDisease, popComorbidity ), by = "PMID")



result2 <- result %>% 
  rename(
           "Result" = "result",   "SUBGROUP" = "exp2") %>% 
  left_join(select(general, PMID,author, yearPublication,studyPlan), by = "PMID") %>% 
  rename("Author" = "author") %>% 
  left_join(select(population2, popSize, PMID), by="PMID") %>% 
  rename("pop_size" = "popSize") %>% 
  mutate(expLow1 = ifelse(expType=="no numeric treshold"| expType=="lower than", NA,expLow1 ),
         expHigh1 = ifelse(expType=="no numeric treshold"| expType=="greater than", NA,expHigh1 ),
         expUnit1 = ifelse(is.na(expUnit1),"",expUnit1),
         studyPlan = tolower(studyPlan)
         )

 
  result2[result2 == " "] <- NA
  

  ##### Quelques nouvelle variable
  
  
  final2 <- result2 %>% 
    mutate(No = idResult0,
           AUTHOR = ifelse(str_detect(Author, "et al"), 
                           str_extract(Author,".*(?=et al)"), Author ),
           AUTHOR = ifelse(is.na(AUTHOR), " ", AUTHOR),
           AuthorYear = paste(Author,yearPublication)) %>%  # TSAT 15 ; 25 %
    mutate(exp1 = as.factor(exp1),
           No = as.factor(No),
           icLow2 = ifelse(icLow < -2,-2, NA),         ## penso siano usati solo per i limiti del forest plot
           icUpper2 = ifelse(icUpper>3,3,NA),
           MEASURE = ifelse(str_detect(measureType,"HR"),"HR",
                              ifelse(str_detect(measureType,"OR"),"OR",
                                     ifelse(str_detect(measureType,"RR"),"RR","Other")))) %>% 
    rename("REFERENCE" = refExp1,
           "N" = pop_size,
           "OUTCOME" = outcome)
  
  final <- final2 %>% mutate(across(c(expLow1, expHigh1, N, Result, icLow, icUpper), as.numeric ),
                            log_result = log(Result),
                            OUTCOME = tolower(OUTCOME),
                            exp1 = exp1,
                            expLow1=ifelse(expLow1==0,NA,expLow1),
                            expHigh1=ifelse(expHigh1==0,NA,expHigh1),
                            res_pop = Result*N,
                            SE_HR =  ifelse(MEASURE=="HR" | MEASURE == "OR" | MEASURE == "RR",((log(icUpper) - log(icLow))/3.92),NA_real_),
                            var_HR = SE_HR^2,
                            w_HR = 1/var_HR,   ## weight
                            w_square = w_HR^2, # square of weight
                            res_w_HR = Result*w_HR,
                            signif_pval = ifelse(icLow <1 & icUpper > 1, "> 0.05", "< 0.05 "),  # for funnel plots
                            signif_pval = as.factor(signif_pval)
                            )
  
  population <- population2 %>% left_join(select(general, author, PMID ), by ="PMID" ) %>% 
    mutate(across(c(ageMean, ageSD, ageLow, ageUp,popSize,femaleFreq) , as.numeric),
           popDisease = tolower(popDisease),
           author = ifelse(str_detect(author, "et al"), 
                           str_extract(author,".*(?= et al)"), author ),
           ageLow = ifelse(ageLow == 0, NA, ageLow),
           ageUp = ifelse(ageUp == 0, NA, ageUp),
           ) %>% 
    rename(`Female sex (%)` ="femaleFreq",  "Author" = "author",
           `Age (mean)`  =  "ageMean" , `Age (SD)` ="ageSD") %>% 
    mutate(`Male sex` = 100 - `Female sex (%)`,
           `Age (mean)` = ifelse(`Age (mean)`==0,NA, `Age (mean)`))
  
  

  return(list(general,population, final, frequencies)) 
  
}



server = function(input, output, session) { 
  
  
  general <- load_all()[[1]]
  population <- load_all()[[2]]
  final <- load_all()[[3]]
  frequencies <- load_all()[[4]]
  
  observe({
    updateSelectInput(session, "projectName", choices = final$projectName, selected = head(final$projectName, 1))   ### to have possible inputs updating according to data
  })
  
  observe({
    updateSelectInput(session, "outcome", choices = final$OUTCOME, selected = head(final$OUTCOME, 1))   ### to have possible inputs updating according to data
  })
  
  observe({
    updateSelectInput(session, "expo1", choices = final$exp1, selected = head(final$exp1, 1))
  })
  
  observe({
    updateSelectInput(session, "exclude", choices = final$No)
  })
  
  
  observe({
    updateSelectInput(session, "freqExposure", choices = frequencies$expFrequencyName)
  })
  
  observe({
    updateSelectInput(session, "Columns", choices = list("PMID","EXPOSURE","SUBGROUP","RESULTS","OUTCOME","AUTHOR","N","REFERENCE","MEASURE","AuthorYear","EXP_num" ),
                      selected = c("PMID","EXPOSURE","OUTCOME"))
  })

  
  ## First Page select PMID ####
  

  selectedPMID <- reactive({
    
    proj <- input$projectName
    abs <- input$abstractSearch
    
    if(length(proj) == 0){
      selectedPMID <- general %>% filter(grepl(input$titleSearch, title) )   
      
    } else {
      selectedPMID <- general %>% filter(projectName %in% proj)
    }
    
    selectedPMID
    
  })
  

  ## First page messages ----
  
  nStudies <- reactive({ 
    nStudies <- selectedPMID() %>% distinct(PMID) %>% nrow()
    nStudies 
    
  })
  
  nHR <- reactive({ 
      nHR <- final %>% 
        filter(PMID %in% selectedPMID()$PMID, MEASURE == "HR" ) %>% 
         nrow()
      nHR 
    
  })
  
  nOR <- reactive({ 
    nOR <- final %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "OR" ) %>% 
      nrow()
    nOR 
    
  })
  
  nRR <- reactive({ 
    nRR <- final %>% 
      filter(PMID %in% selectedPMID()$PMID, MEASURE == "RR" ) %>% 
      nrow()
    nRR 
    
  })
  
  output$textStudies <- renderText({ paste0("Your selection contains ",
                                            nStudies() , " articles.") })
  output$textAnalysis <- renderText({ paste0("In the abstracts of the selections are mentioned ",
                                            nHR() , " Hazard Ratios, ",
                                            nOR(), " Odd Ratios and ",
                                            nRR(), " Relative Risks.") })

  ## Selected PMID table ####
  
  output$tableStudies <-  DT::renderDataTable(DT::datatable(selectedPMID() %>%
                                                              select(PMID, author, title, journal)))
  
  ## Selected PMID Plot ####
  
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
  
  
  ## Reactive results filtered on SelectedPMID ####
  
  
  data <-  reactive(
    final %>% mutate(
      expLow1 = round(expLow1,input$round),
      expHigh1 = round(expHigh1,input$round),
      RESULTS = paste(round(Result,input$roundRes)," ","[",round(icLow,input$roundRes),";",round(icUpper,input$roundRes),"]",sep =""),
      EXPOSURE = ifelse(is.na(expLow1) & is.na(expHigh1), paste(exp1),
                        ifelse(is.na(expLow1) & !is.na(expHigh1), paste(exp1,"<=", expHigh1," ",expUnit1,sep=""), # exp = TSAT <= 15 %
                               ifelse(is.na(expHigh1) & !is.na(expLow1), paste(exp1, ">=", expLow1," ",expUnit1, sep=""), # Exp = TSAT >= 15 %
                                      ifelse(!is.na(expHigh1) & !is.na(expLow1) & expHigh1 == expLow1, paste(exp1,"=",expLow1," ",expUnit1, sep=""),
                                             exp1)))), # TSAT 15 ; 25 %
      
        ) %>% 
      filter(PMID %in%selectedPMID()$PMID) %>% 
      filter(studyPlan  %in% input$studyType) %>%
      # filter(ifelse(is.null(input$userID),user_id %in% user_id & exp1 %in% input$expo1 & input$Num1 <= expLow1 &  input$Num2 >= expHigh1 & OUTCOME %in% input$Outcome, 
      #                     user_id %in% input$userID & exp1 %in% input$expo1 & input$Num1 <= expLow1 &  input$Num2 >= expHigh1 & OUTCOME %in% input$Outcome) )
      # 
      filter(str_detect(exp1,input$expo1)  & str_detect(OUTCOME, input$outcome)) %>%
      filter(No %notin% input$exclude)
    
    # ifelse(is.null(input$userID),user_id %in% user_id, user_id %in% input$userID )
    )

  # Q statistcs for Random effect meta ----
  
  data1 <- reactive({ 
    data1 <- data() %>% mutate(number_obs = nrow(data())) %>% 
      mutate(mean_effect = sum(Result)/number_obs,  
             dif_square = (Result - mean_effect)^2 * w_HR )   ## to calculate Q statistics
    data1
    } )
    
  # all data table output ----
  
  output$data <-  DT::renderDataTable(DT::datatable(data2() %>% select(AUTHOR, N , EXPOSURE,  OUTCOME, RESULTS, SUBGROUP,  REFERENCE, MEASURE, "Standard Error (SE)" = SE_HR, 
                                                                      "Variance" = var_HR, "Weighted Estimate" =  res_w_HR,  number_obs, mean_effect, dif_square, TAU2, Vi) ))
  
  
  #### le DF pour le resultat de la meta analyse
  
 # Meta analysis data ----
 
 ES <-  reactive ({ data1() %>% summarise(sum(res_pop,na.rm = TRUE),  ## sum of (effect size * N)
                                         sum(N, na.rm = TRUE),   
                                         sum(res_w_HR,na.rm = TRUE),  ## sum of effects sizes * inverse of variance
                                         sum(w_HR, na.rm = TRUE),  ## sum of the weights
                                         sum(dif_square, na.rm = TRUE),
                                         mean(number_obs, na.rm = TRUE), ## sum of inverse of variances
                                         sum(w_square, na.rm = TRUE) ) %>%    # sum of square of weights   
     rename(pop_n = 1) %>%
     rename(n = 2) %>% 
     rename(sum_res_w = 3) %>% 
     rename(sum_w = 4) %>% 
     rename(Q_stat = 5) %>% 
     rename(df = 6) %>% 
     rename(sum_square_w = 7) %>%   
     mutate(df = df -1 ,  ## degrees of freedom pour random effects, faire gaffe aux RR
            C = sum_w - (sum_square_w/sum_w),  # to calculate Tau Square
            TAU2 = (Q_stat - df)/C , # TAU squared is the between-studies variance
            EXPOSURE ="",
            SUBGROUP = "" , 
            OUTCOME = "",
            AUTHOR = "",
            N = "",
            strata = "",
            REFERENCE = "",
            MEASURE = "",
            ES_w = sum_res_w/sum_w,
            ES_w_SE = sqrt(1/sum_w),
            ES_w_low =  ES_w - 1.96*ES_w_SE,
           # ES_w_low = ifelse(ES_w_low<0,0.01,ES_w_low),
            ES_w_high = ES_w + 1.96*ES_w_SE   ### end fixed effects variables     
            
            ) %>% 
     mutate(RESULT_w =  paste( round(ES_w, 2),"[",round(ES_w_low,2),";",round(ES_w_high,2), "]")) %>% 
     select(-c("EXPOSURE","SUBGROUP","OUTCOME","AUTHOR", "N","strata", "REFERENCE","MEASURE" ))
   })
  
  # metadata table output ----
  
 output$meta <-  DT::renderDataTable(DT::datatable(ES() ))
   
  # Random effect variance into data ----
  
  data2 <- reactive({ 
    data2 <- data1() %>%
      mutate(TAU2 = ES()$TAU2,
             Vi = var_HR + TAU2, 
             Wi_star = 1/Vi,
             weighted_result_random = Result * Wi_star)
    data2
  } )
  
  # Meta random ----
  
  ES_rand <-  reactive ({ 
    
    ES_rand <- data2() %>% summarise(sum(weighted_result_random,na.rm = TRUE), 
                                                sum(Wi_star, na.rm = TRUE)) %>% 
      rename(sum_res_rand_w = 1) %>%
      rename(sum_rand_w = 2) %>% 
      mutate(T_star = sum_res_rand_w / sum_rand_w, # meta analysis estimation with random effects
             rand_var_T_star = 1 / sum_rand_w,
             SE_T_star = sqrt(rand_var_T_star),
             rand_low_lim = T_star - 1.96 * SE_T_star ,
             rand_up_lim = T_star + 1.96 * SE_T_star ,
             RESULT_rand =  paste(round(T_star,2),"[",round(rand_low_lim,2),";",round(rand_up_lim,2), "]"))
    ES_rand
    
    })  
  
  
  output$ES_rand <-  DT::renderDataTable(DT::datatable(ES_rand() ))
  
 # Forest plot ----

  myPlot <- function(){
    
    df_polygon <- reactive({
      data.frame(x = c(ES()$ES_w_high, ES()$ES_w,  ES()$ES_w_low, ES()$ES_w  ), y=c(0,1,0,-1) )
      }) 
    
    ## f =  draw diamond for fixed effect
    
    f <- ggplot(ES()) + 
      geom_polygon(data= df_polygon(), aes(x = x, y=y), color = "grey", fill = "grey") +
      theme_classic()  +
      scale_x_continuous(limits=c(ifelse(min(data()$icLow) < 1, min(data()$icLow), 0.5),
                                  ifelse(max(data()$icUpper)>3,3, max(data()$icUpper))) )+
      geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks = element_blank())
    
    ## d =  draw diamond for random effect meta analysis
    
    df_polygon_wm <- reactive({
      data.frame(x = c(ES_rand()$rand_up_lim, ES_rand()$T_star,  ES_rand()$rand_low_lim, ES_rand()$T_star  ), y=c(0,1,0,-1) )
    }) 
    
    d <- ggplot(ES(), aes(y = 0 , x = ES_r )) + 
      geom_polygon(data= df_polygon_wm(), aes(x = x, y=y), color = "grey", fill = "grey") +
      theme_classic()  +
      scale_x_continuous(limits=c(ifelse(min(data()$icLow) < 1, min(data()$icLow), 0.5),
                                  ifelse(max(data()$icUpper)>3,3, max(data()$icUpper))) )+
      geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks = element_blank())
    
    if(input$meta_res == "yes") {d = d+
      annotate("text", x = input$meta_x, y = 0, label = ES()$RESULTS, size=input$text_size)}
    
    e <- ggplot(data =ES(), aes(y=0)) +
      theme_classic() + 
      theme(axis.title.y=element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_text(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.line.x = element_blank(),
            plot.margin = margin(0, 0.5, 0, 0,unit = "cm"))+
      scale_y_discrete("No") 
      
    
    a <- ggplot(data=data(), aes(y= No, x = Result, xmin=icLow, xmax=icUpper)) +
      geom_point(data=data()) +
      geom_point(data=data(),color = "Black", size = 1*log(data()$N), shape = 15) +
      geom_errorbarh(height=.1) + 
      scale_x_continuous(limits=c(ifelse(min(data()$icLow) < 1, min(data()$icLow), 0.5),
                                  ifelse(max(data()$icUpper)>3,3,max(data()$icUpper))), position = "top") + 
      scale_y_discrete("N") +
      geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5) + 
      theme_classic() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x =element_blank(),
            axis.text.x = element_text(size =(input$title_size),hjust =0.5 , face = "bold",family = input$font),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())+
      geom_segment(aes(y = No , yend = No, x = icLow , xend = icUpper2),
                   arrow = arrow(length=unit(0.30,"cm"))) 
    

    b <- ggplot(data =data(), aes(y=No)) +
      theme_classic() + 
      theme(axis.title.y=element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_text(),
            axis.text.x = element_text(size =(input$title_size),hjust =0,face = "bold", family = input$font),
            axis.title.x = element_blank(),
            plot.margin = margin(0, 0.5, 0, 0,unit = "cm"))+
      scale_y_discrete("No") 
    
    c <- ggplot(data =data(), aes(y=No)) +
      geom_hline(aes(yintercept=N),color="white")+
      scale_colour_identity() +
      theme_classic() + 
      theme(axis.title.y=element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_text(lineheight = 0.3,colour = "white"),
            axis.text.x = element_text(size =(input$title_size),hjust =0, face = "bold", family = input$font),
            axis.title.x = element_blank(),
            axis.ticks.y = element_blank())+
      scale_y_discrete("No") 
    
    # col 4 ----
    
    if(input$plot_col==4) {b = b+
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_rect(aes(xmin = input$X_break_2, xmax = input$X_break_3, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_3, xmax = input$X_break_4, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) + 
      geom_rect(aes(xmin = input$X_break_4, xmax = 12, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_text(aes(x = 0,y=No, label = str_wrap(.data[[input$Columns[1]]], width = 25) ), nudge_x = input$X_break_1,hjust =0,  size = input$text_size, family=input$font)+
      geom_text(aes(x=0,y=No, label = str_wrap(.data[[input$Columns[2]]], width = 25) ), nudge_x = input$X_break_2,hjust =0, size = input$text_size, family=input$font) +
      geom_text(aes(x=0,y=No, label = str_wrap(.data[[input$Columns[3]]], width = 25) ), nudge_x = input$X_break_3,hjust =0, size = input$text_size, family=input$font) +
      geom_text(aes(x=0,y=No, label = str_wrap(.data[[input$Columns[4]]], width = 25) ), nudge_x = input$X_break_4,hjust =0, size = input$text_size, family=input$font)+
      scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_1,input$X_break_2,input$X_break_3,input$X_break_4),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2),
                                           ifelse(input$title_3=="",input$Columns[3], input$title_3),
                                           ifelse(input$title_4=="",input$Columns[4], input$title_4))), 
                         position = "top") 
    
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand(), aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_4,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    else if(input$Meta_YN =="Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_4,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    if(input$Meta_YN == "Random effect") { grid.arrange(b,a,e,d,nrow=2, ncol = 2, widths=c(1.8,1),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") { grid.arrange(b,a,e,f,nrow=2, ncol = 2, widths=c(1.8,1),heights = c(1.8,1/4)) }
    else { grid.arrange(b,a, ncol = 2, widths=c(1.8,1)) }
    
    
    # col 3 ----
    
    } else if(input$plot_col==3){b = b +
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_2, xmax = input$X_break_3, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) + 
      geom_rect(aes(xmin = input$X_break_3, xmax = 9, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) +
      geom_text(aes(x = 0,y=No, label = str_wrap(.data[[input$Columns[1]]], width = 30) ), nudge_x = input$X_break_1,hjust =0,    size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = str_wrap(.data[[input$Columns[2]]], width = 30) ), nudge_x = input$X_break_2, hjust =0, size = input$text_size , family=input$font)+
      geom_text(aes(x=0,y=No, label = str_wrap(.data[[input$Columns[3]]], width = 30) ), nudge_x = input$X_break_3,hjust =0,  size = input$text_size , family=input$font) +
      scale_x_continuous(limits = c(0,9),breaks = c(input$X_break_1,input$X_break_2,input$X_break_3),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2),
                                           ifelse(input$title_3=="",input$Columns[3], input$title_3))),
                         position = "top")
    
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand() , aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_3,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,9),breaks = c(input$X_break_3))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_3,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,9),breaks = c(input$X_break_3))}
    
    
    
    if(input$Meta_YN =="Random effect") { grid.arrange(b,a,e,d,nrow=2, ncol = 2, widths=c(1.2,1),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,e,f,nrow=2, ncol = 2, widths=c(1.2,1),heights = c(1.8,1/4)) }
    else{ grid.arrange(b,a, ncol = 2, widths=c(1.8,1))}
    
    
    # col 2 ----
    
    }  else if(input$plot_col==2)  { b = b +
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_2, xmax = 5.5, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) + 
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[1]]]),nudge_x = input$X_break_1,hjust =0,    size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[2]]]),nudge_x = input$X_break_2,hjust =0,  size = input$text_size , family=input$font) +
      scale_x_continuous(limits = c(0,5.5),breaks = c(input$X_break_1,input$X_break_2),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2))),
                         position = "top")
    
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand(), aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_2,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,5.5),breaks = c(input$X_break_2))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_2,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,5.5),breaks = c(input$X_break_2))}
    
    
    if(input$Meta_YN =="Random effect") { grid.arrange(b,a,e,d,nrow=2, ncol = 2, widths=c(0.7,1),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,e,f,nrow=2, ncol = 2, widths=c(0.7,1),heights = c(1.8,1/4)) }
    else{ grid.arrange(b,a, ncol = 2, widths=c(0.7,1)) }
    
    # col 1 ----
    
    } else if(input$plot_col==1)  { b = b +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[1]]]),nudge_x =input$X_break_1,hjust =0,    size = input$text_size , family=input$font) +
      scale_x_continuous(limits = c(0,2.5),breaks = c(input$X_break_1),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1))),
                         position = "top")
    
    e = e +
      geom_text(aes(x = 0,y=0, label = .data[[input$Columns[1]]]),nudge_x =input$X_break_1,hjust =0,    size = input$text_size) +
      scale_x_continuous(limits = c(0,2.5),breaks = c(input$X_break_1)) 
    
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data  = ES_rand(), aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_1,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,2.5),breaks = c(input$X_break_1))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_1,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,2.5),breaks = c(input$X_break_1))}
    
    
    if(input$Meta_YN =="Random effect") {grid.arrange(b,a,e,d,nrow=2, ncol = 2, widths=c(0.5,1),heights = c(1.8,1/4))}
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,e,f,nrow=2, ncol = 2, widths=c(0.5,1),heights = c(1.8,1/4))}
    else{ grid.arrange(b,a, ncol = 2, widths=c(0.5,1)) }
    
     }  
    
    # col 5 ----
    
    else if(input$plot_col==5) {b = b+
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_rect(aes(xmin = input$X_break_2, xmax = input$X_break_3, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_3, xmax = input$X_break_4, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) + 
      geom_rect(aes(xmin = input$X_break_4, xmax = 12, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[1]]]), nudge_x = input$X_break_1,hjust =0,  size = input$text_size , family=input$font)+
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[2]]]),nudge_x = input$X_break_2,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[3]]]),nudge_x = input$X_break_3,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[4]]]),nudge_x = input$X_break_4,hjust =0, size = input$text_size , family=input$font)+
      scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_1,input$X_break_2,input$X_break_3,input$X_break_4),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2),
                                           ifelse(input$title_3=="",input$Columns[3], input$title_3),
                                           ifelse(input$title_4=="",input$Columns[4], input$title_4))), 
                         position = "top")  
    
    c = c +
      geom_rect(aes(xmin = input$X_break_5, xmax = 2.5, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[5]]]),nudge_x = input$X_break_5, hjust = 0,size = input$text_size , family=input$font) +
      scale_x_continuous(limits = c(0,2.5),breaks = c(input$X_break_5),
                         labels = paste0(c(ifelse(input$title_5=="",input$Columns[5], input$title_5))),
                         position = "top")
    
    ##
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand(),aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_4,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_4,hjust =0,    size = input$text_size) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    
    if(input$Meta_YN =="Random effect") { grid.arrange(b,a,c,e,d,nrow=2, ncol = 3, widths=c(2.2,1,0.5),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,c,e,f,nrow=2, ncol = 3, widths=c(2.2,1,0.5),heights = c(1.8,1/4)) }
    else{grid.arrange(b,a,c, ncol = 3, widths=c(2.2,1,0.5)) }
    ###
    
    } 
    # col 6 ----
    
    else if(input$plot_col==6) {b = b+
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_rect(aes(xmin = input$X_break_2, xmax = input$X_break_3, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_3, xmax = input$X_break_4, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) + 
      geom_rect(aes(xmin = input$X_break_4, xmax = 12, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[1]]]), nudge_x = input$X_break_1 +0.1,hjust =0,  size = input$text_size , family=input$font)+
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[2]]]),nudge_x = input$X_break_2+0.1,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[3]]]),nudge_x = input$X_break_3+0.1,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[4]]]),nudge_x = input$X_break_4+0.1,hjust =0, size = input$text_size , family=input$font)+
      scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_1,input$X_break_2,input$X_break_3,input$X_break_4),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2),
                                           ifelse(input$title_3=="",input$Columns[3], input$title_3),
                                           ifelse(input$title_4=="",input$Columns[4], input$title_4))), 
                         position = "top") 
    
    c = c +
      geom_rect(aes(xmin = input$X_break_5, xmax = input$X_break_6, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[5]]]),nudge_x = input$X_break_5, hjust = 0,size = input$text_size , family=input$font) +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[6]]]), nudge_x = input$X_break_6, hjust = 0,size = input$text_size , family=input$font)+
      scale_x_continuous(limits = c(0,3),breaks = c(input$X_break_5,input$X_break_6),
                         labels = paste0(c(ifelse(input$title_5=="",input$Columns[5], input$title_5),
                                           ifelse(input$title_6=="",input$Columns[6], input$title_6))),
                         position = "top")
    
    ##
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand() ,aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_4,hjust =0,    size = input$text_size , family=input$font) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_4,hjust =0,    size = input$text_size , family=input$font) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    
    if(input$Meta_YN =="Random effect") { grid.arrange(b,a,c,e,d,nrow=2, ncol = 3, widths=c(2.2,1,1),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,c,e,f,nrow=2, ncol = 3, widths=c(2.2,1,1),heights = c(1.8,1/4)) }
    else{grid.arrange(b,a,c, ncol = 3, widths=c(2.2,1,1)) }
    ###
    
    
    }  
    
    # col 7 ----

    else if(input$plot_col==7) {b = b+
      geom_rect(aes(xmin = input$X_break_1, xmax = input$X_break_2, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_rect(aes(xmin = input$X_break_2, xmax = input$X_break_3, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_3, xmax = input$X_break_4, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) + 
      geom_rect(aes(xmin = input$X_break_4, xmax = 12, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[1]]]), nudge_x = input$X_break_1,hjust =0,  size = input$text_size , family=input$font)+
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[2]]]),nudge_x = input$X_break_2,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[3]]]),nudge_x = input$X_break_3,hjust =0, size = input$text_size , family=input$font) +
      geom_text(aes(x=0,y=No, label = .data[[input$Columns[4]]]),nudge_x = input$X_break_4,hjust =0, size = input$text_size , family=input$font)+
      scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_1,input$X_break_2,input$X_break_3,input$X_break_4),
                         labels = paste0(c(ifelse(input$title_1=="",input$Columns[1], input$title_1),
                                           ifelse(input$title_2=="",input$Columns[2], input$title_2),
                                           ifelse(input$title_3=="",input$Columns[3], input$title_3),
                                           ifelse(input$title_4=="",input$Columns[4], input$title_4))), 
                         position = "top")  
    
    c = c +
      geom_rect(aes(xmin = input$X_break_5, xmax = input$X_break_6, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 )+ 
      geom_rect(aes(xmin = input$X_break_6, xmax = input$X_break_7, ymin = -Inf , ymax = Inf),fill = "#FFFFFF", alpha = 0.1 ) +
      geom_rect(aes(xmin = input$X_break_7, xmax = 5, ymin = -Inf , ymax = Inf),fill = input$colours, alpha = 0.1 ) + 
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[5]]]),nudge_x = input$X_break_5, hjust = 0,size = input$text_size , family=input$font) +
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[6]]]), nudge_x = input$X_break_6, hjust = 0,size = input$text_size , family=input$font)+
      geom_text(aes(x = 0,y=No, label = .data[[input$Columns[7]]]), nudge_x = input$X_break_7,hjust =0, size = input$text_size , family=input$font)+
      scale_x_continuous(limits = c(0,5),breaks = c(input$X_break_5,input$X_break_6,input$X_break_7),
                         labels = paste0(c(ifelse(input$title_5=="",input$Columns[5], input$title_5),
                                           ifelse(input$title_6=="",input$Columns[6], input$title_6),
                                           ifelse(input$title_7=="",input$Columns[7], input$title_7))),
                         position = "top")

    ##
    if(input$Meta_YN =="Random effect") {
      e = e +
        geom_text(data = ES_rand(), aes(x = 0,y=0, label = RESULT_rand),nudge_x =input$X_break_4,hjust =0,    size = input$text_size , family=input$font) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    else if(input$Meta_YN == "Fixed effect") {
      e = e +
        geom_text(aes(x = 0,y=0, label = RESULT_w),nudge_x =input$X_break_4,hjust =0,    size = input$text_size , family=input$font) +
        scale_x_continuous(limits = c(0,12),breaks = c(input$X_break_4))}
    
    
    if(input$Meta_YN =="Random effect") { grid.arrange(b,a,c,e,d,nrow=2, ncol = 3, widths=c(2.2,1,1),heights = c(1.8,1/4)) }
    else if(input$Meta_YN == "Fixed effect") {grid.arrange(b,a,c,e,f,nrow=2, ncol = 3, widths=c(2.2,1,1),heights = c(1.8,1/4)) }
    else{grid.arrange(b,a,c, ncol = 3, widths=c(2.2,1,1)) }
    ###
        
    
    }  ### end col 7
       }
    
    output$forestplot = renderPlot({ 
      req(data())
      myPlot()
      } )
  
    ## Forestplot  renderUI ----
    
      output$forestplotDim <- renderUI(
      if(nrow(data())<=10){
        plotOutput("forestplot",width = "95%",height = "400px")
      } else if(nrow(data())<=20){
        plotOutput("forestplot",width = "95%",height = "640px")
      } else {
        plotOutput("forestplot",width = "95%",height = "800px")
      }
    )
   
    
    # Funnelplot ----
    
    
funnel_fun <-  function() {
  
  funnel <- ggplot(data = data(), aes(x = Result, y = SE_HR, color = signif_pval, label = AUTHOR) ) +
    geom_point(  size = input$funnel_point_size ) + 
    theme_classic() + 
    geom_vline(data = ES(), xintercept = ES()$ES_w, linetype = "dashed") + 
    geom_vline(data = ES(), xintercept = 1) + 
    geom_segment( aes(x = ES()$ES_w, y = 0, xend= ES()$ES_w + 1.96 * max(data()$SE_HR, na.rm = T), yend= max(data()$SE_HR, na.rm = T) ), linetype = "dashed", color = "grey") +
    
    geom_segment( aes(x = ES()$ES_w, y = 0, xend= ifelse( ES()$ES_w - 1.96 * max(data()$SE_HR, na.rm = T) < 0 ,0, ES()$ES_w - 1.96 * max(data()$SE_HR, na.rm = T)  ), ## IC95% du funnel plot potentialement < 0
                      yend= max(data()$SE_HR, na.rm = T) ), linetype = "dashed", color = "grey") +
    labs(x = "Effect size", y = "Standard error of effect size", color = "p-value",
         caption = "--- Dashed line correspond to the global estimate for fixed effects Meta analysis, \n and it's confidence interval according to the SE values") +
    scale_y_continuous(trans = "reverse") +
    scale_x_continuous(  limits = c(input$limit1, input$limit2), 
                        trans=  ifelse(input$log_scale == "Log scale","log","identity" ) ) + 
    theme(axis.text=element_text(size=input$text_size2),
          axis.title=element_text(size=input$text_size2),
          plot.caption = element_text(hjust = 0, size = input$text_size2),
          legend.text  = element_text(size = input$text_size2),
          legend.title = element_text(size = input$text_size2) ) 
  
  if(input$label_f== "no" ) {
    funnel
  } else  { 
    funnel + geom_text(hjust=0, vjust=0 , size = input$funnel_point_size) 
  }
  
}
    
    
    output$funnelplot = renderPlot({ 
      req(data())
      funnel_fun()
      
    } )
    
    output$down_funnel <- downloadHandler(
      filename = "funnelplot.png",
      content = function(file) {
        ggsave(file, funnel_fun(),  width = 11, height = 8, dpi = 150, units = "in", device='png' )  ## la fonction png ne marchait pas pour produire le plot
        
      }
    )     ## remember, it only works in the browser
    
     
    
    
    ### donload plot and data
   
    output$down <- downloadHandler(
      filename = "forestplot.png",
      content = function(file) {
        png(file, width = 1200, height = 600, units = "px")
        myPlot()
        dev.off()
      }
    )
    
    data_dld <-  reactive({
      data_dld <-  data() %>%  select(Author,yearPublication,N,exp1, expLow1,expHigh1, expUnit1, REFERENCE, OUTCOME, Result, icLow, icUpper,MEASURE,
                                      SUBGROUP) # 
      data_dld
    })
    
    output$tbl_data <- downloadHandler(
      filename = "Results_table.csv",
      content = function(file) {
        fwrite(data_dld(),file) }  )  ## write CSV ne marchait pas car une colonne dans data() doit avoir un format liste
  

  
  
  # General tbl output ----
  
  output$tableDT <-  DT::renderDataTable(
    DT::datatable(general %>% 
                    select(PMID, author, yearPublication, title,objective,
                           studyPlan
                    ) %>% 
                   filter(PMID %in% data()$PMID))  )
    
  output$tableAbstract <-  DT::renderDataTable(DT::datatable(general %>% select(PMID,author, ABSTRACT) %>% 
                                                           filter(PMID %in% data()$PMID))  )
  
  ## Sortir certaines colonnes de la table
  
  stud_reac <- reactive(general  %>% 
                          filter(PMID %in% data()$PMID))
  
  output$tbl_study_download <- downloadHandler(
    filename = "Studies_table.csv",
    content = function(file) {
      write.csv(stud_reac(),file) }  )
  
  
  # Population tbl output ----
 
  output$tableDT2 <-  DT::renderDataTable(
    DT::datatable(population %>% 
                    filter(PMID %in% data()$PMID) %>% 
                    select(PMID, Author,"COUNTRY" = popCountry,"Population Size" = popSize,
                           `Age (mean)`, ageLow, ageUp, `Female sex (%)`, "Population pathology" = popDisease) 
                  ))
  
  pop_reac <- reactive(population %>% 
                         filter(PMID %in% data()$PMID))
  
 
  
  
   output$tbl_pop_download <- downloadHandler(
    filename = "Population_table.csv",
    content = function(file) {
      write.csv(pop_reac(),file) }  )
  
  
  
  
  # Barplot Sex ----
  
  output$barplot = renderPlot({ if(input$plot_sex == "Female") {
    ggplot(pop_reac()%>% filter(!is.na(Author)) ,aes(x=.data[[input$xAxis]], fill = .data[[input$sexFill]])) + 
      geom_col(aes(y =`Female sex (%)`)) +
      theme_classic() +
      labs(y = "Female sex (%) ") +
      theme(axis.title.y = element_text(face ="bold",size =16),
            axis.title.x = element_text(face ="bold",size =16),
            axis.text.x = element_text(face ="bold",size =12, angle = input$xAxisAngle, hjust = 1),
            axis.text.y = element_text(face ="bold",size =12),
            legend.title = element_text( face = "bold", size = 16),
            legend.text=element_text(size=14)) +
      scale_color_manual(values=c("#FFCCFF"))
  }
    else {
      ggplot(pop_reac() %>% filter(!is.na(Author)),aes(x=.data[[input$xAxis]], fill = .data[[input$sexFill]])) + 
        geom_col(aes(y =`Male sex`)) +
        theme_classic() +
        labs(y = "Male sex (%) ") +
        theme(axis.title.y = element_text(face ="bold",size =16),
              axis.title.x = element_text(face ="bold",size =16),
              axis.text.x = element_text(face ="bold",size =12, angle = input$xAxisAngle, hjust = 1),
              axis.text.y = element_text(face ="bold",size =12),
              legend.title = element_text( face = "bold", size = 16),
              legend.text=element_text(size=14)) +
        scale_color_manual(values=c("#99FFFF"))
      
    }
    
  })  ## end barplot
  
  # Boxplot Age ####
   
  output$boxplot = renderPlot({ 
    ggplot(pop_reac(),aes(x=.data[[input$xAxis]], color = .data[[input$sexFill]] )) + 
      geom_errorbar(aes(ymin = ageLow, ymax =ageUp)) +
      geom_point(aes(y = `Age (mean)`), size = 3)+ 
      theme_classic() +
      labs(y = "Male & female age (mean ; SD)") +
      theme(axis.title.y = element_text(face ="bold",size =16),
            axis.title.x = element_text(face ="bold",size =16),
            axis.text.x = element_text(face ="bold",size =12, angle = input$xAxisAngle, hjust = 1),
            axis.text.y = element_text(face ="bold",size =12),
            legend.title = element_text( face = "bold", size = 16),
            legend.text=element_text(size=14)) 


    
  })  ## end boxplot
  
  



  
  
} # end of server


# UI ----

############################################## APP
############################################

ui = navbarPage(theme =shinytheme("sandstone"),  ### navbar pour la barre de navigation
                title =(div(h1("SCI Results", style = 'font-size:30px;'))),
                
                
                # Research equation ---- 
                
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
                
                tabPanel(h5("Make your forest plot"),   ### on insere le premier panneau de la barre de navigation et son titre
                         column(3,wellPanel(  div(class = "row",                             ### la partie rose permets de mettre plusieurs composante dans le meme endroit (les 3 sliders)
                           textInput(inputId = "expo1",
                                       label = "Exposure",
                                       # "Names",
                                       # multiple = TRUE
                                     ),
                           textInput(inputId = "outcome",
                                       label = "Outcome",
                                       # multiple = TRUE,
                                       # "Names"
                                     ),
                            style = "padding: 2px; ")
                           
                         )),
                         
                         
                         column(2, wellPanel(div(class = "row",
                                             selectInput(inputId = "plot_col",
                                                          label = "Number of columns",
                                                          choices= c("1","2",
                                                                     "3","4","5","6","7"),
                                                         selected = "3"),
                                              selectInput(inputId = "Columns",
                                                          label = "Choose columns",
                                                          multiple = TRUE,
                                                          choices = "Names"),
                                              style = "padding: 2px; " )
                                             )),
                                            
            column(2,wellPanel(div(class = "row",
                                            selectInput(inputId = "exclude",
                                                        label = "Exclude rows",
                                                        "Names",
                                                        multiple = TRUE),
                                             numericInput(inputId = "text_size",
                                                         label = "Plot text size",
                                                         value = 7), 
                                            style = "padding: 2px;" )
                                                        )),
            column(3,wellPanel( 
              div(
                selectInput(inputId = "studyType",
                            label = "Filter for study plans",
                            choices = c("meta analysis", "cohort", "clinical trial", "review" ,"other"  ),
                            selected = c("meta analysis", "cohort", "clinical trial","review" ,"other" ),
                            multiple = TRUE
                ),
                actionButton("params", "Customize plot", class = "btn-primary",style='padding:10px; font-size:80%; margin: 5px; display: inline-block;vertical-align:top;'),
                
                style = "padding: 5px; display: inline-block; vertical-align:top;" )
              
            )),
            column(2,wellPanel( 
              div(
                
                downloadButton(outputId = "down",label = "Download plot",style='padding:10px; font-size:80%;margin: 5px;' , class = "btn-secondary"),
                downloadButton(outputId = "tbl_data", label = "Download data",style='padding:10px; font-size:80%;margin: 5px;' , class = "btn-secondary"),
                style = "padding: 5px; display: inline-block; vertical-align:top;" )
              
            )),
                         bsModal(id = "PU_params", title = "Plot parameters", trigger = "params", size = "large" ,    ### pop_up window
                                 column(2,wellPanel(numericInput(inputId = "X_break_1",
                                              label = "X-axis break N. 1",
                                              value = 0.2),
                                 numericInput(inputId = "X_break_2",
                                              label = "X-axis break N. 2",
                                              value = 2.2),
                                 numericInput(inputId = "X_break_3",
                                              label = "X-axis break N. 3",
                                              value = 6.4),
                                 numericInput(inputId = "X_break_4",
                                              label = "X-axis break N. 4",
                                              value = 9.5)),
                                 selectInput(inputId = "font",
                                             label = "Plot text font",
                                             choices = c("","Times New Roman","mono", "sans", "Comic Sans MS") )
                                             
                                 ),
                         column(2,wellPanel(numericInput(inputId = "X_break_5",
                                                         label = "X-axis break N. 5",
                                                         value = 0),
                                            numericInput(inputId = "X_break_6",
                                                         label = "X-axis break N. 6",
                                                         value = 1.5),
                                            numericInput(inputId = "X_break_7",
                                                         label = "X-axis break N. 7",
                                                         value = 3.5))),
                         column(3, wellPanel(selectInput(inputId = "colours",
                                                         label = "Background colors, odd columns",
                                                         choices = c("White" = "#FFFFFF", "Grey" = "#CCCCCC")),
                                             numericInput(inputId = "title_size",
                                                          label = "Axis title size",
                                                          value = 20),
                                             selectInput(inputId = "Meta_YN",
                                                         label = "Show Meta analysis",
                                                         choices =  c("Random effect","Fixed effect","No"),
                                                         selected = "No"),
                                             selectInput(inputId = "meta_res",
                                                         label = "Custom position of Meta Result",
                                                         choices = c("no","yes")),
                                             numericInput(inputId = "meta_x",
                                                          label = "X-axis, Meta Result",
                                                          value = 2),
                                             numericInput(inputId = "round",
                                                          label = "Round exposure ",
                                                          value = 0),
                                             numericInput(inputId = "roundRes",
                                                          label = "Round results ",
                                                          value = 2)
                                             )),
                         column(2, wellPanel(
                                             textInput(inputId = "title_1",
                                                       label = "Label column 1"),
                                             textInput(inputId = "title_2",
                                                       label = "Label column 2"),
                                             textInput(inputId = "title_3",
                                                       label = "Label column 3"),
                                             textInput(inputId = "title_4",
                                                       label = "Label column 4"),
                                             textInput(inputId = "title_5",
                                                       label = "Label column 5"),
                                             textInput(inputId = "title_6",
                                                       label = "Label column 6"),
                                             textInput(inputId = "title_7",
                                                       label = "Label column 7")))
                         ),  # end BS modal
                                 
                               uiOutput("forestplotDim"),
                         br(),
                         
                ),
                
          # Funnel plot tab ----  
                
                tabPanel(h5("Funnel plot"),
                         column(2, wellPanel(
                         numericInput(inputId = "limit1",
                                      label = "Plot lower limit, X",
                                      value = 0.5),
                         numericInput(inputId = "limit2",
                                      label = "Plot upper limit, X",
                                      value = 3)
                          
                         )), 
                         column(2, wellPanel(
                           numericInput(inputId = "text_size2",
                                        label = "Axis label size",
                                        value = 16),
                           numericInput(inputId = "funnel_point_size",
                                        label = "Point size",
                                        value = 5)
                         )),
                         column(2, wellPanel( 
                           selectInput(inputId = "label_f",
                                       label = "Show Authors",
                                       choices = c("no", "yes")),
                           selectInput(inputId = "log_scale",
                                       label = "X axis on log scale",
                                       choices= c("Normal scale", "Log scale"))
                           )),
                         column(2, wellPanel( 
                           downloadButton("down_funnel", "Download funnel plot")
                         )),
                         plotOutput("funnelplot",width = "60%")
                         
                        ),
                
          # Study desc tab ----       
                
                tabPanel(h5("Study description"),
                         DT::dataTableOutput(("tableDT")),
                         br(),
                         br(),
                         DT::dataTableOutput("tableAbstract"),
                        
                         downloadButton(outputId = "tbl_study_download", label = "Download Table")
                         
                         ),
                
          # Pop desc tab ---- 
                
                tabPanel(h5("Population description"),
                         DT::dataTableOutput(("tableDT2")),
                         downloadButton(outputId = "tbl_pop_download", label = "Download Table"),
                         br(),
                         br(),
                         selectInput(inputId = "plot_sex",
                                     label = "Plot sex",
                                     choices= c("Male","Female")),
                         selectInput(inputId = "xAxis",
                                     label = "X axis",
                                     choices= c("Author","PMID")),
                         numericInput(inputId = "xAxisAngle",
                                      label = "X axis rotation",
                                      value = 45),
                         selectInput(inputId = "sexFill",
                                     label = "Colour",
                                     choices= c("popDisease"),
                                     selected = "popDisease"),
                         plotOutput("barplot", width = "70%"),
                         br(),
                         br(),
                         br(),
                         plotOutput("boxplot", width = "70%")),
          
          # Meta analysis data ----

          tabPanel(h5("Data"),
                   h3("Find below all calculations for fixed effect and random effect meta analysis according to the formulas in he next page"),
                         DT::dataTableOutput("data"),
                         
                         DT::dataTableOutput("meta"),
          
                         DT::dataTableOutput("ES_rand")),
          
          # Formulas ----
          
          tabPanel(h5("Formulas"),
                   strong(h1("Fixed Effect method for meta- analysis")),
                  div( style = "font-size:25px",
                       withMathJax(),
                   p("1) Estimate the standard error (SE) from a result and its confidence interval at 95% (CI 95%):"),
                   p("For example:"),
                   p("CI 95% upper bound (UB) and lower bound (LB) for an HR are estimated as follows:  "),
                   p("$$\\mathrm{UB}=e^{\\log (\\mathrm{HR})+1.96 \\times \\mathrm{SE}}$$") ,  ## Upper bound formula
                   p("$$\\mathrm{LB}=e^{\\log (\\mathrm{HR})-1.96 \\times \\mathrm{SE}}$$") ,  ## Lower bound formula
                   p("$$S E=\\frac{(\\ln (U B)-\\ln (L B))}{2 \\times 1.96}$$"), # Standard Error
                   p("2) Once the standard error for each effect estimate (HR, OR, RR) is obtained, a weighted estimate is calculated for each study using as weight \\(1/SE^2\\) "),
                   p("3) Then, the global weighted estimate (ES_w) is obtained as follows:  "),
                   p("$$\\left(\\mathrm{ES}_{-} \\mathrm{w}\\right)=\\frac{\\sum Y_{i}\\left(\\frac{1}{S E_{i}^{2}}\\right)}{\\sum\\left(\\frac{1}{S E_{i}^{2}}\\right)}$$"),
                   p("Yi is the intervention effect estimated in the ith study, SEi is the standard error of that estimate, and the summation is across all studies "),
                   p("4) In order to estimate the CI 95% of the global weighted estimate, we need it's standard error."),
                   p("The variance of the weighted average is defined as the reciprocal of the sum of the weights: " ) , 
                   p("$$\\operatorname{Var}(E S \\text { w })=\\frac{1}{\\sum\\left(\\frac{1}{S E_{i}^{2}}\\right)}$$"),
                   p("The standard error of the combined effect (ES_w_SE) will then be the square root of the variance."),
                   br(),
                   strong(h1("Random Effect method for meta- analysis")),
                   p("The random effect meta-analysis follow the hypothesis that there is a distribution of true effect sizes. The combined effect to be estimated will reresent the mean of the population of true effects "),
                   p("In order to estimate the combined effect we need to weight all the estimates not only following the variance within each study but also the variance between the studies "),
                   p("Calculate the Q statistics representing the total variance (within studies + between studies) "),
                   p("$$ Q=\\sum_{i=1}^k w_i\\left(Y_i-\\bar{Y} \\cdot\\right)^2$$"),
                   p("Degrees of fredom (df) = Number of Studies - 1"),
                   p("$$Tau^2 = \\frac{Q - df} {C}$$"),
                   p("Where $$ C=\\sum w_i-\\frac{\\sum w_i^2}{\\sum w_i}$$"),
                   p("The weight for the random effect meta analysis will then be $$W_i^*=\\frac{1}{V_i^*}$$ " ),
                   p("Where $$V_i^* = w_i + Tau^2$$"),
                   p("The combined effect is then computed as:"),
                   p("$$\\bar{T}_{\\cdot}^*=\\frac{\\sum_{i=1}^k W_i^* T_i}{\\sum_{i=1}^k W_i^*}$$"),
                   p("The standard error needed to compute the CI 95% of the combined effect is obtained as the square of the combined effect variance which is defined as the reciprocal of the sum of the weights"),
                   
                  
                   
                  ) # end div
                   ) # end tab panel   
          
          ) # end navbar page





shinyApp(ui = ui, server = server) # perform app launch


