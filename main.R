library(shiny) 
library(tidyverse) # tody dt 
library(plotly) # interactive plots
library(lme4) # mixed mod
library(splines) # ns fun 
library(shinydashboard)


#liver_dt <- read.csv("data/indian_liver_patient.csv") # handle a df, easier to index 
load("data/fitted_Mixed_mod.rds") # liver mixed model 
load("data/liver_dt.rds") # liver mixed model 
load("data/percentiles_DT.rds") # pred percentiles 

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Health Analytics App"),
  dashboardSidebar(),
  dashboardBody( # value box outputs 
    sidebarLayout(
    sidebarPanel( 
    
    radioButtons("gender_Input_id", "Patient's Gender", c("Male", "Female"), "Male", inline = TRUE), # input patient gender
    
    numericInput("Age_input_id", "Patient's Age", value = median(liver_dt$Age), # test scorres
                 min = 4, max = 90, step = 1), # original value need convert to log scale 
    
    numericInput("Direct_Bilirubin_input_id", "Direct Bilirubin", value = median(liver_dt$Direct_Bilirubin), # test scorres
                 min = min(liver_dt$Direct_Bilirubin), max = max(liver_dt$Direct_Bilirubin), step = 0.1), # original value need convert to log scale 
    
    numericInput("Alkaline_Phosphotase_input_id", "Alkaline Phosphotase", value = median(liver_dt$Alkaline_Phosphotase), # test scorres
                 min = min(liver_dt$Alkaline_Phosphotase), max = max(liver_dt$Alkaline_Phosphotase), step = 1), # original value need convert to log scale 
    
    numericInput("Alamine_Aminotransferase_input_id", "Alamine Aminotransferase", value = median(liver_dt$Alamine_Aminotransferase), # test scorres
                 min = min(liver_dt$Alamine_Aminotransferase), max = max(liver_dt$Alamine_Aminotransferase), step = 1), # original value need convert to log scale 
    
    
    numericInput("Aspartate_Aminotransferase_input_id", "Aspartate Aminotransferase", value = median(liver_dt$Aspartate_Aminotransferase), # test scorres
                 min = min(liver_dt$Aspartate_Aminotransferase), max = max(liver_dt$Aspartate_Aminotransferase), step = 1) # original value need convert to log scale 
    ),
    mainPanel(
    fluidRow(
      valueBoxOutput("pred_Point_est_tb"), # point est 
      valueBoxOutput("percentiles_tb"),
      valueBoxOutput("confidence_Internal_tb")))))) # percentiles 
  
  
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## test if age is above the mx of below min age 
    age_data <- reactive({
      validate(
        need(input$Age_input_id < 91 & input$Age_input_id > 3, message = "Age must be between 4 & 90 year's old")
      )
       return(input$Age_input_id) # return cell value 
      })
    
    
    ## Direct_Bilirubin min/max
    Direct_Bilirubin_dt <- reactive({
      validate(
        need(input$Direct_Bilirubin_input_id < 19.8 & 
        input$Direct_Bilirubin_input_id >= 0.1, message = "Direct Bilirubin must be between 0.1 & 19.7")
      )
      return(input$Direct_Bilirubin_input_id) # return cell value 
    })
    
    
    ## Alkaline_Phosphotase min/max
    Alkaline_Phosphotase_dt <- reactive({
      validate(
        need(input$Alkaline_Phosphotase_input_id < 2111 & 
               input$Alkaline_Phosphotase_input_id >= 63, message = "Direct Bilirubin must be between 63 & 2110")
      )
      return(input$Alkaline_Phosphotase_input_id) # return cell value 
    })
    
    
    
    ## Alkaline_Phosphotase min/max
    Alamine_Aminotransferase_dt <- reactive({
      validate(
        need(input$Alamine_Aminotransferase_input_id < 2001 & 
               input$Alamine_Aminotransferase_input_id >= 10, message = "Direct Bilirubin must be between 10 & 2000")
      )
      return(input$Alamine_Aminotransferase_input_id) # return cell value 
    })
    
    
    
    ## Alkaline_Phosphotase min/max
    Aspartate_Aminotransferase_dt <- reactive({
      validate(
        need(input$Aspartate_Aminotransferase_input_id < 4930 & 
               input$Aspartate_Aminotransferase_input_id >= 10, message = "Direct Bilirubin must be between 10 & 4929")
      )
      return(input$Aspartate_Aminotransferase_input_id) # return cell value 
    })
    
  
  
    ##----------- prob point est value box -----------##
    output$pred_Point_est_tb <- renderValueBox({
      
      ## creae model input df 0- newdata 
      data.frame(Gender = input$gender_Input_id,
                 Age = age_data(), # demos
                 Direct_Bilirubin = log(Direct_Bilirubin_dt() + 1), # medical tests
                 Alkaline_Phosphotase = log(Alkaline_Phosphotase_dt() + 1),
                 Alamine_Aminotransferase = log(Alamine_Aminotransferase_dt() + 1),
                 Aspartate_Aminotransferase = log(Aspartate_Aminotransferase_dt() + 1)) -> pred_DF
    
    # pred prob of liver diesase given new patient's inputs 
    pred_Point_est <- predict(fitted_Mixed_mod, newdata = pred_DF, type = "response") # add standard errors 
    
    valueBox(
      paste0(round(pred_Point_est,2) * 100, "%"), "Probability of liver Disease", icon = icon("list"),
      color = "purple"
    )
    
    })
  
    ##----------- prob Percentiles value box -----------##
    output$percentiles_tb <- renderValueBox({
      
      ## creae model input df 0- newdata 
      data.frame(Gender = input$gender_Input_id,
                 Age = age_data(), # demos
                 Direct_Bilirubin = log(Direct_Bilirubin_dt() + 1), # medical tests
                 Alkaline_Phosphotase = log(Alkaline_Phosphotase_dt() + 1),
                 Alamine_Aminotransferase = log(Alamine_Aminotransferase_dt() + 1),
                 Aspartate_Aminotransferase = log(Aspartate_Aminotransferase_dt() + 1)) -> pred_DF
      
      # pred prob of liver diesase given new patient's inputs 
      pred_Point_est <- predict(fitted_Mixed_mod, newdata = pred_DF, type = "response") # add standard errors 
      
      ## convert data values into percentile, min diff between the percentile cut off point 
      percentiles_DT %>%
        mutate(Diff_value_Percentile = abs(values - pred_Point_est)) %>%
        mutate(min_Diff  = min(Diff_value_Percentile)) %>%
        filter(Diff_value_Percentile == min_Diff) -> percentile_Bucket
      
      
      valueBox(
        paste0(percentile_Bucket$Percentiles), "Percentile", icon = icon("list"),
        color = "yellow"
      )
      
    })
    

    ##----------- prob Confidence Intervals value box -----------##
    output$confidence_Internal_tb <- renderValueBox({
      
      ## creae model input df 0- newdata 
      data.frame(Gender = input$gender_Input_id,
                 Age = age_data(), # demos
                 Direct_Bilirubin = log(Direct_Bilirubin_dt() + 1), # medical tests
                 Alkaline_Phosphotase = log(Alkaline_Phosphotase_dt() + 1),
                 Alamine_Aminotransferase = log(Alamine_Aminotransferase_dt() + 1),
                 Aspartate_Aminotransferase = log(Aspartate_Aminotransferase_dt() + 1)) -> pred_DF
    
      ## test model effects 
      sample <- liver_Log_dt[2:5,] %>%
      select(Gender, Age, Direct_Bilirubin, Alkaline_Phosphotase, 
             Alamine_Aminotransferase, Aspartate_Aminotransferase)
      
      combined_Newdata_sample <- rbind(pred_DF, sample)
      
      ## need atleast 5 obs to generate se.fit, take the first obs resultsm viz an eeorbar , max / min (0,1) 
      pred <- predict(fitted_Mixed_mod, newdata = combined_Newdata_sample, type = "response", se.fit = TRUE) # add standard errors 
      
      ## pred dataset, likelihood & =- standard errors 
      data.frame(pred) %>%
        mutate(lower_bound = fit - se.fit) %>%
        mutate(upper_bound = fit + se.fit) %>%
        mutate(lower_bound = if_else(lower_bound < 0, 0, lower_bound)) %>%
        mutate(upper_bound = if_else(upper_bound > 1, 1, upper_bound)) %>%
        mutate(lower_bound = round(lower_bound,2)*100) %>%
        mutate(upper_bound = round(upper_bound,2)*100) %>%
        mutate(fit = round(fit,2)*100) %>%
        mutate(label = "probablity of liver disease") -> prediction_data
      
      
      valueBox(
        paste0("[", prediction_data$lower_bound[1], "%"," , ",
               prediction_data$upper_bound[1], "%" , "]"), "95% Confidence Interval", icon = icon("list"),
        color = "green"
      )
      
    })
    

  
  
} # end of server



# Run the application 
shinyApp(ui = ui, server = server)
