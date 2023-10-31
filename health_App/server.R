library(shiny)
library(shinydashboard)#Rshinydashboardfuns
library(shinyWidgets)#CSSandJavafuns
library(plotly) # interactive plots
library(tidyverse) # tody dt
library(splines) ## ns 
library(randomNames) # gen radom names
library(formattable) # colour cells 


load("data/app_Sample_dt.rds") # sample dt 
load("mod/diabetes_polr_mod.rds") # plyr mod 


# Define server logic required to draw a histogram
function(input, output) {

  ## Tab 1 -- Patient lookup tb
  ## give usd the abiolity to lookup patient ID, corresponding values 
  output$patient_Lookup_tb <- renderDataTable({
    
    ## create fiction names 
    set.seed(123)
    tb_Lookup_dt <- app_Sample_dt %>%
        mutate(fictional_Names = randomNames(100))
    
    if(input$Select_Tb_vars == "Demographics")
      {
    
      tb_Lookup_dt %>%
        select(ID, fictional_Names, BMI, GenHlth, MentHlth, Age:Income, Sex) %>%
        mutate(Sex = if_else(Sex == 1, "Male", "Female"))
    }
    
    ## true and false health questions 
    else {
      tb_Lookup_dt %>%
        select(-BMI, -GenHlth:-PhysHlth, -Age:-ordinal_Diabetes, -Sex) %>%
        gather("var", "value", -ID, -fictional_Names) %>%
        mutate(value = if_else(value == 1, "Yes", "No")) %>%
        spread(var, value) -> tb_Lookup_dt
      
      ## highlight cells T nnd F values 
      datatable(tb_Lookup_dt) %>% formatStyle(
        colnames(tb_Lookup_dt)[3:15],
        border = '1px solid #ddd',
        target = 'cell',
        backgroundColor = styleEqual(c("Yes", "No"), c('#a6cee3', "#b2df8a")))
    }

  })
  
  
  
  ## -- Tab 2 -- Predicted diabestes cate and interactive mod inputs
  ## patient the ability to see patient input values (precentiles and T/F)
  output$pred_Table <- renderTable({  
  
  ## test pred mod 
  dt <- app_Sample_dt %>%
     filter(ID == input$select_mod_ID)
    
  MASS:::predict.polr(diabetes_polr, dt, type = "p") # ployr mod leverages MASS package      
  })
  
  
}
