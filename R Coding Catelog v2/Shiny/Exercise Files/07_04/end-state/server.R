library("tidyverse")
library("rhandsontable")
library("lubridate")

function(input, output, session) {
  
  output$time_tracker_rhandsontable <- renderRHandsontable({
    
    tibble("employee_name" = "", 
           "project_name" = "", 
           "hours_worked" = "", 
           "date" = today()) %>%
      rhandsontable(colHeaders = c("Employee Name",
                                   "Project Name", 
                                   "Hours Worked", 
                                   "Date"),
                    rowHeaders = NULL)
    
  })
  
  inputted_time_tracking <- eventReactive(c(input$analyse_data),{
    
    if(input$analyse_data == 0){
      return()
    }
    
    time_tracker <- input$time_tracker_rhandsontable %>%
      hot_to_r() %>%
      as_tibble() %>%
      mutate(hours_worked = as.numeric(hours_worked))
    
    print(time_tracker)
    
  })
  
  output$time_tracker_summary <- renderPlot({
    
    inputted_time_tracking <- inputted_time_tracking()
    
    if(input$analyse_data == 0){
      return()
    }
    
    inputted_time_tracking %>%
      group_by(project_name) %>%
      summarise(total_hours = sum(hours_worked)) %>%
      ggplot(aes(x = project_name,
                 y = total_hours)) +
      geom_col() +
      labs(x = "",
           y = "Total Hours Worked") +
      theme_minimal() +
      coord_flip()
    
  })
  
}
