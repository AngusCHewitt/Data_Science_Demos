library("tidyverse")
library("readxl")
library("writexl")

function(input, output, session) {
  
  output$download_time_tracker_template <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_template-time-tracker.xlsx")
    },
    content = function(file){
      file.copy("data/template-time-tracker.xlsx", file)
    }
  )
  
  output$time_tracker_summary <- renderPlot({
    
    # uploaded_time_tracker %>%
    #   group_by(`project name`) %>%
    #   summarise(total_hours = sum(`hours worked`)) %>%
    #   ggplot(aes(x = fct_reorder(`project name`, total_hours),
    #              y = total_hours)) +
    #   geom_col() +
    #   labs(x = "",
    #        y = "Total Hours Worked") +
    #   theme_minimal() +
    #   coord_flip()
    
  })
  
}