library("rfigshare")
library("tidyverse")

## See https://doi.org/10.6084/m9.figshare.3761562 for details
deposit_details <- fs_details(3761562)
deposit_files <- unlist(deposit_details$files)
deposit_files <-
  data.frame(split(deposit_files, names(deposit_files)), stringsAsFactors = F)

function(input, output, session) {
  output$deposited_files_table <- renderDT({
    deposit_files %>%
      datatable()
  })

}