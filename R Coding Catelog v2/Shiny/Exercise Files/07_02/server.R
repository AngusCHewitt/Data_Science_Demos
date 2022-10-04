library("tidyverse")
library("DT")

bechdel_ratings_2010s <- read_csv("data/bechdel-ratings-2010s.csv")

function(input, output, session) {
  output$download_bechdel_ratings <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_bechdel-ratings.csv")
    },
    content = function(file) {
      bechdel_ratings_2010s %>%
        write_csv(file)
    }
  )

  output$bechdel_ratings_table <- renderDT({
    bechdel_ratings_2010s %>%
      datatable(colnames = c(
        "Movie Title",
        "Year",
        "Profitability",
        "IMDB Rating"
      ))
  })
}
