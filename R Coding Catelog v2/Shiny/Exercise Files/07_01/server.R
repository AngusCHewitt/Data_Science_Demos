library("tidyverse")
library("kableExtra")

bechdel_ratings_2010s <- read_csv("data/bechdel-ratings-2010s.csv")

function(input, output, session) {
  
  output$bechdel_ratings_table <- function(){
    
    bechdel_ratings_2010s %>%
      kable(col.names = c("Movie Title",
                          "Year",
                          "Profitability",
                          "IMDB Rating")) %>%
      kable_styling(bootstrap_options = "striped")
  }
  
}