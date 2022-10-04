library("DT")

navbarPage(
  "Allow users to download data | Bechdel Test",
  tabPanel(
    "Movie Ratings",
    fluidPage(
      wellPanel(
        "The table below shows the profitability and IMDB ratings for all movies that passed with Bechdel test in an analysis performed by",
        a(
          href = "https://www.google.com/search?q=fivethirtyeight+bechdel+test&oq=fivethirtyeight+bechdel&aqs=chrome.0.0j69i57.4936j0j7&sourceid=chrome&ie=UTF-8",
          "FiveThirtyEight.com."
        ),
        p("Click the download button below to download the data to your machine")
      ),
      downloadButton(
        "download_bechdel_ratings",
        "Download data"
      ),
      hr(),
      DTOutput("bechdel_ratings_table")
    )
  ),
  collapsible = TRUE
)
