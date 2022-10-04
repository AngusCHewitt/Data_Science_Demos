fluidPage(
  theme = "custom.css",
  h3("Loading images"),
  wellPanel(
    "There are three different approaches to inserting images 
            into Shiny apps, 2/3 depend on images living inside the",
    strong("www/"),
    "folder"
  ),
  tabsetPanel(
    tabPanel(
      "img()",
      fluidPage(
        p("The image below is inserted with the img() function"),
        img(
          src = "images/shiny-logo.png",
          width = "150px"
        )
      )
    ),
    tabPanel(
      "<img>",
      fluidPage(
        p("The image below is inserted with the HTML tag <img>"),
        HTML("<img src='images/shiny-logo.png'
           width='150px'></img>")
      )
    ),
    tabPanel(
      "renderImage / imageOutput",
      fluidPage(
        p("The image below is inserted with imageOutput()"),
        imageOutput("shiny_logo")
      )
    )
  )
)
