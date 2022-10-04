library(shiny)

ui <- shiny::navbarPage(
  title="Data Science App", collapsible = FALSE, # tab name 
  
  tabPanel("Intro",
           
           br(), # line breaks
           hr(), # header line 
           
           h1(HTML("<b>Case Study Description</b>"), # page title 
              style="text-align:center; color: grey;"), # title style
           hr(),
           div( # HTML tags 
             style = "width: 75%; margin: auto", # text style within this paragraph
             h4(HTML("<i> A hypothetical Indonesian micro-finance company enters a partnership with a teleco to provide clients with micro loans to purchase pre-paid mobile phone credit.
Prior to offering these loans they contract the services of a data scientist consulting firm D^3 Analytics to segment the telco's current clients into meaningful risk profiles so they can tailor products accordingly. 
The business also wants an interative Prediction App which can predict a new clients risk profiles and there probability of repaying the loan on time. The teleco has provided 3 months worth of
clients pre-paid credit history data</i>"),
                style="text-align:left"),
             br(), # line breaks
             h4(HTML("<b> 3^D analytics has defined the business problems into 3 questions and the corresponding outputs generated the App; </b>"),
                style="text-align:left"),
           
           br(), # line breaks
           h4(HTML("<b> Q1 </b> <i> Can the telecom clients be clustered into meaningful risk profile ?
                   </p> <b> Outputs: </b> <i> Each clusters probabilty-of-success distribution and their main distiguishing features</i>"),
              style="text-align:left"),
      

           br(), # line breaks
           h4(HTML("<b> Q2 </b> <i> Using new clients previous teleco credit history is it possible to predict their risk profile cluster </i> 
                    <p> <b> Q3 </b> <i> and given that risk profile what their likelihood is of repaying the loan on time ? </i> 
                    </p> <b> Outputs: </b> <i>prediction app which will allow for the input in new client info and use it to predict their risk profilec cluster. If more infomation is needed on the clients 
                    probability of repaying the loan a prediction model will also provide a expected probability that the client will repay the loan </i>"),
              style="text-align:left")
           
           
  )
  )
  )


server <- function(input, output,session) {
  
}


shinyApp(ui = ui, server = server)


