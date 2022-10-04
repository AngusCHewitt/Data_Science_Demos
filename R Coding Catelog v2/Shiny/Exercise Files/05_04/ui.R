library("DT")

navbarPage(
  ".httr-oauth | Figshare App",
  tabPanel(
    "Deposited files",
    fluidPage(
      wellPanel("These are the files in the Figshare deposit"),
      DTOutput("deposited_files_table")
    )
  ),
  collapsible = TRUE
)


