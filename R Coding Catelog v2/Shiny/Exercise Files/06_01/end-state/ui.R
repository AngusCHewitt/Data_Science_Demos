library("DT")

navbarPage(
  "Populating menus | % of populatiion using the internet",
  tabPanel(
    "Without updateSelectInput()",
    fluidPage(
      selectInput("select_country_without_updateSelectInput",
                  label = "Select a country:",
                  choices = c("Afghanistan", "Albania", "Angola", "Argentina", "Austria", 
                              "Australia", "Bosnia and Herzegovina", "Bangladesh", "Belgium", 
                              "Burkina Faso", "Bulgaria", "Bahrain", "Burundi", "Benin", "Bolivia", 
                              "Brazil", "Botswana", "Canada", "Congo, Dem. Rep.", "Central African Republic", 
                              "Congo, Rep.", "Switzerland", "Cote d'Ivoire", "Chile", "Cameroon", 
                              "China", "Colombia", "Costa Rica", "Cuba", "Czech Republic", 
                              "Germany", "Djibouti", "Denmark", "Dominican Republic", "Algeria", 
                              "Ecuador", "Egypt, Arab Rep.", "Eritrea", "Spain", "Ethiopia", 
                              "Finland", "France", "Gabon", "United Kingdom", "Ghana", "Gambia, The", 
                              "Guinea", "Equatorial Guinea", "Greece", "Guatemala", "Guinea-Bissau", 
                              "Hong Kong SAR, China", "Honduras", "Croatia", "Haiti", "Hungary", 
                              "Indonesia", "Ireland", "Israel", "India", "Iraq", "Iran, Islamic Rep.", 
                              "Iceland", "Italy", "Jamaica", "Jordan", "Japan", "Kenya", "Cambodia", 
                              "Comoros", "Korea, Rep.", "Kuwait", "Lebanon", "Sri Lanka", "Liberia", 
                              "Lesotho", "Libya", "Morocco", "Montenegro", "Madagascar", "Mali", 
                              "Myanmar", "Mongolia", "Mauritania", "Mauritius", "Malawi", "Mexico", 
                              "Malaysia", "Mozambique", "Namibia", "Niger", "Nigeria", "Nicaragua", 
                              "Netherlands", "Norway", "Nepal", "New Zealand", "Oman", "Panama", 
                              "Peru", "Philippines", "Pakistan", "Poland", "Puerto Rico", "West Bank and Gaza", 
                              "Portugal", "Paraguay", "Romania", "Serbia", "Rwanda", "Saudi Arabia", 
                              "Sudan", "Sweden", "Singapore", "Slovenia", "Slovak Republic", 
                              "Sierra Leone", "Senegal", "Somalia", "Sao Tome and Principe", 
                              "El Salvador", "Syrian Arab Republic", "Eswatini", "Chad", "Togo", 
                              "Thailand", "Tunisia", "Turkey", "Trinidad and Tobago", "Tanzania", 
                              "Uganda", "United States", "Uruguay", "Venezuela, RB", "Vietnam", 
                              "Yemen, Rep.", "South Africa", "Zambia", "Zimbabwe")),
      plotOutput("internet_use_without_updateSelectInput")
    )
  ),
  tabPanel(
    "With updateSelectInput()",
    fluidPage(
      selectInput("select_country_with_updateSelectInput",
                  label = "Choose a country:",
                  choices = NULL),
      plotOutput("internet_use_with_updateSelectInput")
    )
  ),
  collapsible = TRUE
)