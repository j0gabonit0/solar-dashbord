library(shinydashboard)
titlePanel("Solar Deutschland")

sidebar <- dashboardSidebar(
  selectInput(inputId = "selected_country", label = "1.Select city", choices = sort(unique(solar_europe_de_nuts$Stadt))),
  dateRangeInput(inputId = "date", label = "2.Select years", format = "yyyy", startview = "decade", start = "1990-01-01", end = "2010-01-01"),
  numericInput(inputId = "kwhy", label = "3.kWh Jahresverbrauch", value = "0"),
  numericInput(inputId = "m2", label = "Freie Dachfläche", value = "0"),
  numericInput(inputId = "efficency", label = "Effizienz Module", value = "0.2", min = 0, max = 1, step = 0.05)
)


body <- dashboardBody(
  h3("Durchschnittliche globale Sonneneinstrahlung pro m² im Jahr", align = "center"),
  plotOutput("radiation_chart"),
  hr(),
  fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("yieldm2", width = 6),
      valueBoxOutput("m", width = 6)
      ),
  fluidRow(
    valueBoxOutput("ms", width = 6)
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Solar Germany"),
  sidebar,
  body
)

#dashboardHeader(
 # title = Solar Deutschland NUTS
#)