library(shinydashboard)
titlePanel("Solar Deutschland")

sidebar <- dashboardSidebar(
  selectInput(inputId = "selected_country", label = "1.Select city", choices = sort(unique(sedn_slpc$Stadt))),
  dateRangeInput(inputId = "date", label = "2.Select years", format = "yyyy", startview = "decade", start = "1990-01-01", end = "1990-01-01"),
  numericInput(inputId = "kwhy", label = "3.kWh Jahresverbrauch", value = "4000"),
  numericInput(inputId = "m2", label = "4.Freie Dachfläche", value = "1"),
  numericInput(inputId = "efficency", label = "5.Effizienz Module", value = "0.2", min = 0, max = 1, step = 0.05),
  numericInput(inputId = "cost", label = "6.Kosten pro kWh", value = "0.28"),
  numericInput(inputId = "price", label = "7. Einspeisevergütung", value = "0.1")
)


body <- dashboardBody(
  h3("Durchschnittliche globale Sonneneinstrahlung pro m² im Jahr", align = "center"),
  plotOutput("radiation_chart"),
  hr(),
  fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("yieldm2", width = 6),
      valueBoxOutput("m", width = 6),
      valueBoxOutput("ms", width = 6),
      valueBoxOutput("ev", width = 6),
      valueBoxOutput("es", width = 6),
      valueBoxOutput("ge", width = 6)
))
# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Solar Germany"),
  sidebar,
  body
)
