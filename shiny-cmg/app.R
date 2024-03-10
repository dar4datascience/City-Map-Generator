library(shiny)
library(ggplot2)
library(bslib)


ui <- page_navbar(
  title = "🌌Shiny Night Lights🌃",
  selected = "How to Use?",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Data from X to Y",
    markdown(
      mds = c(
        "What is an Alcadia?"
      )
    ),
    radioButtons(
      inputId = "radio-cdmx",
      label = "Select View",
      choices = list("CDMX" = "cdmx", "Alcaldia" = "alcaldia"),
      width = "100%"
    ),
    selectInput(
      inputId = "alcaldia-selection",
      label = "Select Alcaldia",
      choices = list("choice a" = "a", "Value2" = "value2"),
      selected = "a"
    )
  ),
  tabPanel(
    title = "Home",
    markdown(
      mds = c(
        "hello _world_ ",
        "# hellow ",
        "## world"
      )
    )
  ),
  tabPanel(
    title = "Map Maker",
    "Working on it"
  )
)


server <- function(input, output) {

}

shinyApp(ui, server)


