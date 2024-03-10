library(shiny)
library(ggplot2)
library(bslib)
library(waiter)
library(dplyr)
source("cmp-utils.R")

world_cities <- maps::world.cities |>
  select(
    name,
    country.etc,
    lat,
    long
  )

ui <- page_navbar(
  id = "inNavbar",
  #useHostess(),
  title = "🌌Shiny Night Lights🌃",
  selected = "How to Use?",
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "cyborg"),
  sidebar = sidebar(
    title = "Type the city your wish to randomly map:",
    markdown(
      mds = c(
        "What is an Alcadia?"
      )
    ),
    textInput(inputId = "city", label = "City", value = "Coyoacan, Mexico City"),
    actionButton(inputId = "query_button", label = "Map it!")
  ),
  nav_panel(
    title = "How to Use?",
    markdown(
      mds = c(
        "Instructions: ",
        "# How to Use?",
        "1. **Enter City**: Type the name of the city you want to map in the text field.",
"2. **Click 'Map it!'**: Press the button to generate the map.",
"3. **Wait**: The map will take a moment to load.",
"4. **Explore**: Once loaded, interact with the map to see details about the city.",
"5. **Enjoy**",
"<br><br>",
"![test image](https://www.iliketowastemytime.com/sites/default/files/best-gifs-pt6-nonono-cat.gif)"
      )
    )
  ),
  nav_panel(
    useWaiter(),
    title = "Map Maker",
    value = "mappy",
    card(
      title = "Map Maker",
      plotOutput("map")
    )
    )
)


server <- function(input, output, session) {

  mymap <- reactiveValues(dmap = "")
  # create a waiter
  w <- Waiter$new(html= spin_pong())

   observeEvent(input$query_button, {

    # updateNavlistPanel(session,
    #                    "inNavbar",
    #                    selected = "mappy")

    w$show()

    place_base_information <- get_place_base_information(input$city)



    random_point <- select_a_random_point_from_place(place_base_information)


    #p(message = sprintf("Step  %g", 2))

    plot_csr <- determine_best_csr(random_point)

    #p(message = sprintf("Step  %g", 3))

    circle_mask <- make_a_circle_mask(random_point, plot_csr)


    #p(message = sprintf("Step  %g", 4))

    query <- query_osm_data(circle_mask)

    #p(message = sprintf("Step  %g", 5))

    osm_data_attributes <- query_location_attributes(query)


    osm_data_attributes_masked <- transform_n_intersect_with_circle(
      osm_data_attributes,
      circle_mask,
      plot_csr
    )

    #p(message = sprintf("Step  %g", 7))

    circle_map <- make_circle_map(circle_mask, osm_data_attributes_masked, place_base_information)

    w$hide()

    mymap$dmap <- circle_map

  })

# when query button is hiit
  # 1. calculate
  # 2. redirect tab
  output$map <- renderPlot({
    mymap$dmap
  })
}

shinyApp(ui, server)


