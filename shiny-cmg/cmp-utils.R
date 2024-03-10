##  Set up -----------------------------------
library(sf)
library(ggplot2)
library(dplyr)
library(osmdata)
library(crsuggest)
library(sysfonts)
library(showtext)

# Select a Place to Explore

get_place_base_information <- function(place_name) {
  place_name <- opq(place_name) |>   add_osm_feature(key = "boundary",
                                                     value = c("postal_code")) |>
    osmdata_sf()

  place_name
}

select_a_random_point_from_place <- function(place_osmdata_sf) {

  random_point <- place_osmdata_sf$osm_multipolygons |>
    st_make_valid() |>
    st_sample(
              size = 1,
              type = "random")
  random_point
}

determine_best_csr <- function(random_point) {
  plot_crs <- suggest_crs(random_point) |>
    slice_head(n = 1) |>
    pull(crs_gcs)
  plot_crs
}

# Get Location Circle ---------------------------------------------------



make_a_circle_mask <- function(random_point, plot_crs) {
  r <- 5000 #10000
  circle_mask <- st_buffer(random_point, dist = r) |>
    st_transform(crs = plot_crs)
  circle_mask
}

query_osm_data <- function(circle_mask) {
  bbox <- st_bbox(circle_mask)
  query <- opq(bbox = bbox)
  query
}


# Build Artsy Map ---------------------------------------------------------

query_location_attributes <- function(query) {
  water <- query |>   add_osm_feature(key = "natural",
                                      value = "water") |>   osmdata_sf()

  pedestrian <- query |>   add_osm_feature(key = "highway",
                                           value = c("footway", "pedestrian")) |>   osmdata_sf()

  minor_roads <- query |>   add_osm_feature(
    key = "highway",
    value = c(
      "secondary",
      "tertiary",
      "residential",
      "trunk_link",
      "primary_link",
      "motorway_link",
      "secondary_link",
      "tertiary_link"
    )
  ) |>   osmdata_sf()

  main_roads <- query |>   add_osm_feature(key = "highway",
                                           value = c("primary", "trunk", "motorway")) |>   osmdata_sf()

  woods <- query |>   add_osm_features(list("natural" = "wood",
                                            "leisure" = "park")) |>   osmdata_sf()

  #save all the data into a named list

  osm_data_attributes <- list(
    water = water,
    pedestrian = pedestrian,
    minor_roads = minor_roads,
    main_roads = main_roads,
    woods = woods
  )

  return(osm_data_attributes)

}

transform_n_intersect_with_circle <-
  function(osm_data_attributes,
           circle_mask,
           plot_crs) {

    water_circle_masked <-
      osm_data_attributes$water$osm_polygons |>
      st_make_valid() |> #add in case you face 'geom is invalid' error
      st_transform(crs = plot_crs) |>
      st_intersection(circle_mask)

    woods_circle_masked <-
      osm_data_attributes$woods$osm_polygons |>
      st_make_valid() |>
      st_transform(crs = plot_crs) |>
      st_intersection(circle_mask)

    pedestrian_circle_masked <-
      osm_data_attributes$pedestrian$osm_lines |>
      st_make_valid() |>
      st_transform(crs = plot_crs) |>
      st_intersection(circle_mask)

    minor_roads_circle_masked <-
      osm_data_attributes$minor_roads$osm_lines |>
      st_transform(crs = plot_crs) |>
      st_intersection(circle_mask)

    main_roads_circle_masked <-
      osm_data_attributes$main_roads$osm_lines |>
      st_transform(crs = plot_crs) |>
      st_intersection(circle_mask)

    #save all the data into a named list

    osm_data_attributes_masked <- list(
      water = water_circle_masked,
      woods = woods_circle_masked,
      pedestrian = pedestrian_circle_masked,
      minor_roads = minor_roads_circle_masked,
      main_roads = main_roads_circle_masked
    )

    return(osm_data_attributes_masked)
  }



## Map -----------------------------------
make_circle_map <- function(circle_mask,
                            osm_data_attributes_masked,
                            place_name) {
  #set your color palette
  background_fill <- "#292929"
  pedestrian_fill <- "purple"
  forest_fill <- "forestgreen"
  key_color <- "#dcdcdc"
  water_fill <- "lightblue"
  minor_roads_fill <- "#39b9b4"
  mayor_roads_fill <- "violet"


  #add fonts
  font_add_google("Poiret One", "poiret")
  showtext_auto()

  #Plot
  circle_map <- ggplot() +

    geom_sf(data = circle_mask,
            fill = background_fill,
            color = NA) +
    geom_sf(
      data = circle_mask,
      fill = NA,
      color = key_color,
      linewidth = .8
    ) +
    #water
    geom_sf(data = osm_data_attributes_masked$water,
            fill = water_fill,
            color = NA) +

    #woods
    geom_sf(data = osm_data_attributes_masked$woods,
            fill = forest_fill,
            color = NA) +

    #roads
    geom_sf(
      data = osm_data_attributes_masked$pedestrian,
      color = pedestrian_fill,
      linewidth = .3,
      alpha = .3
    ) +
    # Minor Roads
    geom_sf(
      data = osm_data_attributes_masked$minor_roads,
      color = minor_roads_fill,
      linewidth = .4,
      alpha = .8
    ) +

    geom_sf(data = osm_data_attributes_masked$main_roads,
            color = mayor_roads_fill,
            linewidth = .8) +

    labs(caption = paste0("Location: ",
                          place_name$bbox)) +
    theme_void() +


    theme(
      plot.caption = element_text(
        family = "poiret",
        color = key_color,
        size = 14,
        hjust = 0.5
      ),
      plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "in"),
      plot.background = element_rect(fill = background_fill,
                                     color = NA)
    )

  return(circle_map)

}


# Single Workflow -------------------------------------------------------

make_artsy_map_from_place_name <- function(place_name) {

  handlers(handler_txtprogressbar(char = cli::col_red(cli::symbol$heart)))

  p <- progressr::progressor(along = 8)



place_base_information <- get_place_base_information(place_name)

p()


random_point <- select_a_random_point_from_place(place_base_information)

#p(message = sprintf("Step  %g", 2))

plot_csr <- determine_best_csr(random_point)

#p(message = sprintf("Step  %g", 3))

circle_mask <- make_a_circle_mask(random_point, plot_csr)

#p(message = sprintf("Step  %g", 4))

query <- query_osm_data(circle_mask)

#p(message = sprintf("Step  %g", 5))

osm_data_attributes <- query_location_attributes(query)

p(message = sprintf("Step  %g", 6))

osm_data_attributes_masked <- transform_n_intersect_with_circle(
  osm_data_attributes,
  circle_mask,
  plot_csr
)

#p(message = sprintf("Step  %g", 7))

circle_map <- make_circle_map(circle_mask, osm_data_attributes_masked, place_base_information)

#p(message = sprintf("Step  %g", 8))

return(circle_map)

}

# Example -------------------------------------------------------
# library(progressr)
# with_progress(demo_map <- make_artsy_map_from_place_name("Tlalpan, Mexico City")
# )
#

# ggsave(
#   "test_city_circle_map.svg",
#   scale = 1,
#   width = 1500,
#   height = 1500,
#   units = "px",
#   dpi = "retina"
# )

