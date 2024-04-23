
#' UI for Location Leaflet Module
#'
#' @import shiny
#'
#' @param id module id
#'
#' @return shiny.tag.list
#' @export
#'
locLeafletUI <- function(id) {
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        actionButton(ns('testbtn'), "Click Me!")
      ),
      mainPanel(leaflet::leafletOutput(ns('map')))
    )

  )
}

#' Server Function for Location Leaflet Module
#'
#' @param id module id
#'
#' @return shiny reactive
#' @export
#'
locLeafletServer <- function(id) {

  # if (!isTRUE(is.reactive(stores))) {stop("`stores` must be reactive")}
  # if (!isTRUE(is.reactive(comp))) {stop("`comp` must be reactive")}
  # if (!isTRUE(is.reactive(assoc))) {stop("`assoc` must be reactive")}

  moduleServer(id, function(input, output, session) {

    # output$map ----
    output$map <- leaflet::renderLeaflet({

      options(viewer = NULL)
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          options = leaflet::providerTileOptions(
            updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
            updateWhenIdle = TRUE           # map won't load new tiles when panning
          )
        ) %>%
        leaflet::setView(-98.5795, 37.8283, zoom = 5)


    })

  })

}
