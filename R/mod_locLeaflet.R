
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
        fluidRow(
          column(
            width = 12,
            style = "padding-right: 5px !important; padding-left: 5px !important;",
            storeLocUI(ns('store_loc')),
            compLocUI(ns('comp_loc'))
          )
        )
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

  moduleServer(id, function(input, output, session) {

    # ________ ----
    # modules ----

    # * store_loc ----
    store_loc <- storeLocServer('store_loc')

    # * comp_loc ----
    comp_loc <- compLocServer('comp_loc')

    # _______ ----
    # outputs ----

    # * map ----
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

    # _____________ ----
    # module output ----
    list(store_loc = store_loc, comp_loc = comp_loc)

  })

}
