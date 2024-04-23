
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
#' @import shiny
#' @importFrom rlang .data
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

    # ________ ----
    # reactive values ----

    # * store_ids ----
    store_ids <- reactiveValues(curr = vector('character'), add = vector('character'), drop = vector('character'))

    # * comp_ids ----
    comp_ids <- reactiveValues(curr = vector('character'), add = vector('character'), drop = vector('character'))

    # * assoc_ids ----
    assoc_ids <- reactiveValues(curr = vector('character'), add = vector('character'), drop = vector('character'))

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

    map_proxy <- leaflet::leafletProxy(mapId = "map", session = session)

    # _________ ----
    # observers ----

    # * store_loc() ----
    observeEvent(store_loc(), {

      selected_ids <- store_loc() %>%
        dplyr::mutate(layer_id = paste0('store-', .data$store_key)) %>%
        dplyr::pull(.data$layer_id) %>% unique()

      # update 'store_ids'
      store_ids$add <- setdiff(selected_ids, store_ids$curr)
      store_ids$drop <- setdiff(store_ids$curr, selected_ids)
      store_ids$curr <- selected_ids

    })

    # ** store_ids$add ----
    observeEvent(store_ids$add, {

      if (isTRUE(length(store_ids$add) > 0)) {

        add_data <- store_loc() %>%
          dplyr::mutate(layer_id = paste0('store-', .data$store_key)) %>%
          dplyr::filter(.data$layer_id %in% store_ids$add)

        map_proxy %>%
          leaflet::addAwesomeMarkers(
            data = add_data,
            group = "store",
            layerId = ~layer_id,
            lng = ~longitude,
            lat = ~latitude,
            icon = leaflet::makeAwesomeIcon(icon = 'star', markerColor = 'purple', iconColor = 'black')
          )

        store_ids$add <- vector('character')

      }

    })

    # ** store_ids$drop ----
    observeEvent(store_ids$drop, {

      if (isTRUE(length(store_ids$drop) > 0)) {

        map_proxy %>% leaflet::removeMarker(layerId = store_ids$drop)
        store_ids$drop <- vector('character')

      }

    })

    # * comp_loc() ----
    observeEvent(comp_loc(), {

      selected_ids <- comp_loc() %>%
        dplyr::mutate(layer_id = paste0('comp-', .data$comp_key)) %>%
        dplyr::pull(.data$layer_id) %>% unique()

      # update 'comp_ids'
      comp_ids$add <- setdiff(selected_ids, comp_ids$curr)
      comp_ids$drop <- setdiff(comp_ids$curr, selected_ids)
      comp_ids$curr <- selected_ids

    })

    # ** comp_ids$add ----
    observeEvent(comp_ids$add, {

      if (isTRUE(length(comp_ids$add) > 0)) {

        add_data <- comp_loc() %>%
          dplyr::left_join(storeMap::banner_colors, by = 'banner') %>%
          dplyr::mutate(layer_id = paste0('comp-', .data$comp_key)) %>%
          dplyr::filter(.data$layer_id %in% comp_ids$add)

        map_proxy %>%
          leaflet::addCircleMarkers(
            data = add_data,
            group = "comp",
            layerId = ~layer_id,
            lng = ~longitude,
            lat = ~latitude,
            color = ~color,
            label = ~banner,
            stroke = TRUE,
            fillOpacity = 1,
            radius = 4
          )

        comp_ids$add <- vector('character')

      }

    })

    # ** comp_ids$drop ----
    observeEvent(comp_ids$drop, {

      if (isTRUE(length(comp_ids$drop) > 0)) {

        map_proxy %>% leaflet::removeMarker(layerId = comp_ids$drop)
        comp_ids$drop <- vector('character')

      }

    })

    # _____________ ----
    # module output ----
    list(store_loc = store_loc, comp_loc = comp_loc)

  })

}
