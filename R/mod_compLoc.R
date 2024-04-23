
#' UI for Competitor Location Module
#'
#' @importFrom rlang .data
#'
#' @param id module id
#'
#' @return shiny.tag.list
#' @export
#'
compLocUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(
      ns('banner'), label = "Banner",
      selected = NULL, multiple = TRUE,
      choices = storeMap::comp_location %>% dplyr::distinct(.data$banner) %>% dplyr::pull() %>% unique() %>% sort(),
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE, liveSearch = TRUE,
        dropupAuto = FALSE, noneSelectedText = '(Banner)'
      )
    ),

    shinyWidgets::pickerInput(
      ns('location'), label = "Competitor",
      selected = NULL, multiple = TRUE,
      choices = storeMap::comp_location %>% dplyr::distinct(.data$street_address) %>% dplyr::pull() %>% unique() %>% sort(),
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE, liveSearch = TRUE,
        dropupAuto = FALSE, noneSelectedText = '(Location)'
      )
    )
  )
}

#' Server Function for Competitor Location Module
#'
#' @importFrom rlang .data
#'
#' @param id module id
#'
#' @return shiny reactive
#' @export
#'
compLocServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    # ________ ----
    # reactives ----

    # * location_val ----
    location_val <- shiny::reactive({
      storeMap::comp_location %>% dplyr::distinct(.data$banner, .data$street_address)
    })

    # _________ ----
    # observers ----

    # * input$banner ----
    shiny::observeEvent(input$banner, {

      shiny::req(location_val())

      # update locations
      loc_choices <- location_val() %>% dplyr::filter(.data$banner %in% input$banner) %>%
        dplyr::pull(.data$street_address) %>% unique() %>% sort()

      shinyWidgets::updatePickerInput(session = session, "location", selected = loc_choices, choices = loc_choices)

    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # _____________ ----
    # module output ----
    shiny::reactive(storeMap::comp_location %>% dplyr::filter(.data$banner %in% input$banner, .data$street_address %in% input$location))

  })

}
