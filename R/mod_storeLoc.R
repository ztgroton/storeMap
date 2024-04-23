
#' UI for Store Location Module
#'
#' @importFrom rlang .data
#'
#' @param id module id
#'
#' @return shiny.tag.list
#' @export
#'
storeLocUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::column(
      width = 12, style = "padding-right: 5px !important; padding-left: 5px !important;",

      shinyWidgets::pickerInput(
        ns('state'), label = "State", selected = NULL, multiple = TRUE,
        choices = storeMap::store_location %>% dplyr::pull(.data$state) %>% unique() %>% sort(),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          dropupAuto = FALSE, noneSelectedText = '(State Abbr)'
        )
      ),

      shinyWidgets::pickerInput(
        ns('price_zone'), label = "Price Zone", selected = NULL, multiple = TRUE,
        choices = storeMap::store_location %>% dplyr::distinct(.data$price_zone) %>% dplyr::pull() %>% unique() %>% sort(),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          dropupAuto = FALSE, noneSelectedText = '(Price Zone)'
        )
      ),

      shinyWidgets::pickerInput(
        inputId = ns('store'), label = "Store Name", selected = NULL, multiple = TRUE,
        choices = storeMap::store_location %>% dplyr::distinct(.data$name) %>% dplyr::pull() %>% unique() %>% sort(),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE,
          dropupAuto = FALSE, noneSelectedText = '(Store Name)'
        )
      )

    )
  )
}

#' Server Function for Store Location Module
#'
#' @importFrom rlang .data
#'
#' @param id module id
#'
#' @return shiny reactive
#' @export
#'
storeLocServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    # ________ ----
    # reactives ----

    # * pz_val ----
    pz_val <- shiny::reactive({
      storeMap::store_location %>% dplyr::distinct(.data$state, .data$price_zone)
    })

    # * store_val ----
    store_val <- shiny::reactive({
      storeMap::store_location %>% dplyr::distinct(.data$price_zone, .data$name)
    })

    # _________ ----
    # observers ----

    # * input$state ----
    shiny::observeEvent(input$state, {

      # update price zone
      pz_choices <- pz_val() %>% dplyr::filter(.data$state %in% input$state) %>%
        dplyr::pull(.data$price_zone) %>% unique() %>% sort()

      shinyWidgets::updatePickerInput(session = session, "price_zone", selected = pz_choices, choices = pz_choices)

    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # * input$price_zone ----
    shiny::observeEvent(input$price_zone, {

      # update store
      store_choices <- store_val() %>% dplyr::filter(.data$price_zone %in% input$price_zone) %>%
        dplyr::pull(.data$name) %>% unique() %>% sort()

      shinyWidgets::updatePickerInput(session = session, "store", selected = store_choices, choices = store_choices)

    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # _____________ ----
    # module output ----
    shiny::reactive(storeMap::store_location %>% dplyr::filter(.data$name %in% input$store))

  })

}
