
#' Return Shiny Application
#'
#' @return shiny app object
#' @export
#'
app_pkg_run <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}
