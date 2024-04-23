
server <- function(input, output, session) {

  # ________ ----
  # modules ----

  # * loc_leaf ----
  loc_leaf <- locLeafletServer('loc_leaf')

  # _______ ----
  # outputs ----

  # * store_loc_tbl ----
  output$store_loc_tbl <- DT::renderDataTable({
    DT::datatable(
      data = shiny::isolate(loc_leaf$store_loc()),
      rownames = TRUE,
      options = list(
        dom = 'lftip',
        pageLength = c(5),
        lengthMenu = c(5, 10, 15, 25, 50),
        scrollX = TRUE, scrollY = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0))
        )
      )
    )
  })

  proxy__store_loc_tbl <- DT::dataTableProxy('store_loc_tbl')

  # * comp_loc_tbl ----
  output$comp_loc_tbl <- DT::renderDataTable({
    DT::datatable(
      data = shiny::isolate(loc_leaf$comp_loc()),
      rownames = TRUE,
      options = list(
        dom = 'lftip',
        pageLength = c(5),
        lengthMenu = c(5, 10, 15, 25, 50),
        scrollX = TRUE, scrollY = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0))
        )
      )
    )
  })

  proxy__comp_loc_tbl <- DT::dataTableProxy('comp_loc_tbl')

  # _________ ----
  # observers ----

  # * loc_leaf$store_loc() ----
  shiny::observeEvent(loc_leaf$store_loc(), {
    shiny::req(loc_leaf$store_loc())
    DT::replaceData(proxy__store_loc_tbl, loc_leaf$store_loc(), resetPaging = FALSE, rownames = TRUE)
  }, ignoreInit = TRUE)

  # * loc_leaf$comp_loc() ----
  shiny::observeEvent(loc_leaf$comp_loc(), {
    shiny::req(loc_leaf$comp_loc())
    DT::replaceData(proxy__comp_loc_tbl, loc_leaf$comp_loc(), resetPaging = FALSE, rownames = TRUE)
  }, ignoreInit = TRUE)

}
