
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      wellPanel(storeLocUI('store_loc')),
      wellPanel(compLocUI('comp_loc')),
      wellPanel(assocLocUI('assoc_loc'))
    ),
    mainPanel(
      fluidRow(
        column(width = 12, locLeafletUI('loc_leaflet')),
        column(width = 6, dataTableOutput('store_tbl')),
        column(width = 6, dataTableOutput('comp_tbl'))
      )
    )
  )

)
