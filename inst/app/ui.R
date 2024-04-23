
ui <- fluidPage(

  shinydashboardPlus::dashboardPage(

    preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
    header = shinydashboardPlus::dashboardHeader(title = "Store Map App"),

    sidebar = shinydashboardPlus::dashboardSidebar(disable = TRUE),
    controlbar = shinydashboardPlus::dashboardControlbar(disable = TRUE),

    body = shinydashboard::dashboardBody(

      shinybusy::use_busy_spinner(spin = "fading-circle"),

      shinydashboardPlus::box(
        width = 12, title = "Total Wine & More - Stores and Competitors",
        collapsible = TRUE, collapsed = FALSE,

        shiny::column(width = 12, locLeafletUI('loc_leaf'))

      )

    )

  )

)
