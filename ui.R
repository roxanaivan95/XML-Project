
# Dashboard
sidebar <- dashboardSidebar(collapsed = TRUE,
                            fluidPage(
                              sidebarMenu(
                                menuItem("XML", tabName = "relatieFamilie", icon = icon("bar-chart"))
                              )
                            )
)

body <- dashboardBody(
  fluidPage(
    fluidRow(
      tabItems(
        xmlFunctionUI("relatieFamilie")
      )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = paste0('XML'
  )),
  sidebar,
  body
)

