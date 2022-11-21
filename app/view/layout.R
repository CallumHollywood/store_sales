

box::use(
  shiny[tagList, moduleServer, NS, renderText, tags, textOutput],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, 
          dashboardControlbar, dashboardFooter, dashboardBody
          ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    dashboardPage(
      title = "Basic Dashboard",
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody()
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })
}

