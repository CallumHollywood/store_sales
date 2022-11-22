

box::use(
  shiny[NS, moduleServer, tagList]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    'market test'
  )
}

#' @export
server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}