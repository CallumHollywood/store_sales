

box::use(
  shiny[NS, moduleServer, tagList]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    'product test'
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