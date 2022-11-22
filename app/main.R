box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput,
        tagList
        ],
)

box::use(
  app/view/layout
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    layout$ui(ns('layout'))
    
  )
  
  }

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#
    
    layout$server('layout')
    
    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#
    
    
    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#
    
    
  })
}
