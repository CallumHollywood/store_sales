box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput,
        tagList
        ],
  readr[read_csv],
  janitor[clean_names],
  magrittr[`%>%`],
  dplyr[mutate],
  lubridate[mdy_hms]
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
    
    layout$server('layout',
                  sales
                  )
    
    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#
    
    sales <- read_csv('data/sales.csv') %>% 
      clean_names() %>% 
      mutate(date = mdy_hms(date))%>% 
      mutate(date = as.Date(date))
    
    
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
