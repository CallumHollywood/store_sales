

box::use(
  shiny[NS, moduleServer, tagList, h2, h3, fluidRow, column, tags, div,
        includeCSS, uiOutput, renderUI, reactive, icon, req, br
  ],
  dplyr[filter, pull, summarise, mutate, group_by, select, everything],
  magrittr[`%>%`],
  bs4Dash[infoBox]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "app/styles/styles.css")
    # ),
    includeCSS('app/styles/styles.css'),
    fluidRow(
      column(12,
             align = 'center',
             h2('SALES')
      )
    ),
    fluidRow(
      column(6,
             div(class = 'divclass',
                 column(12,
                        align = 'center',
                   h3('2010')
                        ),
                 fluidRow(
                   column(2),
                   column(4, 
                          uiOutput(ns('ot_major_market_2010'))
                   ),
                   column(4, 
                          uiOutput(ns('ot_small_market_2010'))
                   )
                 )
             )
      ),
      column(6,
             div(class = 'divclass',
                 column(12,
                        align = 'center',
                        h3('2011')
                 ),
                 fluidRow(
                   column(2),
                   column(4, 
                          uiOutput(ns('ot_major_market_2011'))
                   ),
                   column(4, 
                          uiOutput(ns('ot_small_market_2011'))
                   )
                 )
             )
      )
    )
      
    # )
  )
}

#' @export
server <- function(
    id,
    sales
) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### <<<<    CALLMODULES     >>>>  ####
      #-------------------------------------#
      
      #### <<<<    STATIC VALUES   >>>>  ####
      #-------------------------------------#
      
      market_sales_rctv <- reactive({
        
        marketsales <- sales %>% 
          mutate(year = lubridate::year(date)) %>%
          group_by(year, market_size) %>%
          summarise(sales = sum(sales))
        
        # print(marketsales)
        # message('marketsales')
        
        return(marketsales)
        
      })
      
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
      
      
      output$ot_small_market_2011 <- renderUI({
        
        req(market_sales_rctv())
        
        value <- market_sales_rctv() %>% filter(year == 2011, market_size == 'Small Market') %>% pull(sales)
        value <- format(value, big.mark=",",scientific=FALSE)
        
        div(class = 'infomargin',
            infoBox(
              tabName = "cardsAPI",
              title = "Small Market",
              value = value,
              color = "indigo",
              icon = icon("laptop-code"),
              width = 12
            )
        )
        
      })
      
      
      
      output$ot_major_market_2011 <- renderUI({
        
        req(market_sales_rctv())
        
        value <- market_sales_rctv() %>% filter(year == 2011, market_size == 'Major Market') %>% pull(sales)
        value <- format(value, big.mark=",",scientific=FALSE)
        
        div(class = 'infomargin',
            infoBox(
              tabName = "cardsAPI",
              title = "Major Market",
              value = value,
              color = "indigo",
              icon = icon("laptop-code"),
              width = 12
            )
        )
      })
      
      output$ot_major_market_2010 <- renderUI({
        
        req(market_sales_rctv())
        
        value <- market_sales_rctv() %>% filter(year == 2010, market_size == 'Major Market') %>% pull(sales)
        value <- format(value, big.mark=",",scientific=FALSE)
        
        div(class = 'infomargin',
            infoBox(
              tabName = "cardsAPI",
              title = "Major Market",
              value = value,
              color = "indigo",
              icon = icon("laptop-code"),
              width = 12
            )
        )
      })
      
      
      output$ot_small_market_2010 <- renderUI({
        
        req(market_sales_rctv())
        
        value <- market_sales_rctv() %>% filter(year == 2010, market_size == 'Small Market') %>% pull(sales)
        value <- format(value, big.mark=",",scientific=FALSE)
        
        div(class = 'infomargin',
            infoBox(
              tabName = "cardsAPI",
              title = "Small Market",
              value = value,
              color = "indigo",
              icon = icon("laptop-code"),
              width = 12
            )
        )
        
      })
      
      
      
      
    }
  )
}