

box::use(
  shiny[NS, moduleServer, tagList, h2, h3, h5, fluidRow, column, tags, div,
        includeCSS, uiOutput, renderUI, reactive, icon, req, br
  ],
  dplyr[filter, pull, summarise, mutate, group_by, select, everything,
        ungroup
  ],
  magrittr[`%>%`],
  bs4Dash[infoBox, box],
  highcharter[renderHighchart, highchartOutput, hc_title, hcaes, hc_xAxis,
              hc_yAxis, hchart
  ],
  reactable[reactable, colDef, colFormat, reactableOutput, renderReactable],
  tidyr[pivot_wider],
  grDevices[rgb, colorRamp]
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
    ),
    br(),
    fluidRow(
      column(12,
             div(class = 'divclass',
      highchartOutput(ns('ot_proft_market'))
             )
    )
    ),
    br(),
    fluidRow(
      box(
        status = 'indigo',
        title = h5('Major Market - Sales by Month', style = 'color: white'),
        solidHeader = TRUE,
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        reactableOutput(ns('ot_heat_major'))
      )
      
    ),
    br(),
    fluidRow(
      box(
        status = 'indigo',
        title = h5('Small Market - Sales by Month', style = 'color: white'),
        solidHeader = TRUE,
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        reactableOutput(ns('ot_heat_small'))
      )
    )
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
      
      market_profit_2_rctv <- reactive({
        
        sales %>% 
          group_by(market_size, date) %>% 
          summarise(total_profit = sum(profit)) %>% 
          ungroup() %>% 
          mutate(date = as.Date(date))
        
      })
      
      
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
      
      
      # 
      
      
      output$ot_heat_small <- renderReactable({
        
        req(sales)
        
        monthy_sales <- sales %>% 
          filter(market_size == 'Small Market') %>% 
          mutate(date = as.Date(date)) %>% 
          group_by(date) %>% 
          summarise(sales = sum(sales)) %>% 
          ungroup() %>% 
          mutate(mnth = month.abb[lubridate::month(date)]) %>% 
          mutate(year = lubridate::year(date)) %>% 
          select(mnth, year, sales) %>% 
          pivot_wider(names_from = mnth, values_from = sales)
        
        monthy_sales_slim <- monthy_sales %>% 
          select(-year)
        
        dimnames <- list(c(2010, 2011), month.abb)
        
        monthy_sales_mtrx <- matrix(unlist(monthy_sales_slim)
                                    , nrow = 2
                                    , dimnames = dimnames
        )
        
        
        BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x),
                                  maxColorValue = 255)
        
        
        reactable(
          monthy_sales_mtrx,
          defaultColDef = colDef(
            align = "center",
            style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - min(monthy_sales_mtrx)) / (max(monthy_sales_mtrx) - min(monthy_sales_mtrx))
              color <- BuYlRd(normalized)
              list(background = color)
            },
            format = colFormat(digits = 0),
            minWidth = 50
          ),
          columns = list(
            .rownames = colDef(name = "Year", sortable = TRUE, align = "left")
          ),
          bordered = TRUE
        )
        
      })
      
      
      output$ot_heat_major <- renderReactable({
        
        req(sales)
        
        monthy_sales <- sales %>% 
          filter(market_size == 'Major Market') %>% 
          mutate(date = as.Date(date)) %>% 
          group_by(date) %>% 
          summarise(sales = sum(sales)) %>% 
          ungroup() %>% 
          mutate(mnth = month.abb[lubridate::month(date)]) %>% 
          mutate(year = lubridate::year(date)) %>% 
          select(mnth, year, sales) %>% 
          pivot_wider(names_from = mnth, values_from = sales)
        
        monthy_sales_slim <- monthy_sales %>% 
          select(-year)
        
        dimnames <- list(c(2010, 2011), month.abb)
        
        monthy_sales_mtrx <- matrix(unlist(monthy_sales_slim)
                                    , nrow = 2
                                    , dimnames = dimnames
        )
        
        
        BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x),
                                  maxColorValue = 255)
        
        
        reactable(
          monthy_sales_mtrx,
          defaultColDef = colDef(
            align = "center",
            style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - min(monthy_sales_mtrx)) / (max(monthy_sales_mtrx) - min(monthy_sales_mtrx))
              color <- BuYlRd(normalized)
              list(background = color)
            },
            format = colFormat(digits = 0),
            minWidth = 50
          ),
          columns = list(
            .rownames = colDef(name = "Year", sortable = TRUE, align = "left")
          ),
          bordered = TRUE
        )
        
      })
      
      
      
      
      output$ot_proft_market <- renderHighchart({
        
        hchart(
          market_profit_2_rctv(), 
          "column",
          hcaes(x = date, y = total_profit, group = market_size),
          color = c("#7CB5EC", "#F7A35C"),
          name = c("Major Market", "Small Market")
        ) %>% 
          hc_xAxis(title = NULL)  %>% 
          hc_yAxis(title = list(text = "Profit ($)")) %>% 
          hc_title(
            text = "<b>Monthly Profit by Market Size</b>",
            margin = 20,
            align = "left",
            style = list(color = "#737574", useHTML = TRUE)
          ) 
      })
      
      
      
      
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