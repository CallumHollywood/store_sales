

box::use(
  shiny[NS, moduleServer, tagList, column, fluidRow, h2, br,
        tags
        ],
  reactable[reactableOutput, renderReactable, colDef, colFormat, reactable],
  dplyr[group_by, summarise, ungroup, arrange, desc, rename, select,
        mutate, left_join, distinct
        ],
  magrittr[`%>%`],
  bs4Dash[box],
  highcharter[renderHighchart, highchartOutput, hcmap, get_data_from_map,
              download_map_data, hchart, hcaes
              ]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             align = 'center',
             h2('Profit')
      )
    ),
    br(),
    fluidRow(
             box(
               title = "Profit by Product Type",
               closable = TRUE,
               width = 6,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               reactableOutput(ns('ot_budget_profit'))
             ),
             box(
               title = "Profit by State",
               closable = TRUE,
               width = 6,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE, 
               highchartOutput(ns('ot_map_profit'))
             )
             
    ),
    br(),
    fluidRow(
      column(12,
             align = 'center',
             h2('Product Metrics')
      )
    ),
    br(),
    fluidRow(
      box(
        title = "Total Expenses by Product and Market Size",
        closable = FALSE,
        width = 4,
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        highchartOutput(ns('ot_product_expenses'))
      ),
      box(
        title = "Total Expenses by Product",
        closable = FALSE,
        width = 8,
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        reactableOutput(ns('ot_product_metrics'))
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
      
      output$ot_product_metrics <- renderReactable({
        
        
        product_metrics <- sales %>% 
          mutate(product = paste(product_type, product)) %>% 
          select(product, product_type, total_expenses, profit, sales) %>% 
          group_by(product) %>% 
          mutate(
            total_expenses = sum(total_expenses),
            profit = sum(profit),
            sales = sum(sales)
          ) %>% 
          ungroup() %>% 
          distinct() %>% 
          arrange(desc(total_expenses))
        
        bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
          
          bar <- tags$div(style = list(background = fill, width = width, height = height))
          
          if(nchar(as.character(label)) == 4){
            
            marginleft <- "1.5rem"
            
          } else if(nchar(as.character(label)) == 5){
            
            marginleft <- "1rem"
            
          }  
          
          chart <- tags$div(style = list(flexGrow = 1, marginLeft = marginleft, background = background), bar)
          tags$div(style = list(display = "flex", alignItems = "center"), label, chart)
        }
        
        
        
        bar_chart2 <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
          
          bar <- tags$div(style = list(background = fill, width = width, height = height))
          
          if(nchar(as.character(label)) == 5){
            
            marginleft <- "1.5rem"
            
          } else if(nchar(as.character(label)) == 6){
            
            marginleft <- "1rem"
            
          }  
          
          chart <- tags$div(style = list(flexGrow = 1, marginLeft = marginleft, background = background), bar)
          tags$div(style = list(display = "flex", alignItems = "center"), label, chart)
        }
        
        product_metrics %>% 
          select(-product_type) %>% 
          reactable(
            showSortable = TRUE,
            columns = list(
              total_expenses = colDef(name = "total_expenses", align = "left", cell = function(value) {
                width <- paste0(value / max(product_metrics$total_expenses) * 100, "%")
                bar_chart(value, width = width, background = "#e1e1e1", fill = "#e6b122")
              }
              ),
              profit = colDef(name = "profit", align = "left", cell = function(value) {
                width <- paste0(value / max(product_metrics$total_expenses) * 100, "%")
                bar_chart(value, width = width, background = "#e1e1e1", fill = "#ab47bf")
              }
              ),
              sales = colDef(name = "sales", align = "left", cell = function(value) {
                width <- paste0(value / max(product_metrics$total_expenses) * 100, "%")
                bar_chart2(value, width = width, background = "#e1e1e1")
              }
              )
            )
          )
        
      })
      
      
      output$ot_product_expenses <- renderHighchart({
        
        product_expenses2 <- sales %>% 
          mutate(product = paste(product_type, product)) %>% 
          select(market_size, product, product_type, total_expenses) %>% 
          group_by(market_size, product) %>% 
          mutate(total_expenses = sum(total_expenses)) %>% 
          ungroup() %>% 
          distinct() %>% 
          arrange(desc(total_expenses))
        
        hchart(
          product_expenses2, 
          "bar",
          hcaes(x = product,
                y = total_expenses
                , group = market_size
          ),
          color = c("#7CB5EC", "#F7A35C"),
          name = c("Major Market", "Small Market")
        )
        
      })
      
      output$ot_map_profit <- renderHighchart({
        
        state_prd_profit <- sales %>% 
          select(state, product_type, product, profit) %>% 
          group_by(state) %>% 
          summarise(profit = sum(profit)) %>% 
          ungroup()
        
        plotting_data <- get_data_from_map(download_map_data("countries/us/us-all")) %>% 
          select(name)
        
        plotting_data <- plotting_data %>% 
          left_join(state_prd_profit %>% rename(name = state)) %>% 
          mutate(profit = ifelse(is.na(profit), 0, profit))
        
        hcmap(
          "countries/us/us-all",
          showInLegend = FALSE,
          data = plotting_data,
          value = 'profit',
          tooltip = list(
            valueDecimals = 2,
            valuePrefix = "$",
            valueSuffix = "USD"
          ),
          name = 'profit',
          dataLabels = list(enabled = TRUE, format = "{point.name}")
        )
        
        
      })
      
      
      output$ot_budget_profit <- renderReactable({
        
        grouped_sales <- sales %>% 
          group_by(product_type, product, type) %>% 
          summarise(avg_budget_profit = round(mean(budget_margin), 0)) %>% 
          ungroup() %>% 
          arrange(product_type, desc(avg_budget_profit))
        
        
        grouped_sales <- grouped_sales %>% 
          rename(
            `Product Type` = product_type,
            Product = product ,
            Type = type,
            `Average Budget Profit` = avg_budget_profit
          )
        
        reactable(
          grouped_sales,
          groupBy = "Product Type",
          searchable = TRUE,
          columns = list(
            `Average Budget Profit` = colDef(
              aggregate = "sum",
              format = colFormat(currency = "USD")
            )
          )
        )
        
      })
      
    }
  )
}

