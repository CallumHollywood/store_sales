

box::use(
  shiny[tagList, moduleServer, NS, renderText, tags, textOutput],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, 
          dashboardControlbar, dashboardFooter, dashboardBody,
          bs4SidebarMenu, bs4SidebarMenuItem, tabItems, tabItem
          ]
)

box::use(
  app/view/market_analysis,
  app/view/product_analysis
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    dashboardPage(
      title = "DEMO - Store Sales Dashboard",
      header = dashboardHeader(
        title = 'Store Sales Dashboard'
      ),
      sidebar = dashboardSidebar(
        bs4SidebarMenu(
          id = NULL,
          .list = NULL,
          flat = FALSE,
          compact = FALSE,
          childIndent = TRUE,
          legacy = FALSE,
          bs4SidebarMenuItem(
            'Market Analysis',
            icon = NULL,
            badgeLabel = NULL,
            badgeColor = "success",
            tabName = 'market_analysis',
            href = NULL,
            newTab = TRUE,
            selected = NULL,
            # expandedName = as.character(gsub("[[:space:]]", "", text)),
            startExpanded = FALSE,
            condition = NULL
          ),
          bs4SidebarMenuItem(
            'Product Analysis',
            icon = NULL,
            badgeLabel = NULL,
            badgeColor = "success",
            tabName = 'product_analysis',
            href = NULL,
            newTab = TRUE,
            selected = NULL,
            # expandedName = as.character(gsub("[[:space:]]", "", text)),
            startExpanded = FALSE,
            condition = NULL
          )
          
        )
      ),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "market_analysis",
            market_analysis$ui(ns('mrktanlyss'))
          ),
          tabItem(
            tabName = "product_analysis",
            product_analysis$ui(ns('prdanlyss'))
          )
          
        )
      )
    )
  )
}

#' @export
server <- function(
    id,
    sales
    ) {
  moduleServer(id, function(input, output, session) {
    
    
    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#
    
    market_analysis$server('mrktanlyss',
                           sales
                           )
    
    product_analysis$server('prdanlyss')
    
    
    
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

