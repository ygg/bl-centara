
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggvis)

# kpi <- readRDS('../../AdHoc/Budget.rds')
kpi %>%
  glimpse()

kpi_list <- c("UWC","NetGW","Bonus","NetEGW","APD","NDC","NVC","UDC","UDC","Wager","WD","Dep","TRTP")
month_list <- 1:12


ui <- dashboardPage(
  dashboardHeader(title = "Track Daily KPI"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        
              fluidRow(
                box(
                  selectInput('aaa','Select KPI' , choices = kpi_list, selected=c("UWC"))
                )
                ,box(  
                  selectInput('mm' , 'Month' , choices = month_list)
                 ) 
              ),
              fluidRow( 
               #  column(1, htmlOutput("trend_ui"))
               # ,column(1, offset=5,htmlOutput("csum_ui"))
               # 
               tabBox(
                 title = "KPI Trend",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1", height = "250px",
                 tabPanel("Trend", htmlOutput("trend_ui")),
                 tabPanel("Cumulative", offset=5,htmlOutput("csum_ui"))
               )
             
        )
      ) 
    )
  )
) 



server <- function(input, output) {
  
  output$plot1 <- renderPlot(
    {
      data <- histdata[seq_len(input$slider)]
      hist(data)
    }
  )

  # Make selection
  
  data <- reactive({
    kpi$kpi <- kpi[,input$aaa]

    kpi %>%
    filter(month_of_year==input$mm) %>%
    mutate(year = as.factor(year_of_calendar)) %>%
    group_by(year,day_of_month) %>%
    arrange(year,day_of_month) %>%
    summarise(kpi = sum(kpi))
#    mutate(kpi =cumsum(kpi)) 
    })

  data %>%
    ggvis(x=~day_of_month,y=~kpi) %>%
    layer_smooths(stroke=~year) %>%
    bind_shiny("trend")

  output$trend_ui <-  renderUI({
    ggvisOutput("trend")
  })

## ccum
  data_ccum <- reactive({
    kpi$kpi <- kpi[,input$aaa]

    kpi %>%
      filter(month_of_year==input$mm) %>%
      mutate(year = as.factor(year_of_calendar)) %>%
      group_by(year,day_of_month) %>%
      arrange(year,day_of_month) %>%
      summarise(kpi = sum(kpi)) %>%
      mutate(kpi =cumsum(kpi))
  })

  data_ccum %>%
    ggvis(x=~day_of_month,y=~kpi) %>%
    layer_smooths(stroke=~year) %>%
    bind_shiny("csum")

  output$csum_ui <-  renderUI({
    ggvisOutput("csum")
  })

}


shinyApp(ui, server)

##
## forecast
## 

library(forecast)


