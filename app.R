library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)
library(tidyverse)
library(DT)

list_call <- list(aus_production,gafa_stock,global_economy,hh_budget,vic_elec)

re_type <- function(i){
  x <- list_call[[i]]
  x[,1] <- sapply(x[,1],as.character)
  return(x)
}

#gafa_stock: Random
#aus_production: Seasonal and Trend
#vic_elec: Seasonal without Trend
#hh_budget
#global_economy


dataset <- list( 
     "Quarterly production of selected commodities in Australia" = 1,
     "GAFA stock prices" = 2,
     "Global economic indicators" = 3,
     "Household budget characteristics" = 4,
     "Half-hourly electricity demand for Victoria, Australia" = 5
)

desc <- list('Quarterly estimates of selected indicators of manufacturing production in Australia.',
             'Historical stock prices from 2014-2018 for Google, Amazon, Facebook and Apple. All prices are
              in $USD.',
             'Economic indicators featured by the World Bank from 1960 to 2017.',
             'Annual indicators of household budgets for Australia, Japan, Canada and USA from 1995-2016.',
             'Half-hourly electricity demand for Victoria, Australia')

#setwd('D://ShinyProject')
# = shinytheme("cerulean")
ui <- dashboardPage(
  
  #Add Heading
  dashboardHeader(title = h5("PSTAT 174: Time Series")),
  
  #Add Slider
  #https://getbootstrap.com/docs/3.4/components/#glyphicons
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
    menuItem("Stucture in Time Series", tabName = "st", icon = icon("equalizer", lib = "glyphicon")),
    menuItem("Time Series Model", tabName = "tsm", icon = icon("align-left", lib = "glyphicon"),
             menuItem('ARIMA(p,d,q)',
                      tabName = 'arima',
                      icon = icon('line-chart')),
             menuItem('SARIMA(P,D,Q)',
                      tabName = 'sarima',
                      icon = icon('line-chart'))
             ),
    menuItem("Gaussian Process", tabName = "gp", icon = icon("asterisk", lib = "glyphicon")),
    menuItem("Real Data Prediction", tabName = "re", icon = icon("list-alt", lib = "glyphicon"),
             menuItem('Commodity Price',
                      tabName = 'cp',
                      icon = icon('line-chart')),
             menuItem('Electric Demand',
                      tabName = 'ed',
                      icon = icon('line-chart')))
  )),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
      #https://github.com/nik01010/dashboardthemes
    ),
    
    tabItems(
      tabItem(tabName = "st",
              fluidRow(
                box(title = "Select Data Set", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12, selectInput("select", label = h5("*Some Data Sets from tsibbledata"), 
                                choices = dataset, 
                                selected = 1),
                    textOutput("text"))),
              fluidRow(  
                box(width = 12, title = "Data", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                  DT::dataTableOutput('table')
                )
              ),
              fluidRow(  
                box(width = 12, title = "Select Feature and Plot", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                        conditionalPanel(
                            condition = "input.select == 1", 
                            selectInput("pd", label = h5("Select Product"),
                                        choices = as.list(1:6) %>%`names<-`(colnames(list_call[[1]])[-1]), 
                                        selected = 1),
                            actionButton("ppd", label = "Plot"),
                            plotOutput('p1')
                            
                        )
                )
              )
              
            ),
              tabItem(tabName = "tsm",
                      h2("Widgets..")),
              
              tabItem(tabName = "gp",
                      h2("Widgets...")),
              
              tabItem(tabName = "re",
                       h2("Widgets...."))
              
      )
      )

  )


server <- function(input, output) {
  
  output$text <- renderText({desc[[as.integer(input$select)]]}) 
  
  output$table <- DT::renderDataTable(re_type(as.integer(input$select)))
  

  
}

shinyApp(ui, server)
