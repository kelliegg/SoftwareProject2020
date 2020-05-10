library(shiny)
library(shinydashboard)
library(DT)


info = read.csv("rShinyData.csv", header = TRUE)
info$X = NULL

weather = read.csv("weather.csv", header = T)
weather$X = NULL

flightsNY = read.csv("flightsNYDone.csv", header = T)
flightsNY$X = NULL

info$Pressure <- as.numeric(as.character(info$Pressure))
info$Wind.Speed <- as.numeric(as.character(info$Wind.Speed))
info$Wind.Direction <- as.numeric(as.character(info$Wind.Direction))
info$humidity <- as.numeric(as.character(info$humidity))
info$DAY_OF_WEEK <- as.numeric(as.character(info$DAY_OF_WEEK))

ui <- dashboardPage( skin = "yellow",
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Correlations", tabName = "corr", icon = icon("cloud")),
    menuItem("Raw Data", tabName = "header", icon = icon("sun")),
    menuItem("Data", tabName = "info", icon = icon("plane")),
    menuItem("Exploration of Data", tabName = "explore", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("corr",
              box(plotOutput("correlation_plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("Wind.Speed", "Pressure",
                              "humidity")), width = 4
              )
     ),
      tabItem("header",
              fluidPage(
              h1("Raw Datasets"),
              h2("Flight Data Raw"),
              dataTableOutput("flightsHeader"),
              h2("Weather Data Raw"),
              dataTableOutput("weatherHeader")
              )
      ),
     tabItem("info",
             fluidPage(
               h1("Merged Datasets"),
               dataTableOutput("infotable")
             )
     ),
      tabItem("explore",
              fluidPage(
                h1("Initial Exploration of the Data"),
                fluidRow(
                  box(
                    h2("Delayed Flights by Airline (in minutes)"),
                    p("Initial Analysis"),
                    imageOutput('airline', height = "auto")),
                  box(
                    h2('FLights On Time, Delayed and Cancelled by Airline'),
                    p("Further Analysis"),
                    imageOutput('delays', height = "auto"))
                  ),
              )
       )
    
     )
  )
)

server <- function(input, output){
    output$correlation_plot <- renderPlot({
      plot(info$tempC, info[[input$features]],
           xlab = "Temperature", ylab = "Selected Feature", col = c('blue'))
    })
    
    output$infotable <- renderDataTable(info)
    output$header = renderDataTable(info, options = list(scrollX = TRUE, pageLength =5))
    
    output$weatherHeader = renderDataTable(weather, options = list(scrollx = TRUE, pageLength = 5))
    output$flightsHeader = renderDataTable(flightsNY, options = list(scrollx = TRUE, pageLength = 5))
    
    output$airline = renderImage({
      return(list(src="airlines.png", contentType="image/png", alt="Airlines"))
    }, deleteFile = FALSE)
    output$delays = renderImage({
      return(list(src= "delaysVisuals.png", contentType="image/png", alt="Airlines"))
    }, deleteFile = FALSE)
    
   
    
    
    
}

shinyApp(ui, server)
