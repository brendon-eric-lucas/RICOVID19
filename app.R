library(googlesheets4)
library(ggplot2)
library(shiny)

gs4_deauth()
ss <- "https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272"
dat <- read_sheet(ss, sheet = "Trends")

ui <- fluidPage(
  
  # App title ----
  titlePanel(h3("Rhode Island COVID-19 Tracker")),
  
  navlistPanel(
    "Testing Data",
    
    tabPanel("New Positive Labs", 
              mainPanel(plotOutput("New Positive Labs")) 
            ),
    
    tabPanel("Total Positive Labs", 
              mainPanel(plotOutput("Total Positive Labs"))
             ),
    
    tabPanel("New Negative Labs", 
              mainPanel(plotOutput("New Negative Labs"))
             ),
    
    tabPanel("Total Negative Labs", 
              mainPanel(plotOutput("Total Negative Labs"))
             ),
    
    tabPanel("Total Tested",
              mainPanel(plotOutput("Total Tested"))
             ),
    
    "Hospital Data",
    tabPanel("New Hospital Admissions"),
    tabPanel("Cumulative Hospital Admissions"),
    tabPanel("New Hospital Discharges"),
    tabPanel("Cumulative Hospital Discharges"),
    "Mortality Data",
    tabPanel("Deaths"),
    tabPanel("Total Deaths")
  )
)

server <- function(input, output) {
  
  data <- reactive({dat})
  
  output$"New Positive Labs" <- renderPlot({qplot(data()$Date, data()$`New positive labs`, geom="line")})
  output$"Total Positive Labs" <- renderPlot({qplot(data()$Date, data()$`Total positive labs`, geom="line")})
  output$"New Negative Labs" <- renderPlot({qplot(data()$Date, data()$`New negative labs`, geom="line")})
  output$"Total Negative Labs" <- renderPlot({qplot(data()$Date, data()$`Total negative labs`, geom="line")})
  output$"Total Tested" <- renderPlot({qplot(data()$Date, data()$`Total tested`, geom="line")})
}

shinyApp(ui = ui, server = server)
