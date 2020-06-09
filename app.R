library(googlesheets4)
library(ggplot2)
library(shiny)

# set default access for a public google shhet
gs4_deauth()
# create google sheet object
ss <- "https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272"
# render google sheet as dataframe
dat <- read_sheet(ss, sheet = "Trends")

# rename total columns
names(dat)[names(dat) == 'Total positive labs'] <- 'total_positive_labs'
names(dat)[names(dat) == 'Total negative labs'] <- 'total_negative_labs'
names(dat)[names(dat) == 'Total tested'] <- 'total_tested'
names(dat)[names(dat) == 'Cumulative hospital admissions'] <- 'cumulative_hospital_admits'
names(dat)[names(dat) == 'Cumulative hospital discharges'] <- 'cumulative_hospital_discharges'
names(dat)[names(dat) == 'Total deaths'] <- 'total_deaths'

ui <- fluidPage(
  
  # App title ----
  titlePanel(h3("Rhode Island COVID-19 Tracker")),
  
  # Creates navlist panel screen right ----
  navlistPanel(
    "Testing Data", # testing data panels
    
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
    
    "Hospital Data", # hospital data panels
    
    tabPanel("New Hospital Admissions",
              mainPanel(plotOutput("New Hospital Admissions"))
             ),
    
    tabPanel("Cumulative Hospital Admissions",
              mainPanel(plotOutput("Cumulative Hospital Admissions"))
             ),
    
    tabPanel("New Hospital Discharges",
              mainPanel(plotOutput("New Hospital Discharges"))
             ),
    
    tabPanel("Cumulative Hospital Discharges",
              mainPanel(plotOutput("Cumulative Hospital Discharges"))
             ),
    
    "Mortality Data", # mortality data panels
    
    tabPanel("Deaths",
              mainPanel(plotOutput("Deaths"))
             ),
    
    tabPanel("Total Deaths",
              mainPanel(plotOutput("Total Deaths"))
             )
  )
)

server <- function(input, output) {
  
  data <- reactive({dat})
  
  # plotting for testing data
  output$"New Positive Labs" <- renderPlot({qplot(data()$Date, data()$`New positive labs`, geom="line")})
  output$"Total Positive Labs" <- renderPlot({ggplot(data(), aes(x=Date, y=total_positive_labs)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
  output$"New Negative Labs" <- renderPlot({qplot(data()$Date, data()$`New negative labs`, geom="line")})
  output$"Total Negative Labs" <- renderPlot({ggplot(data(), aes(x=Date, y=total_negative_labs)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
  output$"Total Tested" <- renderPlot({ggplot(data(), aes(x=Date, y=total_tested)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
    
  # plotting for hospital data
  output$"New Hospital Admissions" <- renderPlot({qplot(data()$Date, data()$`New hospital admissions`, geom="line")})
  output$"Cumulative Hospital Admissions" <- renderPlot({ggplot(data(), aes(x=Date, y=cumulative_hospital_admits)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
  output$"New Hospital Discharges" <- renderPlot({qplot(data()$Date, data()$`New hospital discharges`, geom="line")})
  output$"Cumulative Hospital Discharges" <- renderPlot({ggplot(data(), aes(x=Date, y=cumulative_hospital_discharges)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
  
  # plotting for mortality data
  output$"Deaths" <- renderPlot({qplot(data()$Date, data()$`Deaths`, geom="line")})
  output$"Total Deaths" <- renderPlot({ggplot(data(), aes(x=Date, y=total_deaths)) + geom_area(fill="blue", alpha=0.2) + geom_line()})
}

shinyApp(ui = ui, server = server)
