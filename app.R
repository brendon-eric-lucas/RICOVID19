library(googlesheets4)
library(ggplot2)
library(xts)
library(zoo)
library(shiny)

ui <- fluidPage(
  # graphic
  
  # App title ----
  titlePanel(h3(align="center", style="background-color: #93CCEA", "Rhode Island COVID-19 Tracker")),
  
  # Creates navlist panel screen right ----
  navlistPanel(
    "Testing Data", # testing data panels
    
    tabPanel("New Positive Labs", 
              mainPanel(plotOutput("New Positive Labs"), actionButton("update_npl", "Update")) 
            ),
    
    tabPanel("Total Positive Labs", 
              mainPanel(plotOutput("Total Positive Labs"), actionButton("update_tpl", "Update"))
             ),
    
    tabPanel("New Negative Labs", 
              mainPanel(plotOutput("New Negative Labs"), actionButton("update_nnl", "Update"))
             ),
    
    tabPanel("Total Negative Labs", 
              mainPanel(plotOutput("Total Negative Labs"), actionButton("update_ntl", "Update"))
             ),
    
    tabPanel("Total Tested",
              mainPanel(plotOutput("Total Tested"), actionButton("update_tt", "Update"))
             ),
    
    "Hospital Data", # hospital data panels
    
    tabPanel("New Hospital Admissions",
              mainPanel(plotOutput("New Hospital Admissions"), actionButton("update_nha", "Update"))
             ),
    
    tabPanel("Cumulative Hospital Admissions",
              mainPanel(plotOutput("Cumulative Hospital Admissions"), actionButton("update_cha", "Update"))
             ),
    
    tabPanel("New Hospital Discharges",
              mainPanel(plotOutput("New Hospital Discharges"), actionButton("update_nhd", "Update"))
             ),
    
    tabPanel("Cumulative Hospital Discharges",
              mainPanel(plotOutput("Cumulative Hospital Discharges"), actionButton("update_chd", "Update"))
             ),
    
    "Mortality Data", # mortality data panels
    
    tabPanel("Deaths",
              mainPanel(plotOutput("Deaths"), actionButton("update_d", "Update"))
             ),
    
    tabPanel("Total Deaths",
              mainPanel(plotOutput("Total Deaths"), actionButton("update_td", "Update"))
             )
  )
)

server <- function(input, output) {
  # set default access for a public google shhet
  gs4_deauth()
  # render google sheet as dataframe
  dat <- read_sheet("https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272",
                    sheet = "Trends")
  # create reactive dfrm
  data <- reactive(quote(dat), quoted = TRUE)
  # create reactive date time object
  dt <- reactive({dat$Date})
  # create reactive dfrm for use with xts
  dat2 <- reactive({subset(dat, select = -Date)})
  
  # rename total columns
  names(dat)[names(dat) == 'Total positive labs'] <- 'total_positive_labs'
  names(dat)[names(dat) == 'Total negative labs'] <- 'total_negative_labs'
  names(dat)[names(dat) == 'Total tested'] <- 'total_tested'
  names(dat)[names(dat) == 'Cumulative hospital admissions'] <- 'cumulative_hospital_admits'
  names(dat)[names(dat) == 'Cumulative hospital discharges'] <- 'cumulative_hospital_discharges'
  names(dat)[names(dat) == 'Total deaths'] <- 'total_deaths'
  
  # create xts objects
  npl.data <- reactive({
    ts <- xts(dat2()$`New positive labs`, dt())
    ma <- rollmean(ts, length(dt())/7)
    dfrm <- na.locf(merge(ts, ma))
  })
  
  nnl.data <- reactive({
    ts <- xts(dat2()$`New negative labs`, dt())
    ma <- rollmean(ts, length(dt())/7)
    na.locf(merge(ts, ma))
  })
  
  nha.data <- reactive({
    ts <- xts(dat2()$`New hospital admissions`, dt())
    ma <- rollmean(ts, length(dt())/7)
    na.locf(merge(ts, ma))
  })
  
  nhd.data <- reactive({
    ts <- xts(dat2()$`New hospital discharges`, dt())
    ma <- rollmean(ts, length(dt())/7)
    na.locf(merge(ts, ma))
  })
  
  d.data <- reactive({
    ts <- xts(dat2()$`Deaths`, dt())
    ma <- rollmean(ts, length(dt())/7)
    na.locf(merge(ts, ma))
  })
  
  # plotting for testing data
  output$"New Positive Labs" <- renderPlot({
    plot(npl.data(), bg="#D3D3D3", 
         main='New Positive Labs \nWith Seven Day Rolling Mean', 
         yaxis.right=FALSE, 
         grid.ticks.lwd=0)
  })
  
  output$"Total Positive Labs" <- renderPlot({
    ggplot(data(), aes(x=Date, y=total_positive_labs)) + 
    geom_area(fill="blue", alpha=0.2) + geom_line() +
    labs(x = "Date", y = "Total Positive Labs") +
    theme(panel.background = element_rect(fill = "#D3D3D3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  })
  
  output$"New Negative Labs" <- renderPlot({
    plot(nnl.data(), bg="#D3D3D3", 
         main='New Negative Labs  \nWith Seven Day Rolling Mean', 
         yaxis.right=FALSE, 
         grid.ticks.lwd=0)
  })
  
  output$"Total Negative Labs" <- renderPlot({
    ggplot(data(), aes(x=Date, y=total_negative_labs)) + 
    geom_area(fill="blue", alpha=0.2) + geom_line() +
    labs(x = "Date", y = "Total Negative Labs") +
    theme(panel.background = element_rect(fill = "#D3D3D3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  })
  
  output$"Total Tested" <- renderPlot({
    ggplot(data(), aes(x=Date, y=total_tested)) + 
    geom_area(fill="blue", alpha=0.2) + geom_line() + 
    labs(x = "Date", y = "Total Tested") +
    theme(panel.background = element_rect(fill = "#D3D3D3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  })
    
  # plotting for hospital data
  output$"New Hospital Admissions" <- renderPlot({
    plot(nha.data(), bg="#D3D3D3", 
         main='New Hospital Admissions \nWith Seven Day Rolling Mean', 
         yaxis.right=FALSE, 
         grid.ticks.lwd=0)
  })
  
  output$"Cumulative Hospital Admissions" <- renderPlot({
    ggplot(data(), aes(x=Date, y=cumulative_hospital_admits)) + 
    geom_area(fill="blue", alpha=0.2) + geom_line() + 
    labs(x = "Date", y = "Cumulative Hospital Admissions") +
    theme(panel.background = element_rect(fill = "#D3D3D3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  })
  
  output$"New Hospital Discharges" <- renderPlot({
    plot(nhd.data(), bg="#D3D3D3",
         main='New Hospital Discharges \nWith Seven Day Rolling Mean', 
         yaxis.right=FALSE, 
         grid.ticks.lwd=0)
  })
  
  output$"Cumulative Hospital Discharges" <- renderPlot({
    ggplot(data(), aes(x=Date, y=cumulative_hospital_discharges)) + 
      geom_area(fill="blue", alpha=0.2) + geom_line() +
      labs(x = "Date", y = "Cumulative Hospital Discharges") +
      theme(panel.background = element_rect(fill = "#D3D3D3"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  # plotting for mortality data
  output$"Deaths" <- renderPlot({
    plot(d.data(), bg="#D3D3D3",
         main='Daily Mortality \nWith Seven Day Rolling Mean', 
         yaxis.right=FALSE, 
         grid.ticks.lwd=0)
  })
  
  output$"Total Deaths" <- renderPlot({
    ggplot(data(), aes(x=Date, y=total_deaths)) + 
    geom_area(fill="blue", alpha=0.2) + 
    geom_line() +
    labs(x = "Date", y = "Total Deaths") +
    theme(panel.background = element_rect(fill = "#D3D3D3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  })
}

shinyApp(ui = ui, server = server)
