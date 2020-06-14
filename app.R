library(googlesheets4)
library(ggplot2)
library(xtable)
library(xts)
library(zoo)
library(shiny)
library(lubridate)

# Instructions
instr <- paste("Hover over a point on the graph to display values above. ", 
               "Values not lying on the graph will also display, but are ",
               "not meaningful.", sep="")

ui <- fluidPage(
  
  # App title ----
  titlePanel(h3(align="center", style="background-color: #93CCEA", "Rhode Island COVID-19 Tracker")),
  
  # Creates navlist panel screen right ----
  navlistPanel(
    
    "Testing Data", # testing data panels
    
    tabPanel("New Positive Labs", 
              mainPanel(plotOutput("New Positive Labs", hover = "plot_hover"),
                        verbatimTextOutput("info_npl"),
                        p(instr)
                       
                ) 
            ),
    
    tabPanel("Total Positive Labs", 
              mainPanel(plotOutput("Total Positive Labs"))
             ),
    
    tabPanel("New Negative Labs", 
              mainPanel(plotOutput("New Negative Labs", hover = "plot_hover"),
                        verbatimTextOutput("info_nnl"),
                        p(instr)
                )
             ),
    
    tabPanel("Total Negative Labs", 
              mainPanel(plotOutput("Total Negative Labs"))
             ),
    
    tabPanel("Total Tested",
              mainPanel(plotOutput("Total Tested"))
             ),
    
    "Hospital Data", # hospital data panels
    
    tabPanel("New Hospital Admissions", 
              mainPanel(plotOutput("New Hospital Admissions", hover = "plot_hover"),
                        verbatimTextOutput("info_nha"),
                        p(instr)
                  )
             ),
    
    tabPanel("Cumulative Hospital Admissions",
              mainPanel(plotOutput("Cumulative Hospital Admissions"))
             ),
    
    tabPanel("New Hospital Discharges",
              mainPanel(plotOutput("New Hospital Discharges", hover = "plot_hover"),
                        verbatimTextOutput("info_nhd"),
                        p(instr)
                  )
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
  # set default access for a public google shhet
  gs4_deauth()
  # render google sheets as dataframe
  dat <- read_sheet("https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272",
                    sheet = "Trends")
  # create reactive dfrm
  data <- reactive(dat)
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
  
  # print date and value below plot on hover
  output$info_npl <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("(- , -)\n")
      dt <- date(strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + e$x)
      paste0("date = ", dt, " | ",  "new pos labs = ", round(e$y, 0), "\n")
    }
    paste0(
      "(Date, Value) : ", xy_str(input$plot_hover)
    )
  })
  
  output$info_nnl <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("(- , -)\n")
      dt <- date(strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + e$x)
      paste0("date = ", dt, " | ",  "new neg labs = ", round(e$y, 0), "\n")
    }
    paste0(
      "(Date, Value) : ", xy_str(input$plot_hover)
    )
  })
  
  output$info_nha <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("(- , -)\n")
      dt <- date(strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + e$x)
      paste0(" date = ", dt, " | ", "new hospital admits = ", round(e$y, 0), "\n")
    }
    paste0(
      "(Date, Value) : ", xy_str(input$plot_hover)
    )
  })
  
  output$info_nhd <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("(- , -)\n")
      dt <- date(strptime("1970-01-01", "%Y-%m-%d", tz="GMT") + e$x)
      paste0("date = ", dt, " | ", "new hosp discharges = ", round(e$y, 0), "\n")
    }
    paste0(
      "(Date, Value) : ", xy_str(input$plot_hover)
    )
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
