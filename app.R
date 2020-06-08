library(googlesheets4)
library(ggplot2)
library(shiny)

gs4_deauth()
ss <- "https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272"
dat <- read_sheet(ss, sheet = "Trends")

ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Rhode Island COVID-19 Tracker", align="center")),
  
  navlistPanel(
    "Testing Data",
    tabPanel("New Positive Labs"),
    tabPanel("Total Positive Labs"),
    tabPanel("New Negative Labs"),
    tabPanel("Total Negative Labs"),
    tabPanel("Total Tested"),
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
  
}


shinyApp(ui = ui, server = server)
