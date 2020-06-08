library(googlesheets4)
library(ggplot2)
library(shiny)

gs4_deauth()
ss <- "https://docs.google.com/spreadsheets/d/1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q/edit#gid=590763272"
dat <- read_sheet(ss, sheet = "Trends")

ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Rhode Island COVID-19 Tracker"))
  
  
)

server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)
