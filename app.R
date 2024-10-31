library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Melbourne Housing Project"),
  sidebarLayout(
    sidebarPanel(
      h2("Input Parameters"),
      selectInput("region", label = "Region", 
                  choices = region_vector, 
                  selected = region_vector[1]),
      dateRangeInput("dateOfSale", label = "Date Sold", start = min(housing_data$Date), end = max(housing_data$Date), 
                     min = min(housing_data$Date),
                     max = max(housing_data$Date), format = "mm-dd-yyyy")
    ),
    mainPanel()
  )
)

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)