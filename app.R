library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")

#housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Melbourne Housing Project"),
  sidebarLayout(
    sidebarPanel(
      h3("Input Parameters"),
      selectInput("region", label = "Region", 
                  choices = region_vector, 
                  selected = "All"),
      checkboxGroupInput("type", label = "Housing Type", 
                         choices = c("All",type_vector),
                         selected = "All"),
      ### Need to figure date out
      dateRangeInput('saleDate',
                     label = 'Selling Date Range',
                     start = min(housing_data$Date), end = max(housing_data$Date),
                     min = min(housing_data$Date), max = max(housing_data$Date),
                     separator = " to ", format = "mm-dd-yyyy"
      ),
      sliderInput( 
        "price", "Price (Thousands AUD)", 
        min = min(housing_data$Price)/1000, max = max(housing_data$Price)/1000, 
        value = c(350, 2000) 
      ),
      actionButton("getHousing","Process My Inputs!")
    ),
    mainPanel()
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data <- reactiveValues(processed_data = NULL)
  
  observeEvent(input$getHousing,{
    if("All" %in% input$type){
      type = "All"
    } else {
      type = input$type
    }
    subset <- housing_data %>%
      filter(
        (Price <= (input$price[2] * 1000) & Price >= (input$price[1] * 1000)) &
        (Date >= input$saleDate[1] & Date <= input$saleDate[2])
      ) %>%
      {if(!("All" %in% type)) filter(., Type %in% type) else .} %>%
      {if(input$region != "All") filter(., Regionname == input$region) else .}
    
    data$processed_data <- subset
  })
}

# Run the application 
shinyApp(ui = ui, server = server)