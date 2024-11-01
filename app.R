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
    mainPanel(
      tabsetPanel(
        tabPanel(strong("About"),
                 br(),
                 fluidRow(strong("Purpose:"),
                          "The purpose of this project is to showcase how we can use Shiny to build an interactive dashboard
                          for exploring housing data in Melbourne from 2016 to 2018."
                          ),
                 br(),
                 fluidRow(strong("Data & Source Links:"),
                          "Some content about the data and and outbound link to the kaggle dataset will go here"
                          )
                 ),
        tabPanel(strong("Data Download"),
                 "Data Download Contents"
                 ),
        tabPanel(strong("Data Exploration"),
                 "Data Exploration Contents"
                 )
      )
    )
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