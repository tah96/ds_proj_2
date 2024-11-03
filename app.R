library(shiny)
library(shinyalert)
library(bslib)
library(DT)
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
      sliderInput("saleDate",
                  "Selling Date Range",
                  min = min(housing_data$Date),
                  max = max(housing_data$Date),
                  value=c(min(housing_data$Date),max(housing_data$Date))
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
                          p("The data collected is a Kaggle dataset containing housing data in Melbourne, Austrailia from
                          2016 to 2018. Details about the house such as number of rooms, number of bedrooms, lot size and so on are included.
                          Additionally, location and selling details are available include the real estate agent and date of the sale."
                          ),
                          p("Link to the dataset here:",a('https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market'))
                        ),
                 br(),
                 fluidRow(strong("Navigating the Site:"),
                          p("To navigate the site, you will see three tabs at the top of this section to learn more about
                            the dataset, download the data, and explore the data."),
                          p("On the lefthand side, you'll notice a series of filters you can apply to our dataset. These filters will be applied
                            to data in your download and data exploration")
                          ),
                 br(),
                 fluidRow(img(src="data/austrailia.png"))
                 ),
        tabPanel(strong("Data Download"),
                 br(),
                 DT::dataTableOutput("dataTable"),
                 br(),
                 downloadLink('downloadData', 'Download')
                 ),
        tabPanel(strong("Data Exploration"),
                 tabsetPanel(
                   tabPanel(strong("Categorical Variables"),
                            column(3,
                                   checkboxGroupInput("typeExp", label = "Housing Type", 
                                                          choices = c("All",type_vector),
                                                          selected="All"),
                                   checkboxGroupInput("methodExp",label = "Selling Method",
                                                      choices = c("All",method_vector),
                                                      selected = "All"
                                                      ),
                                   checkboxGroupInput("regionExp",label="Region",
                                                      choices = region_vector,
                                                      selected = "All"
                                                      ),
                                   selectInput("sellerExp", label = "Real Estate Agent", 
                                               choices = seller_vector, 
                                               selected = seller_vector[1]),
                                   ## Likely will have to come update
                                   checkboxGroupInput("roomsExp",label="Number Rooms",
                                                      choices = room_vector,
                                                      selected = "All"
                                    ),
                                   checkboxGroupInput("bathroomsExp",label="Number Bathrooms",
                                                      choices = bathroom_vector,
                                                      selected = "All"
                                   ),
                                   actionButton("getCategoricacl","Show me the categorical summaries")
                                   )
                            ),
                   tabPanel(strong("Numeric Variable"),
                            h3("Numeric variable placeholder")
                    ),
                   tabPanel(strong("Combination Summaries/Plots"),
                            h3("Combination placeholder")
                            )
                  )
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
  
  output$dataTable = DT::renderDataTable({
    data$processed_data
  })
  
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(data$processed_data, con)
       }
     )
}

# Run the application 
shinyApp(ui = ui, server = server)