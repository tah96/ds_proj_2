library(shiny)
library(shinyalert)
library(bslib)
library(DT)
library(tidyverse)
library(shinycssloaders)
source("helpers.R")

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
                 fluidRow(tags$img(src="austrailia.jpg"))
                 ),
        tabPanel(strong("Data Download"),
                 br(),
                 DT::dataTableOutput("dataTable"),
                 br(),
                 downloadLink('downloadData', 'Download')
                 ),
        tabPanel(strong("Data Exploration"),
                 tabsetPanel(
                   tabPanel(strong("Categorical Summaries (Tables)"),
                            fluidRow(style = "background-color: #FFFF00;","Please click the Action Button to see the table or graph populated with your selections"
                            ),
                            br(),
                            column(3,
                                   selectInput("catVars", label = "Categorical Variable to Summarize", 
                                                      choices = cat_vars,
                                                      selected = cat_vars[1]),
                                   selectInput("catVarsAcross", label = "Summarize Across",
                                               choices = c("",cat_vars),
                                               selected = ""),
                                   actionButton("getCont","Generate Summary Tables")
                            ),
                            column(9,
                                   strong("One Way Contingency"),
                                   br(),
                                   shinycssloaders::withSpinner(
                                     DT::dataTableOutput("oneTable") 
                                   ),
                                   br(),
                                   br(),
                                   uiOutput("twoWayContingency"),
                                   br(),
                                   shinycssloaders::withSpinner(
                                     DT::dataTableOutput("twoTable")
                                   )
                            )
                   ),
                   tabPanel(strong("Categorical Summaries (Bar Plots)"),
                            fluidRow(style = "background-color: #FFFF00;","Please click the Action Button to see the table or graph populated with your selections"
                            ),
                            br(),
                            column(3,
                                   selectInput("catVarsGraph", label = "Categorical Variable to Summarize", 
                                               choices = cat_vars,
                                               selected = cat_vars[1]),
                                   selectInput("catVarsAcrossGraph", label = "Fill Variable",
                                               choices = c("None",cat_vars),
                                               selected = "None"),
                                   actionButton("getBar","Generate Plots"),
                                   uiOutput("getBarActionNeeded")
                            ),
                            column(9,
                                   strong("Generated Plot"),
                                   shinycssloaders::withSpinner(
                                     plotOutput("bar")
                                   )
                            )
                   ),
                   tabPanel(strong("Numeric Summaries (Tables)"),
                            fluidRow(style = "background-color: #FFFF00;","Please click the Action Button to see the table or graph populated with your selections"
                            ),
                            br(),
                            column(3,
                                   selectInput("numVars", label = "Numerical Variable to Summarize", 
                                               choices = numeric_vars,
                                               selected = numeric_vars[1]),
                                   selectInput("catVarsAcrossNum", label = "Categorical Variable to Group By",
                                               choices = c("None",cat_vars),
                                               selected = "None"),
                                   actionButton("getNumSum","Get Numeric Summaries")
                            ),
                            column(9,
                                   strong("Summary"),
                                   shinycssloaders::withSpinner(
                                     DT::dataTableOutput("numTable")
                                   )
                            )
                   ),
                   tabPanel(
                     strong("Numeric Summaries (Scatterplots)"),
                     fluidRow(style = "background-color: #FFFF00;","Please click the Action Button to see the table or graph populated with your selections"
                     ),
                            br(),
                            column(3,
                                   selectInput("xNumVar", label = "X-axis variable",
                                               choices = numeric_vars,
                                               selected = numeric_vars[1]),
                                   selectInput("yNumVar", label = "Y-axis variable",
                                               choices = numeric_vars,
                                               selected = numeric_vars[2]),
                                   selectInput("scatFill", label = "Fill variable",
                                               choices = c("None",cat_vars),
                                               selected = "None"),
                                   actionButton("getNumPlot","Generate Plot")
                                   ),
                            column(9,
                                   strong("Generated Plot"),
                                   shinycssloaders::withSpinner(
                                     plotOutput("scatter")
                                   )
                                   )
                            )
                  )
                 )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data <- reactiveValues(processed_data = NULL,one_way_table = NULL, two_way_table = NULL, single_bar = ggplot(), num_table = NULL, scatter = ggplot())
  
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
  
  observeEvent(input$getCont,{
    if(!is.null(data$processed_data)){
      data$one_way_table <- one_way_contingency(data$processed_data,input$catVars)
      if(input$catVarsAcross != '' & input$catVarsAcross != input$catVars){
        data$two_way_table <- two_way_contingency(data$processed_data,c(input$catVars,input$catVarsAcross))
        output$twoWayContingency <- renderUI({
          strong("Two Way Contingency")
        })
      } else {
        data$two_way_table <- NULL
        output$twoWayContingency <- renderUI({
          text <- paste0("You have selected ", isolate(input$catVars),". Please select another to see a two way contingency table")
          p(text)
      })
      }
    }
  })
  
  observeEvent(input$getBar,{
    if(!is.null(data$processed_data)){
      if(input$catVarsAcrossGraph == "None"){
        data$single_bar <- singleVarBar(data$processed_data,isolate(input$catVarsGraph))
        output$bar <- renderPlot({
          #single_bar <- singleVarBar(data$processed_data,isolate(input$catVarsGraph))
          plot(data$single_bar)
        })
      } else {
        data$single_bar <- fillVarBar(data$processed_data,column_name = isolate(input$catVarsGraph),
                                      fill_column = isolate(input$catVarsAcrossGraph)
        )
        output$bar <- renderPlot({
          plot(data$single_bar)
        })
      }
    }
  })
  
  observeEvent(input$getNumSum,{
    if(!is.null(data$processed_data)){
      dataTable <- summarizeNumeric(data$processed_data,numericVar = isolate(input$numVars),
                                    catVar=isolate(input$catVarsAcrossNum))
      data$num_table <- dataTable
      
      output$numTable = DT::renderDataTable(data$num_table)
    }
  })
  
  observeEvent(input$getNumPlot,{
    if(!is.null(data$processed_data)){
      data$scatter <- generateScatter(data$processed_data,x_var=isolate(input$xNumVar),
                                      y_var=isolate(input$yNumVar),
                                      fill_var = isolate(input$scatFill)
      )
        output$scatter <- renderPlot({
          plot(data$scatter)
        })
    }
  })
  
  
  output$dataTable = DT::renderDataTable({
    data$processed_data
  })
  
  output$oneTable = DT::renderDataTable({
    data$one_way_table
  })
  
  output$twoTable = DT::renderDataTable({
    data$two_way_table
  })
  
  output$numTable = DT::renderDataTable({
    data$num_table
  })
  
  output$bar = renderPlot({
    plot(data$single_bar)
  })
  
  output$scatter <- renderPlot({
    plot(data$scatter)
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