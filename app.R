#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(countrycode)
library(plotly)
library(shiny)
library(reshape2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(stringr)

origdata <- read.csv("laurel-world-happiness-report-data/data/data_behind_table_2_1_whr_2017.csv")
happiness <- select(origdata, country:perceptions_of_corruption) %>%
  mutate(confidence_in_gov = origdata$confidence_in_national_government, Gini_Income = origdata$gini_of_household_income_reported_in_gallup_by_wp5_year, Gini_Average = origdata$gini_index_world_bank_estimate_average_2000_13) %>%
  mutate(continent =   countrycode(sourcevar = happiness$country, origin = "country.name",destination = "continent"))
happiness$continent[happiness$country == "Kosovo"] <- "Europe"

# Define UI for Global Happiness Data
ui <- navbarPage("Global Happiness Index",
                 tabPanel("Plots",
                          fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              # Year Selection
                              sliderInput(inputId = "yearSelect",
                                          label = "Year (2007-2016):",
                                          min = min(happiness$year),
                                          max = max(happiness$year),
                                          value = max(happiness$year)
                                          ),
                              # Select Y
                              selectInput("y",
                                          "Y Axis:",
                                          #choices = c(choices = str_to_title(str_replace_all(names, "_"," ")) = colnames(happiness)),
                                          choices = colnames(happiness),
                                          selected = "life_ladder"),
                              selectInput("x",
                                          "X Axis:",
                                          # better way to do choices??
                                          choices = colnames(happiness),
                                          selected = "confidence_in_gov"),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output Scatter
                            mainPanel(
                            plotlyOutput("scatter")
                            )
                          )
                          ),
                          fluidRow(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("continent","Continent:",
                                            c("All",unique(happiness$continent))),
                                selectInput("measure","Measure:",
                                            choices = colnames(happiness))
                              ),
                            mainPanel(
                              plotlyOutput("bar")
                            )))),
                 # Data Table
                 tabPanel(title = "Table",
                          inputPanel(
                            downloadButton("downloadData","Download Global Happiness Data")
                            ),
                          fluidPage(DT::dataTableOutput("table"))
                          ),
                 fluid = TRUE)

                
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
  #Reactive filtered data
    dataInput <- reactive({
      data <- happiness %>%
        #Slider Filter
        filter(year == input$yearSelect)
      return(data)
    }
    )
         
     output$scatter <- renderPlotly({
       data <- happiness
       data <- dataInput()
         ggplotly(ggplot(data, aes_string(x = input$x, y = input$y, color = continent)) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  geom_point() +
                  geom_smooth())
       })
  #Render Data Table
     output$table <- DT::renderDataTable({
       data2 <- swInput()
       
       subset(data2, select = c(continent, country, year, life_ladder, happiness$log_gdp_per_capita, happiness$social_support, happiness$Gini_Average))
     })
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSliderInput(session = session, 
                      inputId = "yearSelect",
                      selected = max(happiness$year))
    updateSelectInput(session = session,
                      inputId = "y",
                      selected = "life_ladder")
    updateSelectInput(session = session,
                      inputId = "x",
                      selected = "confidence_in_gov")
    showNotification("You have reset the application!!! <3", type = "warning", duration = 2)
  })
  }
  
# Run the application 
shinyApp(ui = ui, server = server)

