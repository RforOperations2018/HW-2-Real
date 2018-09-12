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
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              
                              
                              
                              
                              
                              
                            )
                          )
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          )
                 
                 
  
   
                   # Application title
                   titlePanel("Old Faithful Geyser Data"),
                   
                   # Sidebar with a slider input for number of bins 
                   sidebarLayout(
                      sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                         plotOutput("distPlot")
                      )
                   )
                )
                
                # Define server logic required to draw a histogram
                server <- function(input, output) {
                   
                   output$distPlot <- renderPlot({
                      # generate bins based on input$bins from ui.R
                      x    <- faithful[, 2] 
                      bins <- seq(min(x), max(x), length.out = input$bins + 1)
                      
                      # draw the histogram with the specified number of bins
                      hist(x, breaks = bins, col = 'darkgray', border = 'white')
                   })
                }

# Run the application 
shinyApp(ui = ui, server = server)

