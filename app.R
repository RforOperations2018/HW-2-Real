#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(countrycode)
library(plotly)
library(shiny)
library(reshape2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(stringr)
library(lubridate)

origdata <- read.csv("laurel-world-happiness-report-data/data/data_behind_table_2_1_whr_2017.csv")

happiness <- origdata %>%
  mutate(confidence_in_gov = confidence_in_national_government, 
         Gini_Income = gini_of_household_income_reported_in_gallup_by_wp5_year, 
         Gini_Average = gini_index_world_bank_estimate_average_2000_13,
         continent = countrycode(sourcevar = country, origin = "country.name",destination = "continent"),
         country = as.factor(ifelse(country == "Kosovo", "Europe", as.character(country)))) %>%
  select(country:perceptions_of_corruption, Gini_Income, Gini_Average, confidence_in_gov, continent)

# Define UI for Global Happiness Data
ui <- navbarPage("Global Happiness Index",
                 tabPanel("Plots",
                          fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              # Year Selection
                              sliderInput(inputId = "yearSelect",
                                          label = "Year (2005-2016):",
                                          min = min(happiness$year),
                                          max = max(happiness$year),
                                          sep = "", # I would use this in the future for a year slider. It removes the nasty commas.
                                          value = max(happiness$year),step = 1,round = T, format = "####"),
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
                          #Bar Chart
                          fluidRow(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("continent","Continent:",
                                            c("All",unique(happiness$continent))),
                                selectInput("measure","Measure:",
                                            choices = colnames(happiness),
                                            selected = "Gini_Income")
                              ),
                              mainPanel(
                               plotlyOutput("bar")
                            ))),
                          fluidRow(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("countryline","Country:",
                                            choices = c("All",unique(happiness$country)),
                                            selected = "All",
                                            multiple = T)),
                              mainPanel(
                                plotlyOutput("line")
                              ))
                          )),
                 # Data Table
                 tabPanel(title = "Table",
                          inputPanel(
                            downloadButton("downloadData","Download Global Happiness Data")
                            ),
                          fluidPage(DT::dataTableOutput("table"))
                          ), fluid = TRUE)

                
  # Define server logic required to draw a histogram
  server <- function(input, output, session = session) {
    
  #Reactive filtered data for Scatter
    dataInput <- reactive({
      data <- happiness %>%
        #Slider Filter
         filter(year == input$yearSelect)         %>% # %>%
        # melt(id = c("country",input$x, input$y))
        mutate(year = as.Date(paste(year, 1, 1, sep = "-")))
      
    })
    
  #Line data
    output$line <- renderPlotly({
      data <- happiness
      if (input$countryline != "All") {
        data <- data[data$country == input$countryline,]
      }
      ggplotly(ggplot(data, aes(x = year, y = life_ladder, group = input$countryline)) + geom_line())
    })
      
    
    #Reactive Bar Data
    barInput <- reactive({
      barhappiness <- happiness
      if (input$continent != "All"){
        barhappiness <- barhappiness[barhappiness$continent == input$continent,]
      }
      
      data <- barhappiness %>%
        melt(id = c("country", "continent"))
    })
    
    #Bar
    output$bar <- renderPlotly({
      data <- barInput() %>%
        filter(variable %in% c(input$measure, "continent"))
      ggplotly(
        ggplot(data = data, aes(x = country, y = value, fill = continent)) +
          geom_col(position = 'dodge')+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))) 
    })
    
    # Scatter plot
     output$scatter <- renderPlotly({
       data <- dataInput()
         # ggplotly(ggplot(data, aes_string(x = input$x, y = input$y, color = data$continent) +
         #          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
         #          geom_point() +
         #            labs(x = input$x, y = input$y)
         #          )
         #          )
         #          })
       
       melthappy <- melt(data = data, c("country", input$x, input$y, "continent"))
       ggplotly(ggplot(melthappy, aes_string(x = input$x, y = input$y, color = "continent")) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                  geom_point() +
                                  labs(x = input$x, y = input$y)
                                  )
                                  })
       
     
  #Render Data Table
     output$table <- DT::renderDataTable({
       data2 <- dataInput()
       data2 <- subset(data2, select = c(continent, country, year, life_ladder, log_gdp_per_capita))
     })
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSliderInput(session = session, 
                      inputId = "yearSelect",
                      # This was broken for Slider inputs its value
                      value = max(happiness$year))
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

