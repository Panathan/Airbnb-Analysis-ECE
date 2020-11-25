library(shiny)
library(leaflet)
shinyUI(fluidPage(
  titlePanel("Jonathan BOUTAKHOT & Jordan DO BARREIRO Airbnb analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1 : Comparing cities",
                       sidebarLayout(
                         sidebarPanel(
                           titlePanel("Desired Choices"),
                           uiOutput("cities1"),
                           uiOutput("features1"),
                           uiOutput("more_features1"),
                           # dateRangeInput('dateRange',
                           #                label = 'Date range input: yyyy-mm-dd',
                           #                start = uiOutput("min_date"), end = uiOutput("max_date")
                           # ),
                           uiOutput("date_range"),
                           uiOutput("min_date"),
                           uiOutput("plot_type"),
                         ),
                         mainPanel(
                           h3("Comparing"),
                           textOutput("selected_cities1"),
                           textOutput("selecteddd"),
                           textOutput("date_range_text"),
                           textOutput("start_date"),
                           textOutput("end_date"),
                           plotOutput("output_plot"),
                           textOutput("selected_plot_type"),
                         )
                       )
              ),
              tabPanel("Analysis 2 ; Deep dive into a city",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("cities2"),
                           uiOutput("features2"),
                           uiOutput("more_features2")
                         ),
                         mainPanel(
                           leafletOutput("map")
                         )
                       )
              )
  )
))

#library(shiny)
#runApp("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE/App")