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
                           uiOutput("date_range"),
                           uiOutput("plot_type"),
                         ),
                         mainPanel(
                           h3("Comparing"),
                           textOutput("selected_cities1"),
                           textOutput("date_range_text"),
                           textOutput("start_date"),
                           textOutput("end_date"),
                           plotOutput("output_plot"),
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