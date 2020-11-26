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
                           uiOutput("date_range"),
                           uiOutput("plot_type"),
                           uiOutput("more_features1")
                         ),
                         mainPanel(
                           plotOutput("output_plot")
                         )
                       )
              ),
              tabPanel("Analysis 2 ; Deep dive into a city",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("cities2"),
                           uiOutput("features2"),
                           uiOutput("date_range2"),
                           uiOutput("plot_type2"),
                           uiOutput("more_features2")
                         ),
                         mainPanel(
                           plotOutput("output_plot2"),
                           leafletOutput("map")
                         )
                       )
              )
  )
))

#library(shiny)
#runApp("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE/App")