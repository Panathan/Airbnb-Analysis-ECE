library(shiny)
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
                           dateRangeInput('dateRange',
                                          label = 'Date range input: yyyy-mm-dd',
                                          # start = min(listings$date), end = max(listings$date)
                           ),
                           uiOutput("plot_type"),
                         ),
                         mainPanel(
                           h3("Comparing"),
                           textOutput("selected_cities1"),
                           textOutput("selecteddd"),
                           plotOutput("output_plot")
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
                           h3("map"),
                           textOutput("selected_city2")
                         )
                       )
              )
  )
))

#library(shiny)
#runApp("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")