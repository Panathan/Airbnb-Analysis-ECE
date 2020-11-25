library(shiny)
shinyUI(fluidPage(
  titlePanel("Jonathan BOUTAKHOT & Jordan DO BARREIRO Airbnb analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1 : Comparing cities",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("city", 
                                       h3("Select cities:"), 
                                       choices = list("Bordeaux" = "bordeaux", 
                                                      "Lyon" = "lyon"
                                       ),
                                       selected = "bordeaux")
                         ),
                         mainPanel(
                           h3("Comparing")
                         )
                       )
              ),
              tabPanel("Analysis 2 ; Deep dive into a city",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("city", 
                                       h3("Select city:"), 
                                       choices = list("Bordeaux" = "bordeaux", 
                                                      "Lyon" = "lyon"
                                       ),
                                       selected = "bordeaux")
                         ),
                         mainPanel(
                           h3("map")
                         )
                       )
              )
  )
))

#library(shiny)
#runApp("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE/App")