library(shiny)
shinyUI(fluidPage(
  titlePanel("Jonathan BOUTAKHOT & Jordan DO BARREIRO Airbnb analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1 : Comparing cities",
                       sidebarLayout(
                         sidebarPanel(
                           titlePanel("Desired Choices"),
                           
                             uiOutput("cities1"),
                             
                             uiOutput("features"),
                              
                             dateRangeInput('dateRange',
                                            label = 'Date range input: yyyy-mm-dd',
                                            # start = mindate, end = maxdate
                             ),
                             
                             uiOutput("another_features"),
                           
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
#runApp("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE/App")