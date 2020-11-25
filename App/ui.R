library(shiny)
shinyUI(fluidPage(
  titlePanel("Jonathan BOUTAKHOT & Jordan DO BARREIRO Airbnb analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1 : Comparing cities",
                       sidebarLayout(
                         sidebarPanel(
                           titlePanel("Desired Choices"),
<<<<<<< Updated upstream
                           
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
=======
                           uiOutput("cities1"),
                           uiOutput("features1"),
                           uiOutput("more_features1"),
                           dateRangeInput('dateRange',
                                          label = 'Date range input: yyyy-mm-dd',
                                          # start = mindate, end = maxdate
                           )
                         ),
                         mainPanel(
                           h3("Comparing"),
                           textOutput("selected_cities1")
                         )
>>>>>>> Stashed changes
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