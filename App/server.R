library(shiny)
library(googleVis)
source("../Scripts/data_preparation.R")

setwd("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")

listings <- get_cleansed_df()

# print("Start downloading...")
# listings <- load_global_listings()
# print("Done !")

cities <- unique(listings$city)

mindate <- min(listings$date)
maxdate <- max(listings$date)

server <- function(input, output) {

########################################################################
############################## Tab 1 ###################################
########################################################################

  
  # listings_country <- reactive({
  #   listings %>% 
  #     filter(country == input$country)
  # })
  # 
  # listings_cities <- reactive({
  #   
  #   if (is.null(input$city_tab1)){
  #     return(NULL)
  #   }
  
  
<<<<<<< Updated upstream
 
  
=======
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ", start = mindate, end = maxdate)
    )
  })
  
  ########################################################################
  ############################## Tab 2 ###################################
  ########################################################################
    
  output$cities2 <- renderUI({
    selectInput("city2", "Select a city :", choices = cities, selected = NULL)
  })
>>>>>>> Stashed changes
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ", start = mindate, end = maxdate)
    )
  })
  
  
  output$features <- renderUI({
    selectInput("feature", "Select a feature :",
                choices = list("Price over 30 days" = "price_30",
                               "Availability over 30 days" = "availability_30",
                               "Revenue over 30 days" = "revenue_30"
                ),
                selected = 1)
  })
  
  output$cities1 <- renderUI({ 
    checkboxGroupInput("cities1", "Select Cities:", choices = cities, selected = NULL)
  })
    
  output$another_features <- renderUI({
    selectInput("feature2", "Select another feature :",
                choices = list("None" = NULL,
                               "Room type" = "room_type", 
                               "Number of Bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })
  
  

########################################################################
############################## Tab 2 ###################################
########################################################################
}