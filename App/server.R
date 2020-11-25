library(shiny)
library(googleVis)
source("../Scripts/data_preparation.R")

# setwd("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")

listings <- get_cleansed_df()
print(listings)
cities <- unique(listings$city)

mindate <- min(listings$date)
#2020-04-19
maxdate <- max(listings$date)
#2020-09-19

server <- function(input, output) {

  ########################################################################
  ############################## Tab 1 ###################################
  ########################################################################
  
  output$cities1 <- renderUI({
    checkboxGroupInput("cities1", "Select cities :", choices = cities, selected = NULL)
  })
  output$selected_cities1 <- renderText({
    paste("You have selected :",paste(input$cities1, collapse = ", "))
  })
  
  output$features1 <- renderUI({
    selectInput("feature", "Select a main feature :",
                choices = list("Price over 30 days" = "price_30",
                               "Availability over 30 days" = "availability_30",
                               "Revenue over 30 days" = "revenue_30"
                ),
                selected = 1)
  })
  
  output$more_features1 <- renderUI({
    selectInput("more_feature", "Select another feature :",
                choices = list(" " = "NULL",
                               "Neighborhood" = "neighbourhood_cleansed",
                               "Property type" = "property_type",
                               "Room type" = "room_type",
                               "Number of bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ", start = mindate, end = maxdate)
    )
  })
  
  output$plot_type <- renderUI({
    selectInput("type_plot", "Select aggregation type:",
                choices = list("None" = "NULL",
                               "Average" = "average",
                               "Median" = "median",
                               "Histogram" = "histogram",
                               "Density" = "Density",
                               "Boxplot" = "boxplot"
                ),
                selected = NULL)
  })
  
  output$selecteddd <- renderText({
    paste("You have selected :",paste(input$feature, collapse = ", "))
  })
  
  
  
  output$output_plot <- renderPlot({
    
    listings_cities <- listings %>% select_if(city = input)
    
    # if(input$type_plot == "average"){
      # get_cleansed_df()%>%
      # group_by(city) %>%
      # summarise(avg = mean(switch(input$features1,
      #                             price_30 = price_30,
      #                             availability_30 = availability_30,
      #                             revenue_30 = revenue_30
      #                           )))
      # cities_selected <- input$cities1
      
      ggplot(listings, aes(city, availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
        scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T))

      
      # CREER LA DATADRAME AVEC CITIES ET LES FEATURES POUR PLOT
      # listings_cities <- data.frame(
      #   listings %>%
      #     filter(city == input$cities1,
      #            availability_30 == input$feature)
      # )
      
       # listings_cities <- data.frame(input$cities1, input$feature)
      
      # ggplot(listings_cities, aes(city, availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
      #   scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T))
      
      
      # req(input$cities1, input$more_features1)
      # plot_output_list <- lapply(input$cities1, input$more_features1, function(par) {
      #   plotname <- paste("plot", par, sep = "_")
      #   plotOutput(plotname, height = '250px', inline=TRUE)
     # }
  })  
  
  
  
  
  
  
  
    
  ########################################################################
  ############################## Tab 2 ###################################
  ########################################################################
    
  output$cities2 <- renderUI({
    selectInput("city2", "Select a city :", choices = cities, selected = NULL)
  })
  
  output$selected_city2<- renderText({
    paste("You have selected :",paste(input$city2))
  })
  
  output$features2 <- renderUI({
    selectInput("feature2", "Select a main feature :",
                choices = list("Price over 30 days" = "price_30",
                               "Availability over 30 days" = "availability_30",
                               "Revenue over 30 days" = "revenue_30"
                ),
                selected = 1)
  })
  
  output$more_features2 <- renderUI({
    selectInput("more_feature2", "Select another feature :",
                choices = list(" " = "NULL",
                               "Neighborhood" = "neighbourhood_cleansed",
                               "Property type" = "property_type",
                               "Room type" = "room_type",
                               "Number of bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })
}