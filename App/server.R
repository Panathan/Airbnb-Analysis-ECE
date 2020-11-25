library(shiny)
library(leaflet)
#source("./Scripts/data_preparation.R")
source("../Scripts/data_preparation.R")

#setwd("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE")
setwd("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")

listings <- get_cleansed_df()

cities <- unique(listings$city)

mindate <- min(listings$date)
#2020-04-19
maxdate <- max(listings$date)
#2020-09-19

temp <- listings %>%
  group_by(city)%>%
  summarise(average_availability_30=mean(availability_30), 
            median_availability_30=median(availability_30),
            average_revenue_30=mean(revenue_30), 
            median_revenue_30=median(revenue_30),
            average_price=mean(price), 
            median_price=median(price))

listings <- listings %>% left_join(temp, by = c("city" = "city"))
print(listings)

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
                choices = list("None" = "NULL",
                               "Neighborhood" = "neighbourhood_cleansed",
                               "Property type" = "property_type",
                               "Room type" = "room_type",
                               "Number of bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })
  
  output$min_date <- renderText({
    paste(mindate)
  })
  
  output$max_date <- renderText({
    paste(maxdate)
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
                               "Density" = "density",
                               "Boxplot" = "boxplot"
                ),
                selected = NULL)
  })
  
  output$output_plot <- renderPlot({
    
    listings_selected_city <- listings[which(listings$city == input$cities1),]

 
    p <- ggplot(listings_selected_city,aes(x=availability_30,group=city,fill=city))+
      geom_histogram(position="dodge",binwidth=0.75)
    plot(p)
    
    
    if((input$type_plot == "histogram") && (input$feature == "availability_30")){
      p2 <-ggplot( listings_selected_city, aes(availability_30)) +
        geom_histogram(aes(color = city, fill = city),
                       position = "identity", bins = 30, alpha = 0.4) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        scale_fill_manual(values = c("#00AFBB", "#E7B800"))
      plot(p2)
      
    }
    

    
    if((input$type_plot == "density") && (input$feature == "revenue_30")){
      p2 <- ggplot(listings_selected_city, aes(revenue_30, group=city, fill=city)) +
        geom_density(adjust=1.5, alpha=.4)
      plot(p2)
    }
      
    
    # ggplot(listings_selected_city, aes(revenue_30))+
    #   geom_density(color="darkblue", fill="lightblue")
    
    if((input$type_plot == "average") && (input$feature == "availability_30")){
      ggplot(listings_selected_city, aes(city, average_availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
        scale_y_continuous(limits = quantile(listings_selected_city$average_availability_30, c(0.1, 0.9), na.rm = T))
    }
    if((input$type_plot == "average" && input$feature == "revenue_30")){
      ggplot(listings_selected_city, aes(city, average_revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
        scale_y_continuous(limits = quantile(listings_selected_city$average_revenue_30, c(0.1, 0.9), na.rm = T))
    }
    
  })  
  
  
  ########################################################################
  ############################## Tab 2 ###################################
  ########################################################################
    
  output$cities2 <- renderUI({
    selectInput("city2", "Select a city :", choices = cities, selected = 'bordeaux')
  })
  
  output$selected_city2<- renderText({
    paste("You have selected :",paste(input$city2))
  })
  
  data_tab2 <- reactive({
    listings %>% 
      filter(city == input$city2)
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
                choices = list("None" = "NULL",
                               "Neighborhood" = "neighbourhood_cleansed",
                               "Property type" = "property_type",
                               "Room type" = "room_type",
                               "Number of bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })

  output$map <- renderLeaflet({
      listings_selected_city <- listings[which(listings$city == input$city2),]
      listings_selected_city %>%
        leaflet() %>% 
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions())
  })
}