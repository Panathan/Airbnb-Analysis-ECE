library(shiny)
library(leaflet)
#source("./Scripts/data_preparation.R")
source("../Scripts/data_preparation.R")

setwd("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE")
#setwd("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")

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

server <- function(input, output) {

  ########################################################################
  ############################## Tab 1 ###################################
  ########################################################################
  
  listings_selected_city <- reactive({
    listings %>%
      filter(city %in% input$cities1) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  output$cities1 <- renderUI({
    checkboxGroupInput("cities1", "Select cities :", choices = cities, selected = NULL)
  })
  output$selected_cities1 <- renderText({
    paste("You have selected the following cities for comparisons :",paste(input$cities1, collapse = ", "))
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
  
  output$date_range <- renderUI({
    dateRangeInput("date_range", "Select a date range : ", min = mindate, start = mindate, max = maxdate, end = maxdate)
  })
  
  output$date_range_text  <- renderText({
    paste("date range is", 
          paste(as.character(input$date_range), collapse = " to ")
    )
  })
  
  output$start_date <- renderText({
    paste("new start date is",
          paste(as.character(input$date_range[1]))
    )
  })

  output$end_date <- renderText({
    paste("new end date is",
          paste(as.character(input$date_range[2]))
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
  
  output$selected_plot_type <- renderText({
    paste("You have selected :",paste(input$type_plot))
  })
  
  output$output_plot <- renderPlot({
    
    if(!is.null(input$cities1)){
      if((input$type_plot == "histogram") && (input$feature == "availability_30")){ p <- ggplot(listings_selected_city(),aes(x=availability_30,group=city,fill=city))+geom_histogram(position="dodge",binwidth=1)
        plot(p)}
      if((input$type_plot == "histogram") && (input$feature == "price_30")){ p <- ggplot(listings_selected_city(),aes(x=price,group=city,fill=city))+geom_histogram(position="dodge",binwidth=1)
        plot(p)}
      if((input$type_plot == "histogram") && (input$feature == "revenue_30")){ p <- ggplot(listings_selected_city(),aes(x=revenue_30,group=city,fill=city))+geom_histogram(position="dodge",binwidth=2)
        plot(p)}
      if((input$type_plot == "density") && (input$feature == "availability_30")){ p <- ggplot(listings_selected_city(), aes(availability_30, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
        plot(p)}
      if((input$type_plot == "density") && (input$feature == "price_30")){ p <- ggplot(listings_selected_city(), aes(price, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
        plot(p)}
      if((input$type_plot == "density") && (input$feature == "revenue_30")){ p <- ggplot(listings_selected_city(), aes(revenue_30, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
        plot(p)}
      if((input$type_plot == "average") && (input$feature == "availability_30")){ p <- ggplot(listings_selected_city(), aes(city, average_availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$average_availability_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "average") && (input$feature == "revenue_30")){ p <- ggplot(listings_selected_city(), aes(city, average_revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$average_revenue_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "average") && (input$feature == "price_30")){ p <- ggplot(listings_selected_city(), aes(city, average_price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$average_price, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "median") && (input$feature == "availability_30")){ p <- ggplot(listings_selected_city(), aes(city, median_availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$median_availability_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "median") && (input$feature == "revenue_30")){ p <- ggplot(listings_selected_city(), aes(city, median_revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$median_revenue_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "median") && (input$feature == "price_30")){ p <- ggplot(listings_selected_city(), aes(city, median_price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$median_price, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "boxplot") && (input$feature == "availability_30")){ p <- ggplot(listings_selected_city(), aes(city, availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$availability_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "boxplot") && (input$feature == "revenue_30")){ p <- ggplot(listings_selected_city(), aes(city, revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$revenue_30, c(0.1, 0.9), na.rm = T))
        plot(p)}
      if((input$type_plot == "boxplot") && (input$feature == "price_30")){ p <- ggplot(listings_selected_city(), aes(city, price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city()$price, c(0.1, 0.9), na.rm = T))
        plot(p)}
    }
  })  
  
  
  ########################################################################
  ############################## Tab 2 ###################################
  ########################################################################
  
  listings_selected_city2 <- reactive({
    listings %>%
      filter(city %in% input$city2) %>%
      filter(date >= input$date_range2[1] & date <= input$date_range2[2])
  })
  
  output$cities2 <- renderUI({
    selectInput("city2", "Select a city :", choices = cities, selected = 'bordeaux')
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
                choices = list("None" = "NULL",
                               "Neighborhood" = "neighbourhood_cleansed",
                               "Property type" = "property_type",
                               "Room type" = "room_type",
                               "Number of bedrooms" = "bedrooms"
                ),
                selected = NULL)
  })

  output$date_range2 <- renderUI({
    dateRangeInput("date_range2", "Select a date range : ", min = mindate, start = mindate, max = maxdate, end = maxdate)
  })
  
  output$plot_type2 <- renderUI({
    selectInput("type_plot2", "Select aggregation type:",
                choices = list("None" = "NULL",
                               "Average" = "average",
                               "Median" = "median",
                               "Histogram" = "histogram",
                               "Density" = "density",
                               "Boxplot" = "boxplot"
                ),
                selected = NULL)
  })
  
  output$output_plot2 <- renderPlot({
    
    if(!is.null(input$city2)){
      if((input$type_plot2 == "histogram") && (input$feature2 == "availability_30")){ p <- ggplot(listings_selected_city2(),aes(x=availability_30,group=city,fill=city))+geom_histogram(position="dodge",binwidth=1)
      plot(p)}
      if((input$type_plot2 == "histogram") && (input$feature2 == "price_30")){ p <- ggplot(listings_selected_city2(),aes(x=price,group=city,fill=city))+geom_histogram(position="dodge",binwidth=1)
      plot(p)}
      if((input$type_plot2 == "histogram") && (input$feature2 == "revenue_30")){ p <- ggplot(listings_selected_city2(),aes(x=revenue_30,group=city,fill=city))+geom_histogram(position="dodge",binwidth=2)
      plot(p)}
      if((input$type_plot2 == "density") && (input$feature2 == "availability_30")){ p <- ggplot(listings_selected_city2(), aes(availability_30, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
      plot(p)}
      if((input$type_plot2 == "density") && (input$feature2 == "price_30")){ p <- ggplot(listings_selected_city2(), aes(price, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
      plot(p)}
      if((input$type_plot2 == "density") && (input$feature2 == "revenue_30")){ p <- ggplot(listings_selected_city2(), aes(revenue_30, group=city, fill=city)) +geom_density(adjust=1.5, alpha=.4)
      plot(p)}
      if((input$type_plot2 == "average") && (input$feature2 == "availability_30")){ p <- ggplot(listings_selected_city2(), aes(city, average_availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$average_availability_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "average") && (input$feature2 == "revenue_30")){ p <- ggplot(listings_selected_city2(), aes(city, average_revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$average_revenue_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "average") && (input$feature2 == "price_30")){ p <- ggplot(listings_selected_city2(), aes(city, average_price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$average_price, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "median") && (input$feature2 == "availability_30")){ p <- ggplot(listings_selected_city2(), aes(city, median_availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$median_availability_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "median") && (input$feature2 == "revenue_30")){ p <- ggplot(listings_selected_city2(), aes(city, median_revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$median_revenue_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "median") && (input$feature2 == "price_30")){ p <- ggplot(listings_selected_city2(), aes(city, median_price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$median_price, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "boxplot") && (input$feature2 == "availability_30")){ p <- ggplot(listings_selected_city2(), aes(city, availability_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$availability_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "boxplot") && (input$feature2 == "revenue_30")){ p <- ggplot(listings_selected_city2(), aes(city, revenue_30)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$revenue_30, c(0.1, 0.9), na.rm = T))
      plot(p)}
      if((input$type_plot2 == "boxplot") && (input$feature2 == "price_30")){ p <- ggplot(listings_selected_city2(), aes(city, price)) + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +scale_y_continuous(limits = quantile(listings_selected_city2()$price, c(0.1, 0.9), na.rm = T))
      plot(p)}
    }
  })
  
  output$map <- renderLeaflet({
      listings_selected_city2() %>%
        leaflet() %>% 
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions())
  })
}