library(shiny)
library(googleVis)
source("../Scripts/data_preparation.R")

setwd("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE")

listings <- get_cleansed_df()

cities <- unique(listings$city)

mindate <- min(listings$date)
maxdate <- max(listings$date)

server <- function(input, output) {

########################################################################
############################## Tab 1 ###################################
########################################################################
  
  

########################################################################
############################## Tab 2 ###################################
########################################################################
}