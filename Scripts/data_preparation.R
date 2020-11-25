library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

setwd("C:/Users/Jordan/Desktop/Cours/ING5/Data analytics/Airbnb-Analysis-ECE")
#setwd("C:/Users/Jonat/OneDrive/Bureau/ECE ING5/Data Analytics/Our_Project/Airbnb-Analysis-ECE")

# Creates a dataframe containing the country, region, City, date, listing_url :

get_urls_df <- function()
{
  #read from .csv file
  urls_path <- file.path("./Data/all_data_urls.csv")
  print(paste0("reading data from ", urls_path))
  urls_df <- read.csv(urls_path)

  #split different features
  urls_features <- str_split_fixed(urls_df$listings_data_url, "/", 8)
  
  urls_df$country <- urls_features[,4]
  urls_df$city <- urls_features[,6]
  urls_df$date <- urls_features[,7]

  #apply filters : 3 different countries (france without paris, spain, belgium) and the last 3 available scraping dates of data
  urls_df <- urls_df %>%
    filter(country %in% c("france","spain","belgium")) %>%
    filter(city !="paris")
  urls_df <- urls_df %>%
    group_by(city) %>%
    slice_max(order_by = date, n = 3)

  print("urls loaded")
  return(urls_df)
}

# Prepares data for a specific url, country, city, date : 
prepare_data <- function(url, country, city, date)
{
  # Cleaning listings dataframe

  #load data from url
  f <- tempfile()
  download.file(url, f)
  listings <- read.csv(gzfile(f))

  # Add Keys: columns country, city and day date
  listings$country <- country
  listings$city <- city
  listings$date <- date

  # Select interesting columns
  columns_listings <- c("country", "city", "date", "id", "neighbourhood_cleansed",
                        "latitude", "longitude",
                        "property_type", "room_type", "accommodates", "bedrooms",
                        "beds", "price", "minimum_nights",  "maximum_nights", "availability_30")

  listings <- listings %>%
    select(columns_listings) %>%
    arrange(id)
  
  #calculate revenue
  listings$price <- as.numeric(str_remove(listings$price, "[$]"))
  listings$price[is.na(listings$price)] <- 0
  listings$availability_30 <- as.numeric(listings$availability_30)
  listings$availability_30[is.na(listings$availability_30)] <- 30
  listings$revenue_30 <- (30-listings$availability_30) * listings$price

  dir.create(file.path("./Data/data_cleansed", country, city, date), recursive = TRUE)
  write.csv(listings, file.path("./Data/data_cleansed", country, city, date, "listings.csv"))
  print(paste0("saving data into ", file.path("./Data/data_cleansed", country, city, date, "listings.csv")))
}

#downloads data from all urls
dl_data <- function(){
  urls_df <- get_urls_df()
  for(i in 1:nrow(urls_df)){
    prepare_data(urls_df[i,]$listings_data_url, urls_df[i,]$country, urls_df[i,]$city, urls_df[i,]$date)
  }
}

#Uncomment the following to download data
#dl_data()

## Once data for multiple cities are prepared
## We can read these data and concatenate them together into one dataframe
get_cleansed_df <- function(){
  # Reading cleansed data
  countries <- c("france","spain","belgium")
  files_paths <- c()

  # # Read data in cities between min_date and max_date
  for(country in countries){
    file_dir <- file.path(".", "./Data/data_cleansed", country)
    file_subdirs_cities <- list.dirs(file_dir,recursive=FALSE)

    for(file_subdir_city in file_subdirs_cities){
      file_subdirs_dates <- list.dirs(file_subdir_city,recursive=FALSE)
      files_paths <- c(files_paths, file_subdirs_dates)
    }
  }

  files_paths <- file.path(files_paths, "listings.csv")
  listings <-
    do.call(rbind,
            lapply(files_paths, read.csv, row.names=1))
  # ## Preprocess
  listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
  return(listings)
}
