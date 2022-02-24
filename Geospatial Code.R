#Geospatial Analysis using Google Maps for Coordinate-Based Visualizations

  #A- - Load libraries and register your API Key

library(googleway)
library(dplyr)
library(ggmap)
register_google(key = "API_KEY")
key <- #API_KEY

  #B- - Dedicating Service Area, in this example Northeast Colorado. 
#upper_right - (41.00, -102.00)
#upper_left- (41.00, -105.00)
#bottom_left - (39.00, -105.00)
#bottom_right - (39.00, -102.00)

min_lat <- 39.0
max_lat <- 41.00
min_lon <- -105.00
max_lon <- -102.00

#each search must be 31 miles from the next 
#(because radius maxes at 50000 m)
#about 0.5 coordinate degree
#therefore a grid of at least 0.5 degree is needed:

lat_range <- seq(min_lat, max_lat, 0.50)
lon_range <- seq(min_lon, max_lon, 0.50)
length <- length(lat_range) * length(lat_range)
empty_list <- vector("list", length = length)
length <- length(lat_range) * length(lon_range)
latitude <- rep(lat_range, length / length(lat_range))
longitude <- rep(lon_range, length / length(lon_range))
coordinates <- Map(c, latitude, longitude)

  #C- - Building the framework for collecting search results

#Function to create a dataframe from one search call (there are three total search calls). 

format_res <- function(res) {
  
  setNames(
    cbind(
      googleway::access_result(res, "coordinates")
      , googleway::access_result(res, "place_name")
    )
    , c("lat", "long", "name")
  )
}

#In this function a full search is executed, using a Sys.sleep(2) call to ensure the API is not overwhelmed. Credit given: SymbolixAU on Stack Overflow (2022). https://stackoverflow.com/users/5977215/symbolixau

do_search <- function(search_string, location, key, radius, page_token = NULL) {
  
  google_places(
    search_string = search_string
    , location = location
    , key = key
    , radius = radius
    , page_token = page_token
  )
}

full_search <- function(search_string, location, key, radius) {
  
  counter <- 0
  
  page_token <- NULL ## can start on NULL because it means we're doing the first query
  is_another_page <- TRUE 
  
  
  while( is_another_page ) {
    
    res <- do_search(search_string, location, key, radius, page_token)
    
    if( res$status == "OK" ) { ## check a valid result was returned
      
      if( counter == 0 ) {
        df <- format_res( res )
      } else {
        df <- rbind(df, format_res( res ) )
      }
      
      counter <- counter + 1
    }
    
    page_token <- res[["next_page_token"]]
    is_another_page <- !is.null( page_token )
    Sys.sleep(2)  ## Sleep the function before the next call because there's a time limit
  }
  return(df)
}

  #D Running the function through the list of coordinate pairs 

#Set your search parameters

search_string <- "Urgent Care Center"
radius <- 50000

counter <- 0
for (i in coordinates) {
  j <- unlist(i)
  if ( counter == 0 ) {
    service_area <- full_search(search_string, j, key, radius)
  }
  else {
  df <- full_search(search_string, j, key, radius)
  service_area <- rbind(service_area, df)
  }
  counter <- counter + 1}

    ##Remove duplicates

nrow(service_area)
service_area <- dplyr::distinct(service_area)
nrow(service_area)

  ##E - - Export this dataframe to your computer
#Don't forget to set working directory

write.table(service_area, file = "Northeast CO Urgent Care Center Locations", sep = "\t", quote = FALSE, eol = "\n", row.names = FALSE)
