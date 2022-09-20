# This script produces several CSV's that
# are used in multiple following steps

library(blackmaRble)
library(sf)
library(rgdal)
library(curl)
library(osmdata)
library(raster)
library(tidyverse)
library(lubridate)
library(gdalUtils)
library(data.table)
library(cowplot)
library(terra)


# Cities in Ukraine with a population over 100,000
# Excluding Crimea, as it  was under Russian control
# before the invasion. Modified from:
# http://database.ukrcensus.gov.ua/PXWEB2007/ukr/publ_new1/2021/zb_chuselnist%202021.pdf
cities_pop <- 
  read_csv('data/cities_pop/cities_pop_ukr4.csv') %>% 
  dplyr::select(
    City, Population
  )

# Makes the data frame wide
cities_avg_ntl <- 
  cities_pop %>% 
  pivot_wider(
    names_from = City,
    values_from = Population
  )

# Adds 10 empty rows
for (i in 1:10) {
  cities_avg_ntl <- 
    cities_avg_ntl %>% 
    rbind(NA)
}

# Removes population row
cities_avg_ntl <- 
  slice(cities_avg_ntl, -c(1))

# Finds the date 50 days before the
# as a baseline
fifty_before_invasion <- 
  as_date(ymd(20220224)) - 50

cities_avg_ntl <- 
  cities_avg_ntl %>% 
  # Adds dates columns
  dplyr::mutate(
    end_date = 0
  )

# Fills end_date column
for (i in 1:nrow(cities_avg_ntl)) {
  
  cities_avg_ntl$end_date[i] <- 
    fifty_before_invasion + (i * 10)
}

# Adds and fills a start date column
cities_avg_ntl <- 
  cities_avg_ntl %>% 
  dplyr::mutate(
    start_date = 
      (end_date - 10)
  )

# Makes a new column from the dates
search_dates <- 
  cities_avg_ntl %>% 
  dplyr::select(
    start_date,
    end_date
  )

# Correction
search_dates$end_date <- 
  search_dates$end_date - 1

# Removes the dates column from the cities data set
cities_avg_ntl <- 
  cities_avg_ntl %>% 
  dplyr::select(
    -start_date,
    -end_date
  )

image_quality <- 
  cities_avg_ntl

# Finds the Julian date of a lubridate input
lubridate_to_julian <- function(input) {
  
  start_date <- 
    paste(
      year(as_date(input)),
      "-01-01",
      sep = ""
    )
  
  julian <- 
    as.double((as_date(input) - ymd(start_date)) + 1)
  
  if ((julian < 10) == TRUE) {
    julian <- 
      paste(
        "00", 
        julian, 
        sep = ""
      )
  } else if ((julian >= 10 && julian < 100) == TRUE) {
    julian <- 
      paste(
        "0", 
        julian,
        sep = ""
      )
  }
  
  # Using the format on page 7:
  # https://viirsland.gsfc.nasa.gov/PDF/BlackMarbleUserGuide_v1.2_20210421.pdf
  julian <- 
    paste(
      "A",
      year(as_date(input)),
      julian,
      sep = ""
    )
  
  julian <- 
    as.character(julian)
  
  return(julian)
}

search_dates <- 
  search_dates %>% 
  dplyr::mutate(
    
    start_date_julian = 
      "0",
    
    end_date_julian = 
      "0"
  )

# Runs the above function
for (i in 1:nrow(search_dates)) {
  
  search_dates$start_date_julian[i] <- 
    lubridate_to_julian(
      search_dates$start_date[i]
    )
  
  search_dates$end_date_julian[i] <- 
    lubridate_to_julian(
      search_dates$end_date[i]
    )
}

# Produces a new tibble with every date
# in the study time period. 
# (50 days before and after the invasion)
every_date <- 
  as_tibble(
    as_date(
      as_date(search_dates$start_date[1]):as_date(search_dates$end_date[10])
    )
  ) %>% 
  dplyr::mutate(
    julian =
      "0"
  )

for (i in 1:nrow(every_date)) {
  
  every_date$julian[i] <- 
    lubridate_to_julian(
      every_date$value[i]
    )
}

# Should have 100 rows,
# 1 for each day
nrow(every_date)

# produces another data frame for the 
# same dates in 2021. Used to compare
# 2022 data with the previous year
every_date_2021 <- 
  every_date

for (i in 1:nrow(every_date)) {
  
  every_date_2021$value[i] <- 
    every_date$value[i] - years(1)
  
  every_date_2021$julian[i] <- 
    gsub(
      "2022", "2021",
      every_date_2021$julian[i]
    )
  
}

search_dates_2021 <- 
  search_dates

for (i in 1:nrow(search_dates)) {
  
  search_dates_2021$start_date[i] <- 
    as_date(search_dates$start_date[i]) - years(1)
  
  search_dates_2021$end_date[i] <- 
    as_date(search_dates$end_date[i]) - years(1)
  
  search_dates_2021$start_date_julian[i] <- 
    gsub(
      "2022", "2021",
      search_dates_2021$start_date_julian[i]
    )
    
  search_dates_2021$end_date_julian[i] <- 
    gsub(
      "2022", "2021",
       search_dates_2021$end_date_julian[i]
    )
  
}


# Saves the produced data as CSV for later use
cities_avg_ntl %>%
  write.csv('data/time_series_ntl/time_series_ntl.csv', 
            row.names = FALSE)

image_quality %>%
  write.csv('data/time_series_ntl/time_series_image_quality.csv', 
            row.names = FALSE)

every_date %>%
  write.csv('data/time_series_ntl/every_date.csv', 
            row.names = FALSE)

every_date_2021 %>%
  write.csv('data/time_series_ntl/every_date_2021.csv', 
            row.names = FALSE)

search_dates %>%
  write.csv('data/time_series_ntl/search_dates.csv', 
            row.names = FALSE)

search_dates_2021 %>%
  write.csv('data/time_series_ntl/search_dates_2021.csv', 
            row.names = FALSE)