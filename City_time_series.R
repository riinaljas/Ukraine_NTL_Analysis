# This script produces time series of multiple
# cities in Ukraine, using the previously
# generated 10x 10day averaged NTL data.

library(blackmaRble)
library(sf)
library(rgdal)
library(rgeos)
library(curl)
library(osmdata)
library(raster)
library(tidyverse)
library(lubridate)
library(gdalUtils)
library(data.table)
library(cowplot)
library(terra)

# Loads the empty results table from "Set_up.R"
cities_avg_ntl <- 
  read_csv('data/time_series_ntl/time_series_ntl.csv')

image_quality <- 
  read_csv('data/time_series_ntl/time_series_image_quality.csv')

# Cities in Ukraine with a population over 100,000
# Excluding Crimea, as it  was under Russian control
# before the invasion. Modified from:
# http://database.ukrcensus.gov.ua/PXWEB2007/ukr/publ_new1/2021/zb_chuselnist%202021.pdf
# Official Ukrainian source accessed in May however it is
# currently unavailable (25/08/2022). This source estimated the 2021
# population from the 2001 census.
cities_pop <- 
  read_csv('data/cities_pop/cities_pop_ukr4.csv')

# Loads a shapefile of Ukranian settlements from:
# http://download.geofabrik.de/europe/ukraine.html
city_shps <- 
  st_read('data/city_boundary/ukraine-latest-free.shp/gis_osm_places_a_free_1.shp')

# Selects only cities with a population of 100k or more
city_shps <- 
  cities_pop %>% 
  left_join(
    city_shps,
    by = c('Ukrainian_name' = 'name')
  )

# Removes towns and villages with the same name
city_shps <- 
  city_shps %>% 
  dplyr::filter(
    fclass == 'city'
  )

# Removes unwanted columns
city_shps <- 
  city_shps %>% 
  dplyr::select(
    City, Ukrainian_name, Population, geometry
  )

# Loads a csv from "Set_up.R"
search_dates <- 
  read_csv('data/time_series_ntl/search_dates.csv')


# Required Data
# ---------------------------------------------------------------------------
# Calculating mean NTL for each city


for (i in 1:10) {
  
  for (city in 1:ncol(cities_avg_ntl)) {
    
    print(colnames(cities_avg_ntl[city]))
    print(as_date(search_dates$start_date[i]))
    
    file_dir <- 
      paste(
        "data/final_2022_10day_avg/",
        "VNP46A2_10days.",
        search_dates$start_date_julian[i], "-",
        search_dates$end_date_julian[i],
        ".Ukraine.tif",
        sep = ""
      )
    
    vnp_raster <- 
      raster(file_dir)
    
    city_bb <- 
      city_shps$geometry[city] %>% 
      st_as_sf()
    
    vnp_raster <- 
      crop(
        vnp_raster, 
        city_bb
      )
    
    vnp_raster <- 
      mask(
        vnp_raster, 
        city_bb
      )
    
    # Removes extreme pixel values from data
    vnp_raster[vnp_raster >= 4000] = NA
    
    plot(vnp_raster)
    plot(city_bb, add = TRUE)
    
    # Saves the raster as a tiff
    save_file_dir <- 
      paste(
        "data/2022_10day_cities/",
        "VNP46A2.",
        colnames(cities_avg_ntl[city]), ".",
        search_dates$start_date_julian[i], "-",
        search_dates$end_date_julian[i],
        ".tif",
        sep = ""
      )
    
    writeRaster(
      vnp_raster,
      save_file_dir
    )
    
    
    # Finds the number of NA pixels, to give a %
    # value of quality. 75% = 75% of the pixels 
    # across the city are good quality, 25%
    # of the pixels are 'NA'
    cell_inside_poly <- 
      setValues(
        vnp_raster,
        65535
      )
    
    cell_inside_poly <- 
      mask(
        cell_inside_poly,
        city_bb
      )
    
    cell_inside_poly <-
      freq(cell_inside_poly) %>%
      as_tibble()
    
    cell_qual <-
      freq(vnp_raster) %>%
      as_tibble()
    
    with_na <-
      sum(
        cell_qual$count
      )
    
    cell_qual <-
      cell_qual %>%
      drop_na()
    
    without_na <-
      sum(
        cell_qual$count
      )
    
    image_quality[i, city] <-
      ((without_na / cell_inside_poly$count[1]) * 100)
    
    # Calculates the mean value of all of the pixels
    vnp_raster <- 
      cellStats(
        vnp_raster, 
        stat = 'mean', 
        na.rm = TRUE
      )
    
    cities_avg_ntl[i, city] <- 
      vnp_raster
    
  }
  
  cities_avg_ntl %>% 
    write.csv(
      'data/time_series_ntl/time_series_ntl_2022.csv', 
      row.names = FALSE
    )
  
  image_quality %>%
    write.csv(
      'data/time_series_ntl/time_series_image_quality_2022.csv', 
      row.names = FALSE
    )
  
}


# 2022
# ---------------------------------------------------------------------------
# 2021


# Loads the empty results table from "Set_up.R"
cities_avg_ntl_2021 <- 
  read_csv('data/time_series_ntl/time_series_ntl.csv')

image_quality_2021 <- 
  read_csv('data/time_series_ntl/time_series_image_quality.csv')

search_dates_2021 <- 
  read_csv('data/time_series_ntl/search_dates_2021.csv')

for (i in 1:10) {
  
  for (city in 1:ncol(cities_avg_ntl_2021)) {
    
    print(colnames(cities_avg_ntl_2021[city]))
    print(as_date(search_dates_2021$start_date[i]))
    
    file_dir <- 
      paste(
        "data/final_2021_10day_avg/",
        "VNP46A2_10days.",
        search_dates_2021$start_date_julian[i], "-",
        search_dates_2021$end_date_julian[i],
        ".Ukraine.tif",
        sep = ""
      )
    
    vnp_raster <- 
      raster(file_dir)
    
    city_bb <- 
      city_shps$geometry[city] %>% 
      st_as_sf()
    
    vnp_raster <- 
      crop(
        vnp_raster, 
        city_bb
      )
    
    vnp_raster <- 
      mask(
        vnp_raster, 
        city_bb
      )
    
    # Removes extreme pixel values from data
    vnp_raster[vnp_raster >= 4000] = NA
    
    plot(vnp_raster)
    plot(city_bb, add = TRUE)
    
    # Saves the raster as a tiff
    save_file_dir <- 
      paste(
        "data/2021_10day_cities/",
        "VNP46A2.",
        colnames(cities_avg_ntl[city]), ".",
        search_dates_2021$start_date_julian[i], "-",
        search_dates_2021$end_date_julian[i],
        ".tif",
        sep = ""
      )
    
    writeRaster(
      vnp_raster,
      save_file_dir
    )
    
    
    # Finds the number of NA pixels, to give a %
    # value of quality. 75% = 75% of the pixels 
    # across the city are good quality, 25%
    # of the pixels are 'NA'
    cell_inside_poly <- 
      setValues(
        vnp_raster,
        65535
      )
    
    cell_inside_poly <- 
      mask(
        cell_inside_poly,
        city_bb
      )
    
    cell_inside_poly <-
      freq(cell_inside_poly) %>%
      as_tibble()
    
    cell_qual <-
      freq(vnp_raster) %>%
      as_tibble()
    
    with_na <-
      sum(
        cell_qual$count
      )
    
    cell_qual <-
      cell_qual %>%
      drop_na()
    
    without_na <-
      sum(
        cell_qual$count
      )
    
    image_quality_2021[i, city] <-
      ((without_na / cell_inside_poly$count[1]) * 100)
    
    # Calculates the mean value of all of the pixels
    vnp_raster <- 
      cellStats(
        vnp_raster, 
        stat = 'mean', 
        na.rm = TRUE
      )
    
    cities_avg_ntl_2021[i, city] <- 
      vnp_raster
    
  }
  
  cities_avg_ntl_2021 %>% 
    write.csv(
      'data/time_series_ntl/time_series_ntl_2021.csv', 
      row.names = FALSE
    )
  
  image_quality_2021 %>%
    write.csv(
      'data/time_series_ntl/time_series_image_quality_2021.csv', 
      row.names = FALSE
    )
  
}


# Code for viewing any of the produced rasters
read_file_dir <- 
  paste(
    # Select the year
    "data/2021_10day_cities/",
    "VNP46A2.",
    # City number
    colnames(cities_avg_ntl[5]), ".",
    # Date number, same for both
    search_dates_2021$start_date_julian[4], "-",
    search_dates_2021$end_date_julian[4],
    ".tif",
    sep = ""
  )

city_raster <- 
  raster(read_file_dir)

mapview::mapview(city_raster)



# Calculating mean NTL for each city
# ---------------------------------------------------------------------------
# Producing Graphs from the data
# 2022

# Loads a csv from "Set_up.R"
search_dates <- 
  read_csv('data/time_series_ntl/search_dates.csv')

cities_avg_ntl <- 
  read_csv('data/time_series_ntl/time_series_ntl_2022.csv')

image_quality <- 
  read_csv('data/time_series_ntl/time_series_image_quality_2022.csv')

cities_avg_ntl_2021 <- 
  read_csv('data/time_series_ntl/time_series_ntl_2021.csv')

image_quality_2021 <- 
  read_csv('data/time_series_ntl/time_series_image_quality_2021.csv')

# Removes any data when the area had less than
# 80% pixels. 2022.
for (x in 1:ncol(cities_avg_ntl)) {
  
  for (y in 1:nrow(cities_avg_ntl)) {
    
    if ((image_quality[y, x] < 80) == TRUE) {
      
      cities_avg_ntl[y, x] <- NA
      
    }
    
  }
  
}

# Removes any data when the area had less than
# 80% pixels. 2021.
for (x in 1:ncol(cities_avg_ntl_2021)) {
  
  for (y in 1:nrow(cities_avg_ntl_2021)) {
    
    if ((image_quality_2021[y, x] < 80) == TRUE) {
      
      cities_avg_ntl_2021[y, x] <- NA
      
    }
    
  }
  
}

#2022
cities_avg_ntl <- 
  cities_avg_ntl %>% 
  select(
    -Makiivka, -Poltava, -Sumy, -Rivne, 
    -Kramatorsk, -Sloviansk, -Berdiansk, 
    -Sievierodonetsk, -Pavlohrad, -Uzhhorod
  )

cities_avg_ntl$ID <- 
  seq.int(nrow(cities_avg_ntl))

search_dates$ID <- 
  seq.int(nrow(search_dates))

graph_data <- 
  cities_avg_ntl %>% 
  full_join(
    search_dates,
    by = "ID"
  )

graph_data$Donetsk[3] <- NA

cities_avg_ntl <- 
  cities_avg_ntl %>% 
  select(-ID)

# 2021
cities_avg_ntl_2021 <- 
  cities_avg_ntl_2021 %>% 
  select(
    -Makiivka, -Poltava, -Sumy, -Rivne, 
    -Kramatorsk, -Sloviansk, -Berdiansk, 
    -Sievierodonetsk, -Pavlohrad, -Uzhhorod
  )

cities_avg_ntl_2021$ID <- 
  seq.int(nrow(cities_avg_ntl_2021))

graph_data_2021 <- 
  cities_avg_ntl_2021 %>% 
  full_join(
    search_dates,
    by = "ID"
  )

cities_avg_ntl_2021 <- 
  cities_avg_ntl_2021 %>% 
  select(-ID)


# Produces plots as variables that are joined later
for (i in 1:ncol(cities_avg_ntl)) {
  
  colour_1 <- "red"
  colour_2 <- "blue"
  
  col <- 
    paste(
      '`',
      colnames(cities_avg_ntl[i]),
      '`',
      sep = ""
    )
  
  to_parse <- 
    paste(
      'plot_', i, ' <- ',
      'ggplot() +
  geom_smooth(
    data = graph_data,
    aes_string(
      x = "as_date(start_date)",
      y = col
    ),
    se = FALSE,
    colour = colour_1,
    span = 0.6
  ) +
  geom_smooth(
    data = graph_data_2021,
    aes_string(
      x = "as_date(start_date)",
      y = col
    ),
    se = FALSE,
    colour = colour_2,
    span = 0.6
  ) +
      geom_point(
      data = graph_data,
    aes_string(
      x = "as_date(start_date + 3.5)",
      y = col
    ),
    colour = colour_1
      ) +
      geom_point(
      data = graph_data_2021,
    aes_string(
      x = "as_date(start_date + 3.5)",
      y = col
    ),
    colour = colour_2
      ) +
      theme_bw() +
      theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 10)
      ) +
      labs(
      title = colnames(graph_data[i])
      ) +
      geom_vline(
        xintercept = as_date(graph_data$start_date[6])
      )',
      sep = ""
    )
  
  eval(parse(text = to_parse))
  
}


# Adds all the produced plots onto one
plot_grid(
  plot_1, plot_2, plot_3, plot_4,
  plot_5, plot_6, plot_7, plot_8,
  plot_9, plot_10, plot_11, plot_12,
  plot_13, plot_14, plot_15, plot_16,
  plot_17, plot_18, plot_19, plot_20,
  plot_21, plot_22, plot_23, plot_24,
  plot_25, plot_26, plot_27, plot_28,
  plot_29, plot_30, plot_31, plot_32,
  plot_33, plot_34,
  
  labels = "", ncol = 7
)


# ---------------------------------------------------------------------------
# Making an NLCRI plot


# Made in excel by taking the average of 100 days in 2021
# and 50 days in 2022 as the 'before' and 50 days after
# the invasion as the 'after' in this equation -

#          after - before
# NLCRI = ---------------- x 100
#             before


nlcri <- 
  read_csv(
    'data/time_series_ntl/NLCRI_csv.csv'
  )

cities_pop <- 
  read_csv('data/cities_pop/cities_pop_ukr4.csv')

nlcri <- 
  nlcri[1, ]

nlcri <- 
  nlcri %>% 
  pivot_longer(
    cols = c(1:35),
    names_to = 'city',
    values_to = 'nlcri'
  )

plot_data <- 
  nlcri %>% 
  left_join(
    cities_pop,
    by = c('city' = 'City')
  ) %>% 
  dplyr::select(-Ukrainian_name)

plot_data$nlcri <- 
  as.double(plot_data$nlcri)

plot_data <- 
  plot_data %>% 
  dplyr::mutate(
    pos = nlcri >= 0
  )

ggplot(
  data = plot_data,
  aes(
    x = reorder(city, nlcri),
    y = nlcri,
    fill = pos
  )
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "#ff0000",
      "#0099ff"
    ), 
    guide = "none"
  ) +
  ylim(-100, 50) +
  coord_flip() +
  theme_bw() +
  labs(
    x = "City",
    y = "NLCRI"
  )


# To save graphics to a pdf use:

# pdf("path", height = x, width = y, paper = "a4")

# *Make a graphic

# dev.off()



# ---------------------------------------------------------------------------



oblast_shp <- 
  read_sf(
    dsn = 'data/city_boundary/Ukraine_divisions/UKR_adm1.shp'
  )

# Only required columns
oblast_shp <- 
  oblast_shp %>% 
  dplyr::select(
    NAME_1, geometry
  )

city_points <- 
  city_shps %>% 
  dplyr::filter(
    City %in% plot_data$city
  )

# Makes polygons into points
# using the cetnroid
city_points$geometry <- 
  st_centroid(
    city_points$geometry
  )

# Adds the NLCRI column
to_join <- 
  plot_data %>% 
  dplyr::select(
    city, nlcri
  )

city_points <- 
  city_points %>% 
  left_join(
    to_join,
    by = c('City' = 'city')
  )

# Changes the geometry to seperate latlon
city_points$geometry <- 
  do.call(
    rbind, st_geometry(city_points$geometry)
  ) %>% 
  as_tibble() %>% 
  setNames(
    c("lon","lat")
  )

city_points$lon <- 
  city_points$geometry$lon
city_points$lat <- 
  city_points$geometry$lat

city_points <- 
  city_points %>% 
  dplyr::select(
    -geometry
  )


ggplot() +
  geom_sf(
    fill = 'grey95',
    data = oblast_shp$geometry,
    size = 0.1
  ) +
  geom_point(
    data = city_points,
    aes(
      x = lon,
      y = lat,
      size = Population,
      fill = nlcri
    ),
    pch = 21,
    colour = "black"
  ) +
  scale_fill_gradientn(
    colours= c(
      "#000000",
      "#330000",
      "#660000",
      "#990000",
      "#cc0000",
      "#ff0000",
      "#ff3333",
      "#ff6666",
      "#ff9999",
      "#ffffff",
      "#ccebff",
      "#99d6ff",
      "#66c2ff",
      "#33adff",
      "#0099ff"
    ),
    limits = c(-100, 50),
    name = 'NLCRI'
  ) +
  scale_size_continuous(
    range = c(5, 15)
  ) +
  theme_map()
  



















