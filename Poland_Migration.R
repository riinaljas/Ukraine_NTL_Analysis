# The aim of this code is to review the effects that
# the Russian invasion of Ukraine had on the 
# Ukrainian-Polish border crossing points, using
# VNP46A2 data.

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

fifty_before_invasion <- 
  as_date(ymd(20220225)) - 50

hundred_after_invasion <- 
  as_date(ymd(20220225)) + 99

poland_border_ntl <- 
  tibble(
    city = NA,
    start_date = NA,
    end_date = NA,
    ntl_value = NA,
    cell_qual = NA
  )

poland_border_ntl$city <- 
  as.integer(poland_border_ntl$city)
poland_border_ntl$start_date <- 
  as.double(poland_border_ntl$start_date)
poland_border_ntl$end_date <- 
  as.double(poland_border_ntl$end_date)
poland_border_ntl$ntl_value <- 
  as.double(poland_border_ntl$ntl_value)
poland_border_ntl$cell_qual <- 
  as.double(poland_border_ntl$cell_qual)

every_date <- 
  as_tibble(
    as_date(
      as_date(fifty_before_invasion):as_date(hundred_after_invasion)
    )
  ) %>% 
  dplyr::mutate(
    julian =
      "0"
  )

# Fills the dates data frame
for (i in 1:nrow(every_date)) {
  
  every_date$julian[i] <- 
    lubridate_to_julian(
      every_date$value[i]
    )
}

# Lists data from the VNP46A2 h20v03 and h20v04
# tiles from the 06/01/2022 - 04/06/2022.
# In the original .h5 format from 
# https://ladsweb.modaps.eosdis.nasa.gov/search/order/2/VNP46A2--5000
file_list_poland <- 
  list.files(
    'data/poland_data/'
  )

file_list_poland <- 
  as_tibble(
    file_list_poland
  )

# Merges 2 tiles over eastern Poland into a single tiff
# and saves it. For the VNP46A2 data layer, non gap-filled.
for (date in 1:nrow(every_date)) {
  
  print(
    paste(
      "Date:",
      as_date(every_date$value[date])
    )
  )
  
  files_to_combine <- 
    file_list_poland[file_list_poland$value %like% every_date$julian[date],]
  
  for (file in 1:nrow(files_to_combine)) {
    
    dir <- "data/poland_data/"
    
    full_dir <- 
      paste(
        dir,
        files_to_combine$value[file],
        sep = ""
      )
    
    sds <- get_subdatasets(full_dir)
    
    img <- 
      brick(
        stack(
          sapply(
            sds[5], function(x) raster(readGDAL(x))
          )
        )
      )
    
    # Selects the name of the file
    file_name <- 
      str_match(
        sds[1], 
        "poland_data/\\s*(.*?)\\s*://"
      )
    
    raster_layer_name <- 
      paste(
        "img <- img$",
        # The following line was found from 
        # running img$ and copying the suggested
        # full layer name
        "HDF5.data.poland_data.",
        file_name[2],
        "...HDFEOS.GRIDS.VNP_Grid_DNB.Data_Fields.Mandatory_Quality_Flag",
        sep = ""
      )
    
    # Converts from a rasterbrick to layer
    eval(parse(
      text = 
        raster_layer_name
    ))
    
    # Adds WGS84 crs to the raster
    crs(img) <- 
      CRS('+init=EPSG:4326')
    
    # This part only works for tiles over this area
    # As the extent coordinates are put in manually
    if ((grepl("h20v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 50, 60)
    }
    
    if ((grepl("h20v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 40, 50)
    }
    
    # Stacks the data into 1 rasterstack
    if (file == 1) {
      img_stack <- img
    } else {
      img_stack <-
        mosaic(
          img_stack,
          img,
          fun = mean
        )
    }
    
    # Saves the tiff
    if (file == 2) {
      save_dir <- 
        paste(
          "data/poland_data_combined/",
          "VNP46A2.",
          every_date$julian[date],
          ".Poland.tif",
          sep = ""
        )
      
      writeRaster(
        img_stack,
        save_dir
      )
    }
    
  }
  
}

# 8 Shapefiles were made using R and the osm library,
# Using the locations named in:
# https://www.gov.pl/web/unitedkingdom/information-for-refugees-from-ukraine

# A long loop which keeps merging VNP46A2 data,
# Until there is an image with >90% pixels.
# In poland, for 150 days, 8 shapefiles

for (i in 1:8) {
  
  shapefile_name <- 
    paste(
      'data/Migration/',
      'poland_point_',
      i, 
      '/poland_point_',
      i, '.shp',
      sep = ""
    )
  
  border_shapefile <- 
    readOGR(
      shapefile_name
    )
  
  cell_qual <- 0
  stack_count <- 0
  
  for (day in 1:nrow(every_date)) {
    
    if (cell_qual < 90) {
      
      vnp_dir <- 
        paste(
          'data/poland_data_combined/VNP46A2.',
          every_date$julian[day],
          '.Poland.tif',
          sep = ""
        )
      
      qual_dir <- 
        paste(
          'data/poland_data_quality/VNP46A2.Quality.',
          every_date$julian[day],
          '.Poland.tif',
          sep = ""
        )
      
      # Loads NTL data
      vnp_raster <- 
        raster(vnp_dir)
      
      # Loads quality flag data
      qual_raster <- 
        raster(qual_dir)
      
      # Removes low quality pixels from data
      vnp_raster[qual_raster >= 2] = NA
      
      vnp_raster <- 
        crop(
          vnp_raster, 
          border_shapefile
        )
      
      vnp_raster <- 
        mask(
          vnp_raster, 
          border_shapefile
        )
      
      if (stack_count == 0) {
        
        start_date <- 
          every_date$value[day]
        
        vnp_stack <- vnp_raster
        
      } else {
        
        vnp_stack <- 
          stack(
            vnp_stack,
            vnp_raster
          )
        
        vnp_stack <- 
          calc(
            vnp_stack,
            fun = mean, 
            na.rm = TRUE
          )
        
      }
      
      plot(vnp_stack)
      plot(border_shapefile, add = TRUE)
      
      # Finds the number of NA pixels, to give a %
      # value of quality. 50% = 50% of the pixels 
      # across the city are 'NA'
      cell_inside_poly <- 
        setValues(
          vnp_stack,
          65535
        )
      
      cell_inside_poly <- 
        mask(
          cell_inside_poly,
          border_shapefile
        )
      
      cell_inside_poly <-
        freq(cell_inside_poly) %>%
        as_tibble()
      
      cell_qual <-
        freq(vnp_stack) %>%
        as_tibble() %>%
        drop_na()
      
      without_na <-
        sum(
          cell_qual$count
        )
      
      cell_qual <-
        ((without_na / cell_inside_poly$count[1]) * 100)
      
      print(cell_qual)
      stack_count <- 
        stack_count + 1
      
    } else {
      
      end_date <- 
        every_date$value[day]
      
      # Saves the tiff
      save_dir <- 
        paste(
          'data/poland_individual_tiffs/Shape',
          i, '_',
          start_date, '-', end_date,
          '.tif', sep = ""
        )
      
      writeRaster(
        vnp_stack,
        save_dir
      )
      
      ntl_value <- 
        cellStats(
          vnp_stack, 
          stat = 'mean', 
          na.rm = TRUE
        )
      
      poland_border_ntl[nrow(poland_border_ntl) + 1,] <- 
        list(
          i, 
          as.double(start_date), 
          as.double(end_date), 
          ntl_value, 
          cell_qual
        )
      
      cell_qual <- 0
      stack_count <- 0
      
    }
    
  }
  
}


# Producing the data
# ---------------------------------------------------------------------------
# Visualising the data


# Loads the shapefiles used
for (i in 1:8) {
  
  shapefile_name <- 
    paste(
      'data/Migration/',
      'poland_point_',
      i, 
      '/poland_point_',
      i, '.shp',
      sep = ""
    )
  
  to_parse <- 
    paste(
      'border_shapefile_', i, '<- 
    readOGR(
      shapefile_name
    )',
      sep = ""
    )
  
  eval(parse(text = to_parse))
  
}

# Loads the dataset
poland_values <- 
  read_csv(
    'data/time_series_ntl/poland_border.csv'
  )


poland_values <- 
  poland_values %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(
    mid_date = 0,
    city_name = 0
  )

for (i in 1:nrow(poland_values)) {
  
  # Finds the mean value between the start
  # and end search dates. 
  poland_values$mid_date[i] <- 
    round(
      median(
        poland_values$start_date[i]:
          poland_values$end_date[i]
      )
    )
  
  if (poland_values$city[i] == 1) {
    poland_values$city_name[i] <- 
      'Dorohusk-Jagodzin'
  }
  if (poland_values$city[i] == 2) {
    poland_values$city_name[i] <- 
      'Dołhobyczów-Uhrynów'
  }
  if (poland_values$city[i] == 3) {
    poland_values$city_name[i] <- 
      'Zosin-Uściług'
  }
  if (poland_values$city[i] == 4) {
    poland_values$city_name[i] <- 
      'Hrebenne-Rawa Ruska'
  }
  if (poland_values$city[i] == 5) {
    poland_values$city_name[i] <- 
      'Korczowa-Krakowiec'
  }
  if (poland_values$city[i] == 6) {
    poland_values$city_name[i] <- 
      'Medyka-Szeginie'
  }
  if (poland_values$city[i] == 7) {
    poland_values$city_name[i] <- 
      'Budomierz-Hruszew'
  }
  if (poland_values$city[i] == 8) {
    poland_values$city_name[i] <- 
      'Krościenko-Smolnica'
  }
  
}




for (i in 1:8) {
  
  graph_data <- 
    poland_values %>% 
    dplyr::filter(
      # Change the number for 1 to 8
      # border crossing areas.
      city == i
    )
  
  to_parse <- 
    paste(
      'plot_', i, ' <- ',
      'ggplot(
  data = graph_data,
  aes(
    x = as_date(mid_date),
    y = ntl_value
  )
) +
  geom_point() +
  geom_smooth(
  span = 0.6,
  level = 0.8
  ) +
  geom_vline(
    xintercept = 19047,
    colour = "black"
  )  + 
      theme_bw() +
      labs(
      title = graph_data$city_name[i], 
      x = "",
      y = ""
      )',
      sep = ""
    )
  
  eval(parse(text = to_parse))
  
}


plot_grid(
  plot_1, plot_2, plot_3, plot_4,
  plot_5, plot_6, plot_7, plot_8,
  
  labels = "", ncol = 2
)
















