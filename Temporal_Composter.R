# The aim of this script is to produce
# 10x 10 day averages of VNP46A2 data over
# Ukraine, in a tiff format, via temporal 
# composting. For both 2021 and 2022 data. 
# 50 days before and after the
# 24th February... The date of the Russian
# invasion of Ukraine.

# The "Set_up.R" file must be run first, and the data
# should be in the download files - 

#'data/Jan-April_Data' for the
# 3rd January - 17th April 2022

#'data/2021_input_data' for the
#'# 3rd January - 17th April 2021

# In the original H5 format from:
# https://ladsweb.modaps.eosdis.nasa.gov/search/order/2/VNP46A2--5000

# This file contains a considerable amount of 
# copy pasted code ran for 4 different outputs.
# The length of this file could be 
# reduced significantly. However, the code works,
# and shortening it likely wouldn't make it run
# quicker than it does currently.

# The first part of the file produces an output for
# 2022, the second part does 2021.
# The total run time is roughly ~ 3-4hrs

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

# Produces a tibble containing every input file. (2022)
file_list <- 
  as_tibble(
    list.files(
      # This folder contains every VNP46A2 file
      # for the 6 tiles over Ukraine from the
      # 3rd January to the 17th April 2022.
      # In the Original H5 file format from:
      # https://ladsweb.modaps.eosdis.nasa.gov/search/order/2/VNP46A2--5000
      'data/Jan-April_Data'
    )
  )

every_date <- 
  read_csv('data/time_series_ntl/every_date.csv')



# This loop combines VNP46A2 tiles from the
# same date into a single raster layer
# for the Quality flag layer of VNP46A2 data in 2022
for (date in 1:nrow(every_date)) {
  
  print(
    paste(
      "Date:",
      as_date(every_date$value[date])
    )
  )
  
  files_to_combine <- 
    file_list[file_list$value %like% every_date$julian[date],]
  
  for (file in 1:nrow(files_to_combine)) {
    
    dir <- "data/Jan-April_Data/"
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
        "_Data/\\s*(.*?)\\s*://"
      )
    
    raster_layer_name <- 
      paste(
        "img <- img$",
        # The following line was found from 
        # running img$ and copying the suggested
        # full layer name
        "HDF5.data.Jan.April_Data.",
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
    
    # This part only works for tiles over Ukraine
    # As the extent coordinates are put in manually
    if ((grepl("h20v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 50, 60)
    }
    
    if ((grepl("h20v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 40, 50)
    }
    
    if ((grepl("h21v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 50, 60)
    }
    
    if ((grepl("h21v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 40, 50)
    }
    
    if ((grepl("h22v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 50, 60)
    }
    
    if ((grepl("h22v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 40, 50)
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
    if (file == 6) {
      save_dir <- 
        paste(
          "data/final_2022_quality_daily/",
          "VNP46A2_Qual.",
          every_date$julian[date],
          ".Ukraine.tif",
          sep = ""
        )
      
      writeRaster(
        img_stack,
        save_dir
      )
    }
    
  }
  
}


# This loop combines VNP46A2 tiles from the
# same date into a single raster layer
# for the data layer of VNP46A2 data in 2022
for (date in 1:nrow(every_date)) {
  
  print(
    paste(
      "Date:",
      as_date(every_date$value[date])
    )
  )
  
  files_to_combine <- 
    file_list[file_list$value %like% every_date$julian[date],]
  
  for (file in 1:nrow(files_to_combine)) {
    
    dir <- "data/Jan-April_Data/"
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
            sds[1], function(x) raster(readGDAL(x))
          )
        )
      )
    
    # Selects the name of the file
    file_name <- 
      str_match(
        sds[1], 
        "_Data/\\s*(.*?)\\s*://"
      )
    
    raster_layer_name <- 
      paste(
        "img <- img$",
        # The following line was found from 
        # running img$ and copying the suggested
        # full layer name
        "HDF5.data.Jan.April_Data.",
        file_name[2],
        "...HDFEOS.GRIDS.VNP_Grid_DNB.Data_Fields.DNB_BRDF.Corrected_NTL",
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
    
    # This part only works for tiles over Ukraine
    # As the extent coordinates are put in manually
    if ((grepl("h20v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 50, 60)
    }
    
    if ((grepl("h20v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 40, 50)
    }
    
    if ((grepl("h21v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 50, 60)
    }
    
    if ((grepl("h21v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 40, 50)
    }
    
    if ((grepl("h22v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 50, 60)
    }
    
    if ((grepl("h22v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 40, 50)
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
    if (file == 6) {
      save_dir <- 
        paste(
          "data/final_2022_data_daily/",
          "VNP46A2.",
          every_date$julian[date],
          ".Ukraine.tif",
          sep = ""
        )
      
      writeRaster(
        img_stack,
        save_dir
      )
    }
    
  }
  
}

# Loads a csv from "Set_up.R"
search_dates <- 
  read_csv('data/time_series_ntl/search_dates.csv')
  

# Combines 10 days of VNP46A2 data into 1 tiff
# With low quality pixels removed.
# Every 10 days.
for (i in 1:nrow(search_dates)) {
  
  search_end <- 
    i * 10
  
  search_start <- 
    search_end - 9
  
  data_file_list <- list()
  qual_file_list <- list()
  
  # Finds the 20 files needed to combine
  # Makes them into lists
  for (date in search_start:search_end) {
    
    search_date <- 
      every_date$julian[date]
    
    data_file_dir <- 
      paste(
        "data/final_2022_data_daily/",
        "VNP46A2.",
        search_date,
        ".Ukraine.tif",
        sep = ""
      )
    
    data_file_list <- 
      append(
        data_file_list,
        data_file_dir
      )
    
    
    qual_file_dir <- 
      paste(
        "data/final_2022_quality_daily/",
        "VNP46A2_Qual.",
        search_date,
        ".Ukraine.tif",
        sep = ""
      )
    
    qual_file_list <- 
      append(
        qual_file_list,
        qual_file_dir
      )
    
  }
  
  # Stacks the data layer
  stacked_data <- 
    stack(data_file_list)
  
  # Stacks the Quality flag layer
  stacked_qual <- 
    stack(qual_file_list)
  
  # Sets low quality pixels to NA
  stacked_data[stacked_qual >= 2] = NA
  
  # Averages 10 days of data into 1
  stacked_data <- 
    calc(
      stacked_data,
      fun = mean, 
      na.rm = TRUE
    )
  
  plot(stacked_data)
  
  print(
    paste(
      i,
      "out of 10."
    )
  )
  
  save_file_dir <- 
    paste(
      "data/final_2022_10day_avg/",
      "VNP46A2_10days.",
      search_dates$start_date_julian[i], "-",
      search_dates$end_date_julian[i],
      ".Ukraine.tif",
      sep = ""
    )
  
  writeRaster(
    stacked_data,
    save_file_dir
  )
  
}



# 2022
# ---------------------------------------------------------------------------
# 2021




# Produces a tibble containing every input file. (2021)
file_list <- 
  as_tibble(
    list.files(
      # This folder contains every VNP46A2 file
      # for the 6 tiles over Ukraine from the
      # 3rd January to the 17th April 2021.
      # In the Original H5 file format from:
      # https://ladsweb.modaps.eosdis.nasa.gov/search/order/2/VNP46A2--5000
      'data/2021_input_data'
    )
  )

every_date_2021 <- 
  read_csv('data/time_series_ntl/every_date_2021.csv')

# This loop combines VNP46A2 tiles from the
# same date into a single raster layer
# for the Quality flag layer of VNP46A2 data in 2021
for (date in 1:nrow(every_date_2021)) {
  
  print(
    paste(
      "Date:",
      as_date(every_date_2021$value[date])
    )
  )
  
  files_to_combine <- 
    file_list[file_list$value %like% every_date_2021$julian[date],]
  
  for (file in 1:nrow(files_to_combine)) {
    
    dir <- "data/2021_input_data/"
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
        "_data/\\s*(.*?)\\s*://"
      )
    
    raster_layer_name <- 
      paste(
        "img <- img$",
        # The following line was found from 
        # running img$ and copying the suggested
        # full layer name
        "HDF5.data.2021_input_data.",
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
    
    # This part only works for tiles over Ukraine
    # As the extent coordinates are put in manually
    if ((grepl("h20v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 50, 60)
    }
    
    if ((grepl("h20v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 40, 50)
    }
    
    if ((grepl("h21v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 50, 60)
    }
    
    if ((grepl("h21v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 40, 50)
    }
    
    if ((grepl("h22v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 50, 60)
    }
    
    if ((grepl("h22v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 40, 50)
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
    if (file == 6) {
      save_dir <- 
        paste(
          "data/final_2021_quality_daily/",
          "VNP46A2_Qual.",
          every_date_2021$julian[date],
          ".Ukraine.tif",
          sep = ""
        )
      
      writeRaster(
        img_stack,
        save_dir
      )
    }
    
  }
  
}


# This loop combines VNP46A2 tiles from the
# same date into a single raster layer
# for the data layer of VNP46A2 data in 2021
for (date in 1:nrow(every_date_2021)) {
  
  print(
    paste(
      "Date:",
      as_date(every_date_2021$value[date])
    )
  )
  
  files_to_combine <- 
    file_list[file_list$value %like% every_date_2021$julian[date],]
  
  for (file in 1:nrow(files_to_combine)) {
    
    dir <- "data/2021_input_data/"
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
            sds[1], function(x) raster(readGDAL(x))
          )
        )
      )
    
    # Selects the name of the file
    file_name <- 
      str_match(
        sds[1], 
        "_data/\\s*(.*?)\\s*://"
      )
    
    raster_layer_name <- 
      paste(
        "img <- img$",
        # The following line was found from 
        # running img$ and copying the suggested
        # full layer name
        "HDF5.data.2021_input_data.",
        file_name[2],
        "...HDFEOS.GRIDS.VNP_Grid_DNB.Data_Fields.DNB_BRDF.Corrected_NTL",
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
    
    # This part only works for tiles over Ukraine
    # As the extent coordinates are put in manually
    if ((grepl("h20v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 50, 60)
    }
    
    if ((grepl("h20v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(20, 30, 40, 50)
    }
    
    if ((grepl("h21v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 50, 60)
    }
    
    if ((grepl("h21v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(30, 40, 40, 50)
    }
    
    if ((grepl("h22v03", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 50, 60)
    }
    
    if ((grepl("h22v04", files_to_combine$value[file])) == TRUE) {
      extent(img) <- 
        c(40, 50, 40, 50)
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
    if (file == 6) {
      save_dir <- 
        paste(
          "data/final_2021_data_daily/",
          "VNP46A2.",
          every_date_2021$julian[date],
          ".Ukraine.tif",
          sep = ""
        )
      
      writeRaster(
        img_stack,
        save_dir
      )
    }
    
  }
  
}

# Loads a csv from "Set_up.R"
search_dates_2021 <- 
  read_csv('data/time_series_ntl/search_dates_2021.csv')


# Combines 10 days of VNP46A2 data into 1 tiff
# With low quality pixels removed.
# Every 10 days.
for (i in 1:nrow(search_dates_2021)) {
  
  search_end <- 
    i * 10
  
  search_start <- 
    search_end - 9
  
  data_file_list <- list()
  qual_file_list <- list()
  
  # Finds the 20 files needed to combine
  # Makes them into lists
  for (date in search_start:search_end) {
    
    search_date <- 
      every_date_2021$julian[date]
    
    data_file_dir <- 
      paste(
        "data/final_2021_data_daily/",
        "VNP46A2.",
        search_date,
        ".Ukraine.tif",
        sep = ""
      )
    
    data_file_list <- 
      append(
        data_file_list,
        data_file_dir
      )
    
    
    qual_file_dir <- 
      paste(
        "data/final_2021_quality_daily/",
        "VNP46A2_Qual.",
        search_date,
        ".Ukraine.tif",
        sep = ""
      )
    
    qual_file_list <- 
      append(
        qual_file_list,
        qual_file_dir
      )
    
  }
  
  # Stacks the data layer
  stacked_data <- 
    stack(data_file_list)
  
  # Stacks the Quality flag layer
  stacked_qual <- 
    stack(qual_file_list)
  
  # Sets low quality pixels to NA
  stacked_data[stacked_qual >= 2] = NA
  
  # Averages 10 days of data into 1
  stacked_data <- 
    calc(
      stacked_data,
      fun = mean, 
      na.rm = TRUE
    )
  
  plot(stacked_data)
  
  print(
    paste(
      i,
      "out of 10."
    )
  )
  
  save_file_dir <- 
    paste(
      "data/final_2021_10day_avg/",
      "VNP46A2_10days.",
      search_dates_2021$start_date_julian[i], "-",
      search_dates_2021$end_date_julian[i],
      ".Ukraine.tif",
      sep = ""
    )
  
  writeRaster(
    stacked_data,
    save_file_dir
  )
  
}


# 2021
# ---------------------------------------------------------------------------
# 50 day average of before and after the invasion


file_list <- list.files('data/final_2022_10day_avg')
file_list <- as.list(file_list)
file_list <- file_list[!grepl(".aux.xml", file_list)]

tenday_file_list <- 
  list()

for (i in 1:length(file_list)) {
  
  tenday_file_list <- 
    append(
      tenday_file_list,
      paste(
        'data/final_2022_10day_avg/',
        file_list[[i]],
        sep = ''
      )
    )
  
}

# 50 days after the invasion
stacked_data <- 
  stack(tenday_file_list[1:5])

stacked_data <- 
  calc(
    stacked_data,
    fun = mean, 
    na.rm = TRUE
  )

ukraine_before <- stacked_data

save_file_dir <- 
  paste(
    "data/final_50_day_average/",
    "VNP46A2_50days.Before.Ukraine.tif",
    sep = ""
  )

writeRaster(
  stacked_data,
  save_file_dir
)

# 50 days before the invasion
stacked_data <- 
  stack(tenday_file_list[6:10])

stacked_data <- 
  calc(
    stacked_data,
    fun = mean, 
    na.rm = TRUE
  )

ukraine_after <- stacked_data

save_file_dir <- 
  paste(
    "data/final_50_day_average/",
    "VNP46A2_50days.After.Ukraine.tif",
    sep = ""
  )

writeRaster(
  stacked_data,
  save_file_dir
)




















