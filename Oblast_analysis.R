# This file produces results from previously made
# 50day average date, on an Oblast scale.

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

# Loads two 50 day NTL averages. 
# Before and after the 2022 Russian invasion of Ukraine
ukraine_before <- raster('data/50_day_average/VNP46A2_50days.Before.Ukraine.tif')
ukraine_after <- raster('data/50_day_average/VNP46A2_50days.After.Ukraine.tif')

# Loads shapefiles of 27 administrative regions
# of Ukraine, from:
# https://geodata.lib.utexas.edu/catalog/stanford-gg870xt4706
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

for (before_after in 1:2) {
  
  if (before_after == 1) {
    ukraine_raster <- 
      ukraine_before
  } else {
    ukraine_raster <- 
      ukraine_after
  }
  
  for (i in 1:nrow(oblast_shp)) {
    
    # Selectes the ith shp
    bbx <- oblast_shp$geometry[i]
    bbx <- as_Spatial(bbx)
    
    # Crop and mask the raster
    bbx_raster <- 
      crop(
        ukraine_raster, 
        bbx
      )
    
    bbx_raster <- 
      mask(
        bbx_raster, 
        bbx
      )
    
    # Finds the locations of cells with values over x
    extreme_points <- 
      xyFromCell(
        bbx_raster, 
        which(bbx_raster[] > 4000)
      )
    
    extreme_points <- 
      as_tibble(extreme_points)
    
    if (nrow(extreme_points) > 0) {
      
      extreme_points <- 
        st_as_sf(
          extreme_points, 
          coords = c("x", "y"),
          crs = 4326
        )
      
      # Produces a 5km mask around extreme points
      for (x in 1:nrow(extreme_points)) {
        
        point_buffer <- 
          buffer(
            as(
              st_geometry(extreme_points$geometry[x]), 
              "Spatial"
            ), 
            width = 5000
          )
        
        bbx_raster <- 
          mask(
            bbx_raster, 
            point_buffer,
            inverse = TRUE
          )
        
      }
      
    }
    
    # Un-comment htis part to produce NLCRI by Oblast
    # avg_value <-
    #   cellStats(
    #     bbx_raster,
    #     stat = 'mean',
    #     na.rm = TRUE
    #   )
    # 
    # avg_value <- 
    #   round(avg_value)
    # 
    # bbx_raster <- 
    #   setValues(
    #     bbx_raster,
    #     avg_value
    #   )
    # 
    # bbx_raster <- 
    #   crop(
    #     bbx_raster, 
    #     bbx
    #   )
    # 
    # bbx_raster <- 
    #   mask(
    #     bbx_raster, 
    #     bbx
    #   )
    
    # Saves the raster
    to_run <- 
      paste(
        "oblast_", i, "_",
        before_after,
        " <- bbx_raster",
        sep = ""
      )
    
    eval(
      parse(
        text = to_run
      )
    )
    
  }
  
}

# Combines all of the rasters to one
before_mosaic <- 
  mosaic(
    oblast_1_1,
    oblast_2_1,
    oblast_3_1,
    oblast_4_1,
    oblast_5_1,
    oblast_6_1,
    oblast_7_1,
    oblast_8_1,
    oblast_9_1,
    oblast_10_1,
    oblast_11_1,
    oblast_12_1,
    oblast_13_1,
    oblast_14_1,
    oblast_15_1,
    oblast_16_1,
    oblast_17_1,
    oblast_18_1,
    oblast_19_1,
    oblast_20_1,
    oblast_21_1,
    oblast_22_1,
    oblast_23_1,
    oblast_24_1,
    oblast_25_1,
    oblast_26_1,
    oblast_27_1,
    fun = mean
  )


after_mosaic <- 
  mosaic(
    oblast_1_2,
    oblast_2_2,
    oblast_3_2,
    oblast_4_2,
    oblast_5_2,
    oblast_6_2,
    oblast_7_2,
    oblast_8_2,
    oblast_9_2,
    oblast_10_2,
    oblast_11_2,
    oblast_12_2,
    oblast_13_2,
    oblast_14_2,
    oblast_15_2,
    oblast_16_2,
    oblast_17_2,
    oblast_18_2,
    oblast_19_2,
    oblast_20_2,
    oblast_21_2,
    oblast_22_2,
    oblast_23_2,
    oblast_24_2,
    oblast_25_2,
    oblast_26_2,
    oblast_27_2,
    fun = mean
  )

# Limits the maximum value to 2000
before_mosaic[before_mosaic > 2000] = 2000
after_mosaic[after_mosaic > 2000] = 2000

bbx <- oblast_shp$geometry
bbx <- as_Spatial(bbx)

before_mosaic[is.na(before_mosaic[])] <- 0

# Crop and mask the raster
before_mosaic <- 
  crop(
    before_mosaic, 
    bbx
  )

before_mosaic <- 
  mask(
    before_mosaic, 
    bbx
  )

after_mosaic[is.na(after_mosaic[])] <- 0

# Crop and mask the raster
after_mosaic <- 
  crop(
    after_mosaic, 
    bbx
  )

after_mosaic <- 
  mask(
    after_mosaic, 
    bbx
  )



# Changes raster to data frame for ggplot
# Modify the 'before_mosaic' to
# 'after_mosaic' and vice versa
plot_data <- 
  as.data.frame(
    before_mosaic, 
    xy = TRUE
  ) %>% 
  drop_na()


ggplot() +
  geom_raster(
    data = plot_data,
    aes(
      x = x,
      y = y,
      fill = layer
    )
  ) +
  geom_sf(
    fill = 'transparent',
    data = oblast_shp$geometry,
    size = 0.1
  ) +
  theme_map() +
  # Manually selected to best display the data
  scale_fill_gradientn(
    colours = c(
      "#080811",
      "#d5bd8f",
      "#f4e6b9",
      "#f3f5f4",
      "#f3f5f4",
      "#f3f5f4",
      "#f3f5f4",
      "#FFFADB",
      "#FFFADB",
      "#FFFADB",
      "#FFFADB",
      "#FFF096",
      "#FFF096",
      "#FFF096",
      "#FFEB70",
      "#FFEB70",
      "#FFEB70",
      "#FFE545",
      "#FFE545",
      "#FFE545",
      "#FFDF18",
      "#FFDF18",
      "#FFDF18",
      "#FFCD0D",
      "#FFCA00",
      "#FFCA00"
    ),
    name = "nWatts·cm−2·sr−1"
  ) +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 4.5),
    legend.direction = "horizontal",
    legend.position = c(0.05, 0.35)
  )
# +
#   ggsave(
#     filename = 'images/Ukraine_Before.png',
#     dpi = 800
#   )


# Alternatively to save graphics to a pdf use:

# pdf("path", height = x, width = y, paper = "a4")

# *Make a graphic

# dev.off()


# Use - 
# scale_fill_gradientn(limits = c(x, y))
# to manually set the scales the same if required

# ---------------------------------------------------------------------------

# NLCRI as described in
# Ongoing Conflict Makes Yemen Dark: From the
# Perspective of Nighttime Light

oblast_nlcri <- round(
  (((after_mosaic - before_mosaic) / before_mosaic) * 100)
)

library(RColorBrewer)
mapview::mapview(oblast_nlcri, col.regions=brewer.pal(9, "YlGn"))

oblast_nlcri <- 
  as.data.frame(
    oblast_nlcri, 
    xy = TRUE
  )

# Plots the Oblast NLCRI map
ggplot() +
  geom_raster(
    data = oblast_nlcri,
    aes(
      x = x,
      y = y,
      fill = layer
    )
  ) +
  scale_fill_gradientn(
    colours = c(
      "#ff0000",
      "#ffffff"
    ),
    na.value = "white",
    name = 'NLCRI',
    guide = 
      guide_colourbar(
        frame.colour = "black"
      )
  ) +
  geom_sf(
    data = oblast_shp$geometry,
    size = 0.1,
    # Sets the shapefile transparent
    fill = alpha("Green", 0)
  ) +
  theme_map() +
  theme(
    legend.position = 'bottom'
  )

# Finds the locations of cells with values over x
extreme_points <- 
  xyFromCell(
    bbx_raster, 
    which(bbx_raster[] > 4000)
  )

extreme_points <- 
  as_tibble(extreme_points)

extreme_points <- 
  st_as_sf(
    extreme_points, 
    coords = c("x", "y"),
    crs = 4326
  )

for (i in 1:nrow(extreme_points)) {
  
  point_buffer <- 
    buffer(
      as(
        st_geometry(extreme_points$geometry[i]), 
        "Spatial"
      ), 
      width = 5000
    )
  
  bbx_raster <- 
    mask(
      bbx_raster, 
      point_buffer,
      inverse = TRUE
    )
  
}

mapview::mapview(bbx_raster)
