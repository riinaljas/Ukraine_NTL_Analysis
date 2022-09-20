# The code tests the validity of using VNP46A2
# data for the analysis of the impact of the 
# Russian invasion of Ukraine in 1 city.
# Mariupol, South-East Ukraine.

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

area_unosat <- 
  read_sf(
    dsn = 'data/Unitar/UNOSAT_Mariupol_26March2022_RDA_shp/Mariupol_26March2022_RDA.shp'
  )

area_shp <- 
  area_unosat$geometry %>% 
  as_Spatial() %>% 
  raster::aggregate()



# A long loop which keeps merging VNP46A2 data,
# Until there is an image with 99% pixels.
# Change directories if using a different place
cell_qual <- 0
stack_count <- 0

for (day in 51:nrow(every_date)) {
  
  if (cell_qual < 99) {
    
    vnp_dir <- 
      paste(
        'data/Jan-April_Combined/VNP46A2.',
        every_date$julian[day],
        '.Ukraine.tif',
        sep = ""
      )
    
    qual_dir <- 
      paste(
        'data/Jan-April_Qual/VNP46A2_Qual.',
        every_date$julian[day],
        '.Ukraine.tif',
        sep = ""
      )
    
    # Loads NTL data
    vnp_raster <- 
      raster(vnp_dir)
    
    area_shp <- 
      spTransform(
        area_shp, 
        crs(vnp_raster)
      )
    
    # Loads quality flag data
    qual_raster <- 
      raster(qual_dir)
    
    # Removes low quality pixels from data
    vnp_raster[qual_raster >= 2] = NA
    
    vnp_raster <- 
      crop(
        vnp_raster, 
        area_shp
      )
    
    vnp_raster <- 
      mask(
        vnp_raster, 
        area_shp
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
    plot(area_shp, add = TRUE)
    
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
        area_shp
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
    print(day)
    
    stack_count <- 
      stack_count + 1
    
  } else {
    
    end_date <- 
      every_date$value[day]
    
    # Saves the tiff
    save_dir <- 
      paste(
        'data/mariupol_individual_tiffs/Mariupol_',
        start_date, '-', end_date,
        '.tif', sep = ""
      )
    
    writeRaster(
      vnp_stack,
      save_dir
    )
    
    cell_qual <- 0
    stack_count <- 0
    
  }
  
}

# Data from:
# https://www.unitar.org/maps/map/3517
# Made into a raster using Arcmaps
# Polygon to Raster tool.

damaged_buildings <- 
  raster('data/Unitar/3_apr_rast1.tif')

# Sets pixels without damaged buildings to 0
damaged_buildings[damaged_buildings == 6] = 0

# Sets pixels with damaged buildings to 1
damaged_buildings[damaged_buildings == 14] = 1


mariupol_before <- 
  raster(
    'data/mariupol_individual_tiffs/Mariupol_50_before.tif'
  )


mariupol_april <- 
  raster(
    'data/mariupol_individual_tiffs/Mariupol_2022-04-01-2022-04-09.tif'
  )


#   after - before
#   --------------   x 100
#       before

mariupol_perc_change <- 
  (((mariupol_april - mariupol_before) / mariupol_before) * 100)

# writeRaster(
#   mariupol_perc_change,
#   'data/mariupol_individual_tiffs/mariupol_perc_change.tif'
# )

area_shp <- 
  st_as_sf(area_shp)

area_shp <- 
  area_shp %>% 
  st_transform(
    proj4string(mariupol_before)
  )

plot_data <- 
  as.data.frame(
    mariupol_april, 
    xy = TRUE
  )


ggplot() +
  geom_raster(
    data = plot_data,
    aes(
      x = x,
      y = y,
      fill = Mariupol_2022.04.01.2022.04.09
    )
  ) +
  theme_map() +
  # Manually selected to best display the data
  scale_fill_gradientn(
    colours = c(
      "#080811",
      "#8f8879",
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
    name = "nWatts·cm−2·sr−1",
    na.value = "White",
    limits = c(0, 750)
  ) +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 4.5)
  ) +
  geom_sf(
    fill = 'transparent',
    data = area_shp$geometry,
    size = 0.1
  )

# Shows that the majority of the city of
# Mariupol experienced a 95% or higher drop
# in NTL.
large_decrease <- 
  mariupol_perc_change

large_decrease[large_decrease > -95] = NA

plot(large_decrease)

pixel_count <- 
  freq(large_decrease) %>% 
  as_tibble() %>% 
  drop_na()

# The number of
sum(pixel_count$count)

mariupol_perc_change <- 
  projectRaster(
    mariupol_perc_change,
    damaged_buildings
  )

mariupol_perc_change <- 
  resample(
    mariupol_perc_change,
    damaged_buildings,
    method = "bilinear"
  )

# Makes the 2 rasters exactly the same
mariupol_perc_change <- 
  mask(
    mariupol_perc_change,
    damaged_buildings
  )

damaged_buildings <- 
  mask(
    damaged_buildings,
    mariupol_perc_change
  )

# Removes InF and NaN values.
mariupol_perc_change[mariupol_perc_change == NaN] = NA
mariupol_perc_change[mariupol_perc_change == Inf] = NA

# Find the correlation between the two images
cor(values(damaged_buildings),
    values(mariupol_perc_change),
    use = "na.or.complete")


# Produced in Arcmap by taking 1000 random points,
# then collecting the data from 2 layers.
# Using the Extract Multi Values to Points tool
point_stats <- 
  read_sf(
    'data/Unitar/stats/points4.shp'
  ) %>% 
  select(-Id)

# Removes points outside of the city
point_stats <- 
  point_stats[!(as.character(point_stats$vnp_perc) == '-9999'),]

point_stats <- 
  point_stats[!(as.character(point_stats$dmg_bd) == '-9999'),]

# Sets pixels without damaged buildings to 0
point_stats$dmg_bd[point_stats$dmg_bd == 6] = 0
# Sets pixels with damaged buildings to 1
point_stats$dmg_bd[point_stats$dmg_bd == 14] = 1

# Changes to tibble
point_stats <- 
  as_tibble(point_stats) %>% 
  select(dmg_bd, vnp_perc)

plot(point_stats)

# Runs a stats test on the data
cor.test(point_stats$dmg_bd, point_stats$vnp_perc)

# Produces a boxplot and a summary of the data
boxplot(vnp_perc~dmg_bd, data = point_stats)

point_stats %>% 
  filter(dmg_bd == 1) %>% 
  summary()

point_stats %>% 
  filter(dmg_bd == 0) %>% 
  summary()


# ---------------------------------------------------------------------------
# Landsat as a base map

# Here an attempt was made to use Landsat as a base map,
# as shown by:
# https://earthobservatory.nasa.gov/images/150002/tracking-night-lights-in-ukraine
# However, the resolution of the NTL data is not high enough
# for it to have the same effect as shown by the link.


# Loads landsat data from earthexplorer
landsat <- 
  raster(
    'data/landsat/LC09_L2SP_176027_20220703_20220705_02_T1_SR_B5.tif'
  )

# Crops to Mariupol
area_shp <- 
  spTransform(
    area_shp, 
    crs(landsat)
  )

landsat <- 
  crop(
    landsat,
    area_shp
  )

landsat <- 
  mask(
    landsat,
    area_shp
  )

landsat <- 
  projectRaster(
    landsat,
    crs = crs(mariupol_before)
  )

plot_data <- 
  as.data.frame(
    landsat, 
    xy = TRUE
  ) %>% 
  drop_na()

plot_data_2 <- 
  as.data.frame(
    mariupol_before, 
    xy = TRUE
  ) %>% 
  drop_na()

ggplot() +
  geom_raster(
    data = plot_data,
    aes(
      x = x,
      y = y,
      fill = LC09_L2SP_176027_20220703_20220705_02_T1_SR_B5
    )
  ) + 
  scale_fill_gradient(
    low = '#ffffff',
    high = '#000000'
  )



# ---------------------------------------------------------------------------
# Plotting damaged building onto landsat data



landsat <- 
  projectRaster(
    landsat,
    crs = crs(area_shp$geometry)
  )

large_decrease <- 
  projectRaster(
    large_decrease,
    crs = crs(area_shp$geometry)
  )

plot_data <- 
  as.data.frame(
    landsat, 
    xy = TRUE
  ) %>% 
  drop_na()

plot_data_2 <- 
  as.data.frame(
    large_decrease, 
    xy = TRUE
  ) %>% 
  drop_na()

plot_data_2$layer[plot_data_2$layer < 0] = 0


ggplot() +
  geom_raster(
    data = plot_data,
    aes(
      x = x,
      y = y,
      fill = LC09_L2SP_176027_20220703_20220705_02_T1_SR_B5
    )
  ) + 
  scale_fill_gradient(
    low = '#ffffff',
    high = '#000000',
    na.value = "00FFFFFF"
  ) +
  geom_raster(
    data = plot_data_2,
    aes(
      x = x,
      y = y,
      fill = layer
    )
  ) +
  theme_map() +
  theme(
    legend.position = 'none'
  ) +
  geom_sf(
    fill = 'transparent',
    data = area_shp$geometry,
    size = 0.1
  )
















