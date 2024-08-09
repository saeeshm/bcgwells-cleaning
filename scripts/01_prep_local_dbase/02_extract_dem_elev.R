# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2024-04-23

# Description: Extracting Elevation information for each well based on a
# reference DEM?

# ==== Libraries ====
library(terra)
library(sf)
library(readr)
library(dplyr)
library(purrr)

# ===== Paths and global variables =====

# Path to reference DEM
dem_path <- "Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/BC_DEM/BCDEM.tif"

# Path to wells dataset (local)
well_path <- 'data/well.csv'

# output directory where the updated well dataset will be saved
out_dir <- 'output'

# ===== Reading data =====
well_og <- read_csv(well_path)
dem_og <- rast(dem_path)
names(dem_og) <- 'ground_elevation_dem'

# ===== Spatializing and extracting =====

# Spatialzing well data
well <- vect(well_og, geom=c('longitude_Decdeg', 'latitude_Decdeg'), crs='epsg:4326') |> 
  terra::project('epsg:3005')

# Extracting elevation - THIS TAKES TIME
well_elev <- terra::extract(dem_og, well, method='simple', bind=T)

# Creating table with only elevations
etab <- well_elev |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  select(well_tag_number, ground_elevation_dem)

# Joining back to well table
well_og <- well_og |> left_join(etab)

# ===== Exporting =====
write_csv(well_og, file.path(out_dir, 'gwells_with_dem_elev.csv'))
write_csv(etab, file.path(out_dir, 'wtn_elev_key.csv'))

