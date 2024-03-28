# Author: Saeesh Mangwani
# Date: 2024-03-09

# Description: Downloading Gwells data tables from the BC-data catalogue

# ==== libraries ====
library(bcdata)
library(dplyr)
library(sf)

# ==== Paths and global variables ====
out_dir <- 'data'

# ==== Downloading tables ====

# Groundwater wells ----------
# bcdc_get_record('groundwater-wells')
wells <- bcdc_query_geodata('e4731a85-ffca-4112-8caf-cb0a96905778') |> 
  collect()
st_write(wells, file.path(out_dir, 'wells.shp'), append=F)
wells |>
  # mutate(long_3005 = st_coordinates(wells)[,1]) |>
  # mutate(lat_3005 = st_coordinates(wells)[,2]) |>
  st_drop_geometry() |>
  readr::write_csv(file.path(out_dir, 'wells.csv'))

# Groundwater well lithology ----------
# bcdc_get_record('lithology-of-ground-water-wells')
litho <- bcdc_query_geodata('dd462162-f358-463d-ab25-9115fb30006b') |> 
  collect()
st_write(litho, file.path(out_dir, 'lithology.shp'))
litho |> 
  st_drop_geometry() |>
  readr::write_csv(file.path(out_dir, 'lithology.csv'))

# Groundwater aquifers ----------
# bcdc_get_record('ground-water-aquifers')
aquifers <- bcdc_query_geodata('099d69c5-1401-484d-9e19-c121ccb7977c') |> 
  collect()
aquifers |> 
  mutate(across(where(is.numeric), \(x){round(x, 5)})) |> 
  st_write(file.path(out_dir, 'aquifers.shp'), append=F)
aquifers |>
  # mutate(long_3005 = st_coordinates(aquifers)[,1]) |>
  # mutate(lat_3005 = st_coordinates(aquifers)[,2]) |>
  st_drop_geometry() |>
  readr::write_csv(file.path(out_dir, 'aquifers.csv'))
