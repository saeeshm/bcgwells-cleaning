# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2024-04-23

# Description: Creating a leapfrom formatted output for the wells table

# ==== Libraries ====
library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(DBI)
library(RPostgres)
library(purrr)
library(openxlsx)

# ===== Paths and global variables =====

# Input paths --------

# Bedrock lithology cleaned
litho_path <- 'output/lithology_frac_yield_extracted.csv'
# Well table with elevation extracted
well_path <- 'output/gwells_with_dem_elev.csv'
# Original lithology
# litho_og_path <- 'data/lithology.csv'
drilling_path <- 'data/drilling_method.csv'
screen_path <- 'data/screen.csv'
casing_path <- 'data/casing.csv'
liner_path <- 'data/perforation.csv'

# Output paths --------

# Path to where output formatted excel sheet will be stored (for strater)
excel_out_path <- 'output/gwells_leapfrog_formatted.xlsx'

# Postgres credentials path
creds_path <- 'options/credentials.json'

# Output postgres schema
schema <- 'bcgwells_leapfrog'

# ===== Reading data =====
litho <- read_csv(litho_path)
well <- read_csv(well_path)
screen <- read_csv(screen_path)
casing <- read_csv(casing_path)
liner <- read_csv(liner_path)
drilling <- read_csv(drilling_path)

# ===== Preparing tables =====

# Collar table --------

# Getting BC Albers coordinates
coords <- well |> 
  filter(!is.na(longitude_Decdeg)) |> 
  st_as_sf(coords = c('longitude_Decdeg', 'latitude_Decdeg'), crs=4326) |> 
  st_transform(crs=3005) %>% 
  mutate(
    `East (X) AlB` = st_coordinates(.)[,1],
    `North (Y) ALB` = st_coordinates(.)[,2],
  ) |> 
  st_drop_geometry() |> 
  dplyr::select(well_tag_number, `East (X) AlB`, `North (Y) ALB`)

# Preparing table
collar <- well |> 
  left_join(coords) |> 
  dplyr::select(
    'Hole ID' = well_tag_number, 
    identification_plate_number, 
    `East (X) AlB`,
    `North (Y) ALB`,
    utm_northing, utm_easting, utm_zone_code,
    'Ground_elevation_masl_DEM' = ground_elevation_dem,
    'Ground_elevation_masl_Reported' = `ground_elevation_ft-asl`,
    'mdepth1' = `total_depth_drilled_ft-bgl`,
    'mdepth2' = `finished_well_depth_ft-bgl`
  ) |> 
  # Empty field for surveyed elevation
  mutate('Ground_elevation_masl_Surveyed' = NA_real_) |> 
  # Getting max depth from lithology if it doesn't exist here
  mutate(`Max Depth (m)` = ifelse(is.na(mdepth1), mdepth2, mdepth1)) |> 
  select(-contains('mdepth'))

# Survey table --------

# # Fractures table --------
fractures <- litho |> 
  mutate( `Fracture yield_usgpm` = ifelse(is.na(single_frac_yield2), 
                                          single_frac_yield, 
                                          single_frac_yield2)) |> 
  mutate(`Fracture keyword` = NA_character_) |> 
  dplyr::select(
    'Hole ID' = wtn,
    'from (m)' = fracture_from,
    'to (m)' = fracture_to,
    `Fracture keyword`,
    `Fracture yield_usgpm`,
    'Fracture description'=fracture_type,
    'from (ft)' = fracture_from,
    'to (ft)' = fracture_to,
  ) |> 
  mutate(`from (m)` = `from (m)`*0.3048) |> 
  mutate(`to (m)` = `to (m)`*0.3048) |> 
  filter(!is.na(`from (m)`))

# Lithology table --------
lithology <- litho |> 
  mutate(`Overburden/Bedrock` = case_when(
    is.na(matclass) | (matclass=='unknown') ~ NA_character_,
    matclass=='bedrock' ~ 'bedrock',
    T ~ 'overburden'
  )) |> 
  dplyr::select(
    'Hole ID' = wtn,
    'from (m)' = depth_from,
    'to (m)' = depth_to,
    'lithology_raw_data' = lithology,
    'lithology_clean'=matclass,
    `Overburden/Bedrock`,
    'from (ft)' = depth_from,
    'to (ft)' = depth_to,
  ) |> 
  mutate(`from (m)` = `from (m)`*0.3048) |> 
  mutate(`to (m)` = `to (m)`*0.3048)
  
# Screen table --------
screen_lf <- screen |> 
  # Elaborating codes
  mutate(screen_assembly_code_long = case_when(
    screen_assembly_type_code == "SCREEN" ~ "Screen",
    screen_assembly_type_code == "K_RISER" ~ "K-packer/riser",
    screen_assembly_type_code == "K_PACKER" ~ "K-packer",
    screen_assembly_type_code == "LEAD" ~ "Lead",
    screen_assembly_type_code == "RISER_PIPE" ~ "K-packer/riser",
    screen_assembly_type_code == "SCRN_BLANK" ~ "Screen",
    screen_assembly_type_code == "TAIL_PIPE" ~ "Screen Bottom",
    screen_assembly_type_code == "OTHER" ~ "Unspecified screen",
    TRUE ~ tolower(screen_assembly_type_code)
  )) |> 
  # Joining description information from the well table
  left_join(
    (well |> dplyr::select(well_tag_number, screen_information))
  ) |> 
  mutate(`Inner Diameter (inch)` = NA_real_) |> 
  dplyr::select(
    'Hole ID' = well_tag_number,
    'from (m)' = `screen_from_ft-bgl`,
    'to (m)' = `screen_to_ft-bgl`,
    'Outer Diameter (inch)' = screen_diameter_inches,
    `Inner Diameter (inch)`,
    'Construction Keyword' = screen_assembly_code_long,
    'Item description' = screen_information,
    'from (ft)' = `screen_from_ft-bgl`,
    'to (ft)' = `screen_to_ft-bgl`
  ) |> 
  mutate(`from (m)` = `from (m)`*0.3048) |> 
  mutate(`to (m)` = `to (m)`*0.3048)
  
# Water level table --------
water_level <- well |> 
  mutate(temp = NA_character_) |> 
  mutate(construction_end_date = as.character(ymd(construction_end_date))) |> 
  dplyr::select(
    'Hole ID' = well_tag_number,
    'Date of measurement' = construction_end_date,
    'Depth (mbg)' = `static_water_level_ft-btoc`,
    'Depth (ft)' = `static_water_level_ft-btoc`,
    'Description' = comments,
    'comments' = temp,
  ) |> 
  mutate(`Depth (mbg)` = `Depth (mbg)`*0.3048)

# ===== Exporting =====

# Creating a named list of output tables for quick export
otabs <- list(
  'Collar' = collar,
  # 'Survey' = survey,
  "Lithology" = lithology,
  "Fractures" = fractures,
  "Screen" = screen_lf,
  "Water level" = water_level
)

# Exporting to postgres --------

# Reading credentials
creds <- rjson::fromJSON(file = creds_path)

# Opening database connection
conn <- dbConnect(
  RPostgres::Postgres(), 
  host = creds$host, 
  dbname = creds$dbname,
  user = creds$user, 
  password = creds$password
)

# Exporting tables to the appropriate schema
dbExecute(conn, paste('create schema if not exists', schema))
# Creating tables
iwalk(otabs, \(x, tabname){
  print(tabname)
  dbWriteTable(conn, 
               DBI::Id(schema = schema, table = tabname), 
               x,
               append = F,
               overwrite=T)
})
dbDisconnect(conn)

# Exporting to excel --------
wb <- createWorkbook()
iwalk(otabs, \(x, tabname){
  print(tabname)
  addWorksheet(wb, tabname)
  writeData(wb, tabname, x, startRow = 1, startCol = 1, colNames = T)
})

saveWorkbook(wb, file = excel_out_path, overwrite = TRUE)
print("workbook saved")






  
  