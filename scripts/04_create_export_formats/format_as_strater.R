# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-10-22

# Description: Formatting the cleaned lithology and matclass tables into strater consistent format. The table used as a template for this conversion
# is located at: Z:\GWSI server Master Share Entry\GWSI Library and Resources\DATABASES\GWS_WELL_LOGS\MASTER_GWS_WellLog_Overburden2.xlsx

# ==== Loading libraries ====
library(tidyverse)
library(openxlsx)
source("strater_pre_process.R")

# ==== Building formatted tables ====

# Lithology table --------
litho <- litho %>% 
  rename("Hole id" = wtn) %>%
  mutate("From" = from_depth*0.3048) %>% 
  mutate("To" = to_depth*0.3048) %>% 
  mutate("Lithology keyword" = matclass) %>% 
  mutate("indent percentage" = NA_integer_) %>% 
  rename("Lithology description" = lithology) %>% 
  rename("From ft" = from_depth) %>% 
  rename("To ft" = to_depth) %>% 
  mutate("Simplified lithology" = matclass) %>% 
  select(`Hole id`, From, To, `Lithology keyword`, `indent percentage`, 
         `Lithology description`, `From ft`, `To ft`, `Simplified lithology`)

print("lithology table formatted")

# The bedrock table --------
bedrock <- bedrock %>% 
  filter(!is.na(fracture_from)) %>% 
  rename("Hole id" = wtn) %>%
  mutate("From" = fracture_from*0.3048) %>% 
  mutate("To" = fracture_to*0.3048) %>% 
  mutate("Fracture keyword" = NA_character_) %>% 
  mutate("Fracture yield" = case_when(!is.na(single_frac_yield2) ~ as.numeric(single_frac_yield2),
                                         !is.na(cum_frac_yield) ~ as.numeric(cum_frac_yield),
                                         TRUE ~ NA_real_)) %>% 
  mutate("Fracture description" = ifelse(!is.na(`Fracture yield`), "Water Producing", NA_character_)) %>% 
  rename("From ft" = fracture_from) %>% 
  rename("To ft" = fracture_to) %>% 
  mutate("Proj. No." = NA_character_) %>% 
  mutate("Project Name" = NA_character_) %>% 
  select(`Hole id`, From, To, `Fracture keyword`, `Fracture yield`, 
         `Fracture description`, `From ft`, `To ft`, `Proj. No.`, `Project Name`)

print("bedrock table formatted")

# The collar table --------
collar <- tibble("Hole Id" = well$well_tag_number)
collar$`UTM Zone` <- well$utm_zone_code
collar$Easting <- well$utm_easting
collar$Northing <- well$utm_northing
collar$Elevation <- well$ground_elevation
collar$`Starting Depth` <- 0
collar$`Ending Depth` <- well$finished_well_depth*0.3048
collar$`Starting Depth (ft)` <- 0
collar$`Ending Depth (ft)` <- well$finished_well_depth
collar$`Drilling Contractor Depth` <- well$company_of_person_responsible
collar$Driller <- well$driller_name
collar$`Drilling Method` <- collar %>%
  select("well_tag_number" = `Hole Id`) %>% 
  left_join(drilling) %>% 
  pull(drilling_method_code)
collar$`Stick up (TOC-m)` <- NA_real_
collar$`Well ID Plate Number` <- well$identification_plate_number
collar$`Well Tag Number` <- well$well_tag_number
collar$`Well Diameter` <- well %>% 
  mutate(diameter = paste(diameter,"inch -",round(diameter*25.4),"mm")) %>% 
  mutate(diameter = ifelse(str_detect(diameter, "NA"), NA_character_, diameter)) %>% 
  pull(diameter)
collar$`Date Commenced` <- well$construction_start_date
collar$`Date Completed` <- well$construction_end_date
collar$`Total Depth` <- well$total_depth_drilled*0.3048
collar$`Total Depth (ft)` <- well$total_depth_drilled
collar$`Depth to bedrock` <- well$bedrock_depth
collar$`Logged by` <- "BC gov"
collar$`Location` <- well %>% 
  mutate(location = ifelse(is.na(city), street_address, city)) %>% 
  pull(location)
collar$`Client` <- NA_character_
collar$`Consulting company` <- well$consultant_company
collar$`Proj No.` <- NA_character_
collar$`Project Name` <- NA_character_

print("collar table formatted")
  
# the well construction table  --------

# Calling the helper function to iterate over all well tag numbers and generate a strater formatted table
construction <- map_dfr(unique(well$well_tag_number), format_construction_strater)

# adding project columns
construction <- construction %>% 
  mutate("Proj. No." = NA_character_) %>% 
  mutate("Project Name" = NA_character_)

print("construction table formatted")

# Writing the water level table --------
water_level <- tibble("Hole Id" = well$well_tag_number)
water_level$Date <- well$construction_end_date
water_level$Depth <- well$static_water_level*0.3048
water_level$Description <- well$comments
water_level$Depth_FT <- well$static_water_level
water_level$comments <- NA_character_

print("water level table formatted")

# ==== Writing the outputted data as excel ====

wb <- createWorkbook()
addWorksheet(wb, "Lithology")
addWorksheet(wb, "Fractured BR")
addWorksheet(wb, "Collars")
addWorksheet(wb, "Well Construction")
addWorksheet(wb, "water level")

writeData(wb, "Lithology", litho, startRow = 1, startCol = 1, colNames = T)
writeData(wb, "Fractured BR", bedrock, startRow = 1, startCol = 1, colNames = T)
writeData(wb, "Collars", collar, startRow = 1, startCol = 1, colNames = T)
writeData(wb, "Well Construction", construction, startRow = 1, startCol = 1, colNames = T)
writeData(wb, "water level", water_level, startRow = 1, startCol = 1, colNames = T)

saveWorkbook(wb, file = "Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/format_as_strater/strater_formatted.xlsx", overwrite = TRUE)

print("workbook saved")


