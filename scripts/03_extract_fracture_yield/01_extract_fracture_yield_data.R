# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03

# Description: Extracting and organizing fracture and yield information for the
# bedrock wells in the gwells lithology table

# ==== Loading libraries ====
library(dplyr)
library(readr)
library(stringr)
library(future)
library(furrr)
library(purrr)
source("scripts/03_extract_fracture_yield/_frac_yield_utils.R")

# ==== Paths and global variables ====

# Input lithology table (where material classes are already extracted)
litho_path <- 'output/lithology_matclass_cleaned.csv'

# Well table path - used for its reference comment information
well_path <- 'data/well.csv'

# Output directory
out_dir <- 'output'

# Number of cores for parellel processing
parallelly::availableCores()
numCores <- 4

# ==== Reading data ====

# Defining column types and names from the outset
litho_og <- read_csv(litho_path, col_types = "ddddcccc") %>% 
  mutate(well_type = NA_character_, general_remarks = NA_character_) %>% 
  dplyr::rename('record_index' = rid, 
                "depth_from" = from_depth, 
                "depth_to" = to_depth) |> 
  # Keeping only relevant columns
  dplyr::select(wtn, record_index, depth_from, depth_to, lithology, matclass)

# The relevant columns frmo the well dataset, which contains reference comment
# information that will be used for subsequent data extraction.
well <- read_csv(well_path) %>% dplyr::select("wtn" = well_tag_number, comments)

# ==== Tidying and formatting dataset for data extraction ====

# Adding general comment information about each well from the well table (where
# present)
litho_og <- litho_og |> 
  left_join(well, by='wtn') |> 
  rename('general_remarks' = comments)

# Splitting bedrock wells, as only for these is it relevant to run frac-yield
# extraction
litho <- litho_og %>% 
  filter(wtn %in% unique(pull(filter(litho_og, matclass == "bedrock"), wtn)))

# All other only overburden wells are treated separately
overburden <- litho_og |> 
  filter(!wtn %in% unique(litho$wtn))

# Joining the rows back and doing further cleaning
litho <- litho %>% 
  # Making copies of the lithology and comment rows so that we can manipulate
  # text without changing the original information
  mutate(lithology_copy = lithology) %>% 
  mutate(general_remarks_copy = general_remarks) %>% 
  # Turning all text to lower case
  mutate(lithology = tolower(lithology)) %>% 
  mutate(general_remarks = tolower(general_remarks)) %>% 
  # Removing unneccessary whitespace:
  mutate(lithology = str_squish(lithology)) %>% 
  mutate(general_remarks = str_squish(general_remarks)) %>% 
  # Removing plus signs
  mutate(lithology = str_remove_all(lithology, "\\+")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "\\+")) %>% 
  # Transforming all gal.pm and gal/min signs to gpm
  mutate(lithology = str_replace_all(lithology, "gal\\.PM|gal\\.pm|gal/min", "gpm")) %>% 
  mutate(general_remarks = str_replace_all(general_remarks, "gal\\.PM|gal\\.pm|gal/min", "gpm")) %>% 
  # Transforming all missing lithology and general remark values to empty
  # strings to prevent code breakage
  mutate(lithology = ifelse(is.na(lithology), "", lithology)) %>% 
  mutate(general_remarks = ifelse(is.na(general_remarks), "", general_remarks)) %>% 
  # Removing all dates as these confuse the regex parser. These will be added
  # back when the orginal lithology comments are copied back over
  mutate(lithology = str_remove_all(lithology, "((?:\\d{1,4})\\/)?(\\d{1,2}|jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\/(\\d{2,4})")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "(?:(?:\\d{1,4})\\/)?(?:\\d{1,2}|jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\/(?:\\d{2,4})")) %>% 
  # Ensuring no extra whitespace left over from this cleaning
  mutate(lithology = str_squish(lithology)) %>% 
  mutate(general_remarks = str_squish(general_remarks)) %>% 
  # Adding columns for storing fracture and yield information
  mutate(fracture_from = NA_character_) %>% 
  mutate(fracture_to = NA_character_) %>% 
  mutate(fracture_type = NA_character_) %>% 
  mutate(single_frac_yield = NA_character_) %>% 
  mutate(single_frac_yield2 = NA_character_) %>% 
  mutate(cum_frac_yield = NA_character_) %>% 
  mutate(unit = NA_character_) %>% 
  # Turning all columns to character for consistent parsing in the following
  # functions
  mutate(across(everything(), as.character))

# ==== Extracting Bedrock Lithology Data ====

# Splitting the table by well
litho_split <- litho %>% 
  mutate(wtn_grp=wtn) %>% 
  group_by(wtn_grp) %>%
  group_split()

# Checking the row count: If there are more than 10,000 rows being cleaned,
# using parallel processing.
if (nrow(litho) > 10000){
  # Setting the plan for parallel processing
  plan(multisession, workers=numCores)
  
  # Running the extraction functions
  out_table_temp <- furrr::future_map(litho_split, extract_litho_data)
  out_table_temp2 <- furrr::future_map(out_table_temp, extract_comment_data)
  
  # Resetting the plan
  plan(sequential)
}else{
  # Running the extraction functions
  out_table_temp <- map(litho_split, extract_litho_data)
  out_table_temp2 <- map(out_table_temp, extract_comment_data)
}

# Remove rows where extra rows for frac/yield data have been made but they
# contain duplicated information (per WTN) - example of this = WTN: 39483
out_table_temp3 <- map(out_table_temp2, \(df){
  # Separating newly introduced rows from existing rows
  new_rows <- df |> 
    filter(record_index == 999999999) |> 
    # Keeping only unique ones here too
    distinct(pick(c('fracture_from', 'fracture_to', 
                    'single_frac_yield', 'single_frac_yield2')), .keep_all = T)
  base_rows <- df |> 
    filter(record_index != 999999999)
  
  # Removing rows whose fracture/yield data already exists in the table, and
  # adding back in only the unique rows
  new_rows |> 
    anti_join(base_rows, by=c('fracture_from', 'fracture_to', 
                              'single_frac_yield', 'single_frac_yield2')) |> 
    bind_rows(base_rows) |> 
    arrange(depth_from, depth_to)
})

# Joining the split table back together and removing the grouping variable
out_table <- out_table_temp3 %>% 
  bind_rows() %>% 
  group_by(wtn_grp) %>% 
  arrange(as.numeric(wtn_grp), depth_from, depth_to, as.numeric(record_index)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(wtn_grp = NULL)

# ==== Tidying final tables ====

# Replacing the original lithology and general remarks columns
out_table <- out_table %>% 
  mutate(lithology = lithology_copy) %>% 
  mutate(lithology_copy = NULL) %>% 
  mutate(general_remarks = general_remarks_copy) %>% 
  mutate(general_remarks_copy = NULL)

# Where the classified cumulative yield values are NA, using yield values from
# the original table if present
out_table <- out_table |> 
  left_join(
    litho_og |> 
      mutate(wtn=as.character(wtn)) %>% 
      select(wtn, depth_from, depth_to, 
             'wb_est' = water_bearing_estimated_flow,
             'wb_unit' = well_yield_unit_code),
    by=c('wtn', 'depth_from', 'depth_to')
  ) |> 
  # Joining any cumulative yield estimates from the original table back
  mutate(cum_frac_yield = ifelse(is.na(cum_frac_yield), wb_est, cum_frac_yield)) |> 
  mutate(unit = ifelse(is.na(unit), wb_unit, unit)) |> 
  select(-wb_est, -wb_unit)

# Joining back the overburden wells with their matclass classifications
out_table <- out_table |> 
  bind_rows(overburden) |> 
  arrange(wtn, depth_from, depth_to)

# ==== Saving data to disk and deleting objects from memory ====
write_csv(out_table, file.path(out_dir, "lithology_frac_yield_extracted.csv"))
write_csv(error_wells, file.path(out_dir, "litho_error_record_table.csv"))
# rm(list=ls())




