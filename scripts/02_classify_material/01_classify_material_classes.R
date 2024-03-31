# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-22

# Description: A scripts that formats the dowloaded lithology table to extract
# matclass values from it (this is the cleaned R implementation) of the
# OpenRefine process that was previously being used, a report of which is found
# in the master_openrefine_process.json document. This script has been developed
# through closely parsing and replicating that process and then updating it to
# fix errors later detected

# ==== Loading libraries ====
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(future)
library(furrr)
library(purrr)
source('scripts/02_classify_material/_matclass_utils.R')

# ==== Paths and global variables ====

# Well data home directory
gwells_dir <- 'data'

# Output directory where the matclass cleaned table will be stored
out_dir <- 'output'

# Temporary directory (used to store intermediate objects from this cleaning
# process)
tempdir <- 'temp'
dir.create(tempdir)

# Number of parellel processing works (CHECK HOW MANY YOU HAVE BEFORE SELECTING
# ONE)
parallelly::availableCores()
numCores <- 4

# ==== Reading Data ====

# Ensuring all columns are read as characters (prevents distortions due to read/write)
litho_og <- read_csv(file.path(gwells_dir, "lithology.csv"), col_types = 'c')
well <- read_csv(file.path(gwells_dir, "well.csv"))
casing <- read_csv(file.path(gwells_dir, "casing.csv"))
screen <- read_csv(file.path(gwells_dir, "screen.csv"))

# ==== Preliminary tidying and dataset structuring ====
litho <- litho_og |> 
  # Renaming columns
  rename("wtn" = well_tag_number,
         "from_depth" = lithology_from,
         "to_depth" = lithology_to) |> 
  # Correctly specifying the type for numeric columns
  mutate(across(c("from_depth", "to_depth", "water_bearing_estimated_flow"), as.numeric)) |> 
  # Basic text cleaning on character columns
  mutate(across(where(is.character), \(x) str_squish(tolower(x)))) |>
  # Turning all "0-nothing entered" values in the 3 relevant columns to NA
  mutate(across(
    c("lithology_description_code", 
      "lithology_colour_code", 
      "lithology_material_code"), 
    \(x){
      ifelse(x == "0 nothing entered", NA_character_, x)
    })) |> 
  # Creating a "codes" column that concatenates the information from all the
  # descriptive lithology columns
  tidyr::unite("codes", 
               c("lithology_description_code", "lithology_material_code", 
                 "lithology_hardness_code", "lithology_colour_code", 
                 "lithology_observation"), 
               sep = " ", na.rm = T, remove = F)

# Joining all lithology information into 1 column and removing redundancies.
# Since we're using furrr for parallel iteration, we need to set the plan to
# allow for this first
plan(multisession, workers = numCores)

# For each row, checking to see if any phrases/terms put into the codes column
# already exist within the lithology raw-data column and if so, removing them.
# This is to remove redundancy among the codes. The words that remain are passed
# back into the codes column
litho <- litho |> 
  future_pmap_dfr(function(wtn, from_depth, to_depth, lithology_raw_data, codes, ...){
    a <- str_split(codes, " ")[[1]]
    b <- str_split(lithology_raw_data, "(\\s|,\\s?|\\.\\s?)")[[1]]
    # Removing from the codes column any words already present in lithology-raw
    codes <- paste(a[!(a %in% b)], collapse = " ")
    return(tibble(wtn, from_depth, to_depth, lithology_raw_data, codes, ...))
  })

# Resetting the plan
plan(sequential)

# Saving state at this stage to not have to run that lengthy function again
litho |> write_csv(file.path(tempdir, "temp_lithology_codes_cleaned.csv"))

# Joining codes to create a consolidated lithology description column
litho <- litho  |>  
  # joining the codes and raw data columns into 1
  tidyr::unite("lithology", c("lithology_raw_data", "codes"), sep = "; ", 
               na.rm = T, remove = T) |> 
  # Removing trailing punctuation and whitespace
  mutate(lithology = str_remove(str_squish(str_trim(lithology)), ";$")) |> 
  # Removing random quotes that get created (but leaving informative ones, that
  # appear after numbers usually to indicate inches)
  mutate(lithology = str_remove_all(lithology, '(?<!\\d\\s?)"\\s*'))

# Checking for interval errors by creating an interval-error column that stores
# the location of these issues
litho <- litho |> 
  mutate(interval_error = case_when(
    # Both are missing
    is.na(from_depth) | is.na(to_depth) ~ "Blanks",
    # Both are 0
    from_depth == 0 & to_depth == 0 ~ "0-depths",
    # To is less than from (OOS = Out-of-sequence)
    to_depth - from_depth <= 0 ~ "OOS",
    TRUE ~ NA_character_)
  , .after='lithology')

# Saving state at this stage to not have to run that lengthy function again
litho |> write_csv(file.path(tempdir, "temp_lithology_codes_joined.csv"))

# ==== Preparing individual well tables for material class extraction ====

# Getting a list of individual tables per well
litho_split <- litho |> 
  mutate(wtn_grp = as.numeric(wtn)) |> 
  group_by(wtn_grp) |> 
  group_split()

# Testing cleaning functions
# test <- litho_split
litho |> filter(!litho$wtn %in% well$well_tag_number)
# test <- which(map(lt4, nrow) == 0)
df <- litho |> filter(wtn == 17991)
# df <- lt3[[101974]]
df <- lt5[[22059]]

df |>
  clean_0_depth_rows() |>
  clean_blanks() |>
  clean_oos_rows() |>
  # remove_uninformative_rows() |>
  clean_error_depths() |>
  invert_mismatched_rows() |>
  join_dup_ranges() |>
  check_end_depths(well, well_depth_column = 'finished_well_depth_ft-bgl')

# Applying a suite of tidying steps to prepare each well table for matclass
# cleaning

# Setting the plan for parallel processing
plan(multisession, workers = numCores)

print("1. Cleaning 0-depth rows...")
lt1 <- furrr::future_map(litho_split, clean_0_depth_rows)
# Using linear processing for this one as some software bug in the furrr package
# seems to prevent it from completing when using furrr
print("2. Cleaning blank depth information...")
lt2 <- purrr::map(lt1, clean_blanks)
print("3. Cleaning out-of-sequence depths...")
lt3 <- furrr::future_map(lt2, clean_oos_rows)
# print("4. Removing un-informative rows...")
# lt4 <- furrr::future_map(lt3, remove_uninformative_rows)
print("4. Cleaning poorly assigned depths...")
lt4 <- furrr::future_map(lt3, clean_error_depths)
print("5. Inverting mismatched from/to depths...")
lt5 <- furrr::future_map(lt4, invert_mismatched_rows)
print("6. Joining rows cover duplicated depth ranges...")
lt6 <- furrr::future_map(lt5, join_dup_ranges)
print("6. Checking for appropriate end depths...")
litho_tables_cleaned <- furrr::future_map(lt6, check_end_depths, 
                                well, 
                                well_depth_column = 'finished_well_depth_ft-bgl')
# Resetting plan
plan(sequential)
print("Cleaning complete!")
# rm(list=paste0('lt', 1:6))
# gc()

# ==== Material class cleaning ====

# First pass ----------

# Checking queries and columns in case there are breakings
check_matclass_queries(well, casing, screen)

# Setting the plan for parallel processing
plan(multisession, workers = numCores)

# Using the cleaned lithology tables, calling the first matclass cleaning and
# function which does its best to extract the appropriate matclass for each well
# by relying on any ancilliary information available in the well, casing and
# screen tables
matclass_firstpass_tables <- furrr::future_map(
  litho_tables_cleaned, 
  classify_matclass_ancilinfo, 
  well, casing, screen
)
# Resetting plan
plan(sequential)

# Joining the split table back into a single dataframe
litho_firstpass_cleaned <- matclass_firstpass_tables |> 
  bind_rows() |> 
  group_by(wtn_grp) |> 
  arrange(wtn, from_depth, to_depth) |> 
  distinct() |> 
  ungroup() |> 
  mutate(wtn_grp = NULL)

# Saving this temporary state
litho_firstpass_cleaned |> 
  write_csv(file.path(tempdir, "temp_litho_firstpass_cleaned.csv"))

# Second pass ----------

# This pass uses regular expression parsing to classify the matclass for the
# rest of the column

# Calling the regex parsing function to classify the matclass columns using
# information from the lithology columns
litho_cleaned <- classify_matclass_regex(litho_firstpass_cleaned)

# Organizing the cleaned dataset in an exportable format
output_table <- litho_cleaned |> 
  mutate(rid = 1:n()) |> 
  select(rid, wtn, from_depth, to_depth, lithology, matclass, 
         water_bearing_estimated_flow, well_yield_unit_code)

# Saving output to file
write_csv(output_table, file.path(out_dir, "lithology_matclass_cleaned.csv"))
