# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-10-26

# Description: A script that conducs preparatory manipulations on the gwells
# tables and initializes helper functions to assist in the creation of a strater
# table

# ==== Loading libraries ====
library(tidyverse)

# ==== Reading Data ====
litho <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/output/lithology_matclass_cleaned.csv")
bedrock <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/output/bedrock_table-frac_yield_extracted.csv")
well <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/data/well.csv")
drilling <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/data/drilling_method.csv")
screen <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/data/screen.csv")
casing <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/data/casing.csv")
liner <- read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/data/perforation.csv")
# The reference table containing the well tag numbers that we're interested in formatting
wtns <- tryCatch(read_csv("Z:/GWSI server Master Share Entry/GWSI Library and Resources/DATABASES/WELLS Database/gwells/gwells_tidying/format_as_strater/wtn_input.csv", col_names = F), 
                 error = function(e){
                   return(tibble("X1" = numeric(0)))
                 })

# ==== Table preprocessing ====
# Filtering each table with the well tag numbers inputted and applying some preprocessing steps where required

# lithology table
litho <- litho %>% 
  filter(ifelse(nrow(wtns) > 0, list(wtn %in% wtns$X1), list(wtn %in% litho$wtn))[[1]])

# Bedrock table
bedrock <- bedrock %>% 
  filter(ifelse(nrow(wtns) > 0, list(wtn %in% wtns$X1), list(wtn %in% litho$wtn))[[1]])

# Well table
well <- well %>% 
  # Filtering
  filter(ifelse(nrow(wtns) > 0, list(well_tag_number %in% wtns$X1), list(well_tag_number %in% litho$wtn))[[1]]) %>% 
  # Creating a column that contains expanded descriptions for material code
  mutate("surface_seal_mat_code_long" = case_when(
    surface_seal_material_code == "BN_BACKFILL" ~ "bentonite backfill",
    surface_seal_material_code == "BN_CEM_MIX" ~ "bentonite cement mix",
    surface_seal_material_code == "BN_DRL_CUT" ~ "bentonite unclear",
    surface_seal_material_code == "BN_SD_WTMX" ~ "bentonite sand water mix",
    surface_seal_material_code == "BN_STL_CSG" ~ "bentonite steel",
    surface_seal_material_code == "BN_WT_MIX" ~ "bentonite water mix",
    surface_seal_material_code == "BNTITE_CLY" ~ "bentonite clay",
    surface_seal_material_code == "CON_GRT" ~ "unclear",
    surface_seal_material_code == "GRT_STLCSG" ~ "unclear",
    surface_seal_material_code == "NT_CMT_BMX" ~ "unclear",
    surface_seal_material_code == "NT_CMT_GRT" ~ "unclear",
    surface_seal_material_code == "SND_CMT_GT" ~ "unclear",
    surface_seal_material_code == "OTHER" ~ "unspecified material type",
    TRUE ~ tolower(surface_seal_material_code)
  )) %>% 
  # Creating a description column that combines information for various columns to describe the surface seal
  mutate("surface_seal_desc" = tolower(paste(surface_seal_mat_code_long, surface_seal_method_code))) %>% 
  # Doing the same process for filter pack data
  mutate("filter_pack_mat_code_long" = case_when(
    filter_pack_material_code == "FINE_GRAVL" ~ "fine gravel",
    filter_pack_material_code == "V_FI_GRAVL" ~ "very fine gravel",
    filter_pack_material_code == "V_COU_SAND" ~ "very course sand",
    filter_pack_material_code == "V_COU_SAND" ~ "very course sand",
    filter_pack_material_code == "OTHER" ~ "unspecified material type",
    TRUE ~ tolower(filter_pack_material_code)
  )) %>% 
  # Also renaming the material size codes as these are also relevant to add to the description.
  mutate("filter_pack_mat_size_code_long" = case_when(
    filter_pack_material_size_code == "1020_SAND" ~ "10-20 sand",
    filter_pack_material_size_code == "2_4_mm" ~ "2-4 mm",
    filter_pack_material_size_code == "PEA_GRAVEL" ~ "pea gravel",
    filter_pack_material_size_code == "OTHER" ~ "unspecified material size",
    TRUE ~ tolower(filter_pack_material_size_code)
  )) %>% 
  # Description
  mutate("filter_pack_desc" = tolower(paste(filter_pack_mat_code_long, filter_pack_mat_size_code_long)))

# Casing table
casing <- casing %>% 
  # Filtering
  filter(ifelse(nrow(wtns) > 0, list(well_tag_number %in% wtns$X1), list(well_tag_number %in% litho$wtn))[[1]]) %>% 
  # Elaborating codes
  mutate("casing_mat_code_long" = case_when(
    casing_material_code == "STL_PUL_OT" ~ "Steel",
    casing_material_code == "CEMENT" ~ "Cement",
    casing_material_code == "OPEN_HOLE" ~ "Open hole",
    casing_material_code == "PLASTIC" ~ "Plastic",
    casing_material_code == "STEEL" ~ "Steel",
    casing_material_code == "OTHER" ~ "Unspecified",
    TRUE ~ tolower(casing_material_code)
  )) %>% 
  # Creating a description
  mutate("desc" = paste(diameter, "in dia.", casing_mat_code_long, "casing.", "Drive shoe", drive_shoe_code))

# Screen table
screen <- screen %>% 
  # Filtering
  filter(ifelse(nrow(wtns) > 0, list(well_tag_number %in% wtns$X1), list(well_tag_number %in% litho$wtn))[[1]]) %>% 
  # Elaborating codes
  mutate("screen_assembly_code_long" = case_when(
    screen_assembly_type_code == "SCREEN" ~ "Well Screen",
    screen_assembly_type_code == "K_RISER" ~ "K-packer/riser",
    screen_assembly_type_code == "K_PACKER" ~ "K-packer",
    screen_assembly_type_code == "LEAD" ~ "Lead",
    screen_assembly_type_code == "RISER_PIPE" ~ "K-packer/riser",
    screen_assembly_type_code == "SCRN_BLANK" ~ "Well Screen",
    screen_assembly_type_code == "TAIL_PIPE" ~ "Screen Bottom",
    screen_assembly_type_code == "OTHER" ~ "Unspecified screen",
    TRUE ~ tolower(screen_assembly_type_code)
  )) %>% 
  # Creating a description
  mutate("desc" = paste(screen_diameter, "in dia.", "screen type:", screen_assembly_code_long, "slot size:", slot_size, "mm")) %>% 
  # converting the slot size to mm based on the rule given by Antonio
  mutate(slot_size = if_else(slot_size <= 1, slot_size*25.4, (slot_size/1000)*25.4))

# Drilling table
drilling <- drilling %>% 
  # Filtering
  filter(ifelse(nrow(wtns) > 0, list(well_tag_number %in% wtns$X1), list(well_tag_number %in% litho$wtn))[[1]]) %>% 
  # Grouping so as to remove duplicates by combining all descriptions associated with a single wtn into one row
  group_by(well_tag_number) %>% 
  # Combining descriptions
  summarise(drilling_method_code = paste(drilling_method_code, collapse = ";")) %>% 
  # ungrouping. Data are now deduplicated.
  ungroup()

# ==== Intializing helper functions ====

# A function that takes a well tag number and gathers the surface seal, filter, casing, screen and liner information associated with that well from
# all the relevant tables. It returns a dataframe containing all of these data formatted in strater format.
format_construction_strater <- function(wtn){
  out_row <- tibble("Hole Id" = numeric(0),
                    "From" = numeric(0),
                    "To" = numeric(0),
                    "Outer Diameter" = numeric(0),
                    "Inner Diameter" = numeric(0),
                    "Offset" = numeric(0),
                    "Construction Keyword" = character(0),
                    "Item" = character(0),
                    "from (ft)" = numeric(0),
                    "to (ft)" = numeric(0))
  
  # Organizing surface seal data
  well_data <- well %>% filter(wtn == well_tag_number)
  if(!is.na(well_data$surface_seal_material_code)){
    out_row <- tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = 0,
      "To" = if_else(is.na(well_data$surface_seal_depth), well_data$surface_seal_length*0.3048, well_data$surface_seal_depth*0.3048),
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = "Surface Seal",
      "Item" = well_data$surface_seal_desc,
      "from (ft)" = 0,
      "to (ft)" = if_else(is.na(well_data$surface_seal_depth), well_data$surface_seal_length, well_data$surface_seal_depth)) %>% 
      bind_rows(out_row)
  }
  
  # Organizing backfill data
  if(!is.na(well_data$backfill_type)){
    out_row <- tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = 0,
      "To" = well_data$backfill_depth*0.3048,
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = "Backfill",
      "Item" = tolower(well_data$backfill_type),
      "from (ft)" = 0,
      "to (ft)" = well_data$backfill_depth) %>% 
      bind_rows(out_row)
  }
  
  # Organizing filter data
  if(!is.na(well_data$filter_pack_material_code)){
    out_row <- tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = well_data$filter_pack_from*0.3048,
      "To" = well_data$filter_pack_to*0.3048,
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = case_when(
        str_detect(well_data$filter_pack_material_code,"GRAVL") ~ "Gravel Pack",
        str_detect(well_data$filter_pack_material_code,"SAND") ~ "Sorted Sand",
        TRUE ~ "Other Filter Pack"
      ),
      "Item" = well_data$filter_pack_desc,
      "from (ft)" = well_data$filter_pack_from,
      "to (ft)" = well_data$filter_pack_to) %>% 
      bind_rows(out_row)
  }
  
  # Organizing casing data
  casing_data <- casing %>% filter(wtn == well_tag_number)
  if(nrow(casing_data) > 0){
    out_row <- casing_data %>% 
      mutate("Hole Id" = well_tag_number,
             "From" = casing_from*0.3048,
             "To" = casing_to*0.3048,
             "Outer Diameter" = diameter,
             "Inner Diameter" = NA_real_,
             "Offset" = 0,
             "Construction Keyword" = case_when(
               casing_code == "SURFACE" ~ "Surface Casing",
               casing_code == "OPEN" | casing_material_code == "OPEN_HOLE" ~ "Open Hole",
               TRUE ~ "Casing"),
             "Item" = desc,
             "from (ft)" = casing_from,
             "to (ft)" = casing_to) %>% 
      select(`Hole Id`, From, To, `Outer Diameter`, `Inner Diameter`, `Offset`, `Construction Keyword`, `Item`, `from (ft)`,`to (ft)`) %>% 
      bind_rows(out_row)
  }
  
  # Organizing screen data
  screen_data <- screen %>% filter(wtn == well_tag_number)
  if(nrow(screen_data) > 0){
    out_row <- screen_data %>% 
      mutate("Hole Id" = well_tag_number,
             "From" = screen_from*0.3048,
             "To" = screen_to*0.3048,
             "Outer Diameter" = screen_diameter,
             "Inner Diameter" = NA_real_,
             "Offset" = 0,
             "Construction Keyword" = screen_assembly_code_long,
             "Item" = desc,
             "from (ft)" = screen_from,
             "to (ft)" = screen_to) %>% 
      select(`Hole Id`, From, To, `Outer Diameter`, `Inner Diameter`, `Offset`, `Construction Keyword`, `Item`, `from (ft)`,`to (ft)`) %>% 
      bind_rows(out_row)
  }
  
  # Returning the formatted table
  return(arrange(out_row, From, To))
}
