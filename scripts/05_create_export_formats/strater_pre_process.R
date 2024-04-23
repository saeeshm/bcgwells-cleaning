# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-10-26

# Description: A script that conducs preparatory manipulations on the gwells
# tables and initializes helper functions to assist in the creation of a strater
# table

# ==== Loading libraries ====
library(readr)
library(dplyr)

# ==== Paths and global variables ====

# Bedrock lithology cleaned
litho_path <- 'output/lithology_frac_yield_extracted.csv'
# Others
# litho_path <- 'data/lithology.csv'
well_path <- 'data/well.csv'
drilling_path <- 'data/drilling_method.csv'
screen_path <- 'data/screen.csv'
casing_path <- 'data/casing.csv'
liner_path <- 'data/perforation.csv'

# Path to reference table containing the well tag numbers we're interested in
# formatting
wtn_input  <- 'input/wtn_input.csv'

# ==== Reading data ====
litho <- read_csv(litho_path)
# bedrock <- read_csv(bedrock_path)
well <- read_csv(well_path)
screen <- read_csv(screen_path)
casing <- read_csv(casing_path)
liner <- read_csv(liner_path)
drilling <- read_csv(drilling_path)

# Defaulting to all wells if no input file exists
if(file.exists(wtn_input)){
  wtns <- read_csv(wtn_input, col_names=F) |> 
    pull(X1)
}else{
  wtns <- unique(well$well_tag_number)
}

# ==== Table pre-processing ====

# Filtering each table with the well tag numbers inputted and applying some
# preprocessing steps where required (if there are none in the csv, using all
# wells)

# lithology table
litho <- litho %>% 
  filter(wtn %in% wtns)

# Bedrock table
# bedrock <- bedrock %>% 
#   filter(wtn %in% ifelse(nrow(wtns) > 0, wtns$X1, litho$well_tag_number))

# Well table
well <- well %>% 
  # Filtering
  filter(well_tag_number %in% wtns) |> 
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
  filter(well_tag_number %in% wtns) |> 
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
  mutate("desc" = paste(diameter_inches, "in dia.", casing_mat_code_long, "casing.", "Drive shoe", drive_shoe_code))

# Screen table
screen <- screen %>% 
  # Filtering
  filter(well_tag_number %in% wtns) |> 
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
  mutate("desc" = paste(screen_diameter_inches, "in dia.", 
                        "screen type:", screen_assembly_code_long, 
                        "slot size:", slot_size, "mm")) %>% 
  # converting the slot size to mm based on the rule given by Antonio
  mutate(slot_size = if_else(slot_size <= 1, slot_size*25.4, (slot_size/1000)*25.4))

# Drilling table
drilling <- drilling %>% 
  # Filtering
  filter(well_tag_number %in% wtns) |> 
  # Grouping so as to remove duplicates by combining all descriptions associated with a single wtn into one row
  group_by(well_tag_number) %>% 
  # Combining descriptions
  summarise(drilling_method_code = paste(drilling_method_code, collapse = ";")) %>% 
  # ungrouping. Data are now deduplicated.
  ungroup()

# ==== Intializing helper functions ====

# A function that takes a well tag number and gathers the surface seal, filter,
# casing, screen and liner information associated with that well from all the
# relevant tables. It returns a dataframe containing all of these data formatted
# in strater format.
format_construction_strater <- function(wtn){
  # Column names for construction table
  cnames <- c('Hole Id', 'From', 'To', 'Outer Diameter', 'Inner Diameter',
              'Offset', 'Construction Keyword', 'Item', 'from (ft)', 'to (ft)')
  # Organizing surface seal data
  well_data <- well %>% filter(well_tag_number == wtn)
  ss_row <- if(!is.na(well_data$surface_seal_material_code)){
    tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = 0,
      "To" = well_data$surface_seal_depth_ft*0.3048,
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = "Surface Seal",
      "Item" = well_data$surface_seal_desc,
      "from (ft)" = 0,
      "to (ft)" = well_data$surface_seal_depth_ft
    )
  }else{
    tibble()
  }
  
  # Organizing backfill data
  bf_row <- if(!is.na(well_data$backfill_type)){
    tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = 0,
      "To" = well_data$backfill_depth_ft*0.3048,
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = "Backfill",
      "Item" = tolower(well_data$backfill_type),
      "from (ft)" = 0,
      "to (ft)" = well_data$backfill_depth_ft)
  }else{
    tibble()
  }
  
  # Organizing filter data
  fl_row <- if(!is.na(well_data$filter_pack_material_code)){
    tibble(
      "Hole Id" = well_data$well_tag_number,
      "From" = well_data$filter_pack_from_ft*0.3048,
      "To" = well_data$filter_pack_to_ft*0.3048,
      "Outer Diameter" = NA_real_,
      "Inner Diameter" = NA_real_,
      "Offset" = 0,
      "Construction Keyword" = case_when(
        str_detect(well_data$filter_pack_material_code,"GRAVL") ~ "Gravel Pack",
        str_detect(well_data$filter_pack_material_code,"SAND") ~ "Sorted Sand",
        TRUE ~ "Other Filter Pack"
      ),
      "Item" = well_data$filter_pack_desc,
      "from (ft)" = well_data$filter_pack_from_ft,
      "to (ft)" = well_data$filter_pack_to_ft)
  }else{
    tibble()
  }
  
  # Organizing casing data
  casing_data <- casing %>% filter(wtn == well_tag_number)
  cs_row <- if(nrow(casing_data) > 0){
    casing_data %>% 
      mutate("Hole Id" = well_tag_number,
             "From" = `casing_from_ft-bgl`*0.3048,
             "To" = `casing_to_ft-bgl`*0.3048,
             "Outer Diameter" = diameter_inches,
             "Inner Diameter" = NA_real_,
             "Offset" = 0,
             "Construction Keyword" = case_when(
               casing_code == "SURFACE" ~ "Surface Casing",
               casing_code == "OPEN" | casing_material_code == "OPEN_HOLE" ~ "Open Hole",
               TRUE ~ "Casing"),
             "Item" = desc,
             "from (ft)" = `casing_from_ft-bgl`,
             "to (ft)" = `casing_to_ft-bgl`) |> 
      dplyr::select(all_of(cnames))
  }else{
    tibble()
  }
  
  # Organizing screen data
  screen_data <- screen %>% filter(wtn == well_tag_number)
  sc_row <- if(nrow(screen_data) > 0){
    screen_data %>% 
      mutate("Hole Id" = well_tag_number,
             "From" = `screen_from_ft-bgl`*0.3048,
             "To" = `screen_to_ft-bgl`*0.3048,
             "Outer Diameter" = screen_diameter_inches,
             "Inner Diameter" = NA_real_,
             "Offset" = 0,
             "Construction Keyword" = screen_assembly_code_long,
             "Item" = desc,
             "from (ft)" = `screen_from_ft-bgl`,
             "to (ft)" = `screen_to_ft-bgl`) |> 
      dplyr::select(all_of(cnames))
  }else{
    tibble()
  }
  
  # Returning the formatted table
  out_tab <- bind_rows(ss_row, bf_row, fl_row, cs_row, sc_row)
  return(out_tab)
}

