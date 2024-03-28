# Author: Saeesh Mangwani
# Date: 2024-03-13

# Description: EDA for GWells

# ==== libraries ====
library(dplyr)
library(vroom)

# ==== Paths and global variables ====
gwells <- vroom('data/wells.csv')
litho <- vroom('data/lithology.csv')

litho |> head(50) |> View()

# Emptiness of columns - GWELLS
gwells$FINISHED_WELL_DEPTH |> is.na() |> table(useNA = 'ifany')
gwells$YIELD |> is.na() |> table()
gwells$ARTESIAN_COND |> is.na() |> table()
gwells$ARTESIAN_PRESSURE |> is.na() |> table()
gwells$STATIC_WATER_LEVEL |> is.na() |> table()
gwells$AQUIFER_MATERIAL |> is.na() |> table()

# Emptiness of columns - lithology
litho$LITHOLOGY_OBSERVATION |> table()

litho |> 
  filter(WELL_TAG_NUMBER == 17549) |> 
  View()

litho |> 
  filter(LITHOLOGY_OBSERVATION == "granodiorite. Water  @ 84'") |> 
  View()
