# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2024-04-24

# Description: Subsetting selected wells from the leapfrom formatted table,
# using an input set of WTNs

# ==== Libraries ====
library(readr)
library(rjson)
library(DBI)
library(RPostgres)
library(dplyr)
library(purrr)
library(openxlsx)

# ===== Paths and global variables =====

# Input WTN csvs
input_path <- 'input/leapfrom_wtn_input.csv'

# Output path to store the data subset
output_path <- 'output/subsets/leapfrom_wtn_subset.xlsx'

# Postgres credentials path
creds_path <- 'options/credentials.json'

# Postgres schema
schema <- 'bcgwells_leapfrog'

# List of leapfrom databases
tabnames <-  c(
  'Collar', 
  # 'Survey' = survey,
  "Lithology",
  "Fractures",
  "Screen",
  "Water level"
)
names(tabnames) <- tabnames

# ===== Database connection =====

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

# ===== Filtering data for relevant wells =====

# Reading input wtns
wtns <- read_csv(input_path, col_names = F) |> pull(X1)

# Subsetting from the database
dats <- map(tabnames, \(x){
  print(x)
  # Building query
  query <- paste0('select * from ', schema, '."', x, '"', 
                  '\nwhere "Hole ID" in (', paste(wtns, collapse = ','), ')')
  # Getting table
  res <- dbGetQuery(conn, query)
})
dbDisconnect(conn)
# ===== Exporting subsetted table =====

# Building excel sheet
wb <- createWorkbook()
iwalk(dats, \(x, tabname){
  print(tabname)
  addWorksheet(wb, tabname)
  writeData(wb, tabname, x, startRow = 1, startCol = 1, colNames = T)
})

# Exporting excel sheet
saveWorkbook(wb, file = output_path, overwrite = TRUE)
print("workbook saved")
