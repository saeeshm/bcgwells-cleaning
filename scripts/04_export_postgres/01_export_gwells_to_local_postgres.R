# Author: Saeesh Mangwani
# Date: 2024-04-02

# Description: Exporting created and tidied Gwells tables to postgres

# ==== libraries ====
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(DBI)
library(rjson)
library(RSQLite)

# ==== Paths and global variables ====

# Postgres credentials
creds_path <- 'options/credentials.json'

# Path list
paths <- list(
  'litho_cln' = 'output/lithology_frac_yield_extracted.csv',
  'litho_raw' = 'data/lithology.csv',
  'well' = 'data/well.csv',
  'drilling' = 'data/drilling_method.csv',
  'screen' = 'data/screen.csv',
  'casing' = 'data/casing.csv',
  'liner' = 'data/perforation.csv',
  'pumptest' = 'data/pt_aquifer_parameters.csv'
)

# Column types list
ctypes <- list(
  'litho_cln' = 'ccddcccddcdddc',
  'litho_raw' = 'cddcccccdcc',
  # Guessing for well, but these are manually corrected later
  'well' = '?',
  'drilling' = 'cc',
  'screen' = 'cdddcd',
  'casing' = 'cdddccdc',
  'liner' = 'cdd',
  'pumptest' = 'cdDcdcdddddcc'
)

# ==== Reading data ====
dats <- pmap(list(paths, names(paths), ctypes), \(path, tabname, typevec){
  out <- read_csv(path, col_types=typevec)
  if(tabname=='well'){
    out <- out |>
      mutate(across(contains('date'), lubridate::ymd))
  }
  return(out)
})

# ==== Opening database connection ====

# Reading credentials
creds <- fromJSON(file = creds_path)

# Openining database connection
conn <- dbConnect(
  RPostgres::Postgres(), 
  host = creds$host, 
  dbname = creds$dbname,
  user = creds$user, 
  password = creds$password
)

# ==== Writing tables to database ====
iwalk(dats, \(x, y){
  print(y)
  dbWriteTable(
    conn,
    DBI::Id(schema = creds$schema, table = y),
    x,
    append = F,
    overwrite=T
  )
})

