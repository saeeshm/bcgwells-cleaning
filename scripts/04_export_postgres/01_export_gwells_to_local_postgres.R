# Author: Saeesh Mangwani
# Date: 2024-04-02

# Description: Exporting created and tidied Gwells tables to postgres

# ==== libraries ====
library(readr)
library(purrr)
library(DBI)
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

# ==== Reading data ====
dats <- map(paths, read_csv)

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
    DBI::Id(schema = creds$schema, table = "y"),
    x,
    append = F
  )
})

