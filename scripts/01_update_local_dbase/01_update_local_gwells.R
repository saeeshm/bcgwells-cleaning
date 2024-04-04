# Author: Saeesh Mangwani
# Date: 2024-03-09

# Description: Downloading the database from the GWELLS platform (note that I
# work with the separated CSV databases as these are a more complete source of
# information that the synthesized version delivered via the API or the BCData
# platform)

# ==== libraries ====
library(purrr)
library(stringr)
library(readr)
library(dplyr)

# ==== Paths and global variables ====

# Directory where the newly downloaded database will be stored
out_dir <- 'data'

# Download URL for the CSV zip package - may need to updated periodically
url <- 'https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip'

# Downloaded file name
fname <- paste0('gwells_', Sys.Date(), '.zip')

# Path to update report
update_report_path <- file.path(out_dir, 'update_status.txt')

# Temporary paths/directories
zip_path <- file.path(out_dir, fname)
uz_dir <- file.path(out_dir, 'gwells_download')

# ==== Updating tables ====

# Downloading new tables
download.file(url, destfile = zip_path, method='auto')

# Unzipping
uz_dir <- file.path(out_dir, 'gwells_download')
unzip(file.path(out_dir, fname), exdir=uz_dir)

# All tables names in the new download
tabnames <- list.files(uz_dir, pattern='\\.csv')

# For every table except lithology, overwriting the current version with the
# newly downloaded version
walk(tabnames, \(x){
  if(str_detect(x, 'lithology')) return()
  from_path <- file.path(uz_dir, x)
  to_path <- file.path(out_dir, x)
  file.copy(from_path, to_path, overwrite=T)
})

# For the lithology, only adding new wells (no changes to existing well data)
litho_curr <- read_csv(file.path(out_dir, 'lithology.csv'))
litho_new <- read_csv(file.path(uz_dir, 'lithology.csv'))
names(litho_new) <- names(litho_curr)

# Getting only rows added (i.e no existing from-to for this well)
new_rows <- anti_join(litho_new, litho_curr,
                      by=c('well_tag_number', 'lithology_from', 'lithology_to'))

# Adding them directly to the lithology table as updates (potential redundancies
# are later resolved during the lithology tidying process)
litho <- bind_rows(litho_curr, new_rows) |> 
  arrange(well_tag_number, lithology_from, lithology_to)

# Writing the updated lithology table to disk
write_csv(litho, file.path(out_dir, 'lithology.csv'), append = F)

# Adding an update report txt document
sink(update_report_path, append=F)
cat(paste('Database last updated on:', Sys.time()))
cat('\n')
cat("All tables except lithology are OVERWRITTEN. Lithology table is APPENDED with any new rows/wells")
sink()

# Clearing zip and download directory
unlink(uz_dir, recursive=T)
file.remove(zip_path)
print("Database update complete")

