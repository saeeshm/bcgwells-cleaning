# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-07-08

# Description: Helper functions for tidying and classifying material classes
# (matclass) in the gwells lithology table


# ==== Libraries ====
library(stringr)
library(stringi)
library(purrr)

# ==== Functions ====

# Cleans rows where the assigned depth for both from and to is 0 (appears that
# these have been created that have been created mainly for capturing
# overflowing comments). This cleaning is achieved by joining the comments and
# other data to the most-apprpriate adjacent row, and removing the offending row. This process is iteratively repeated for the whole table until all 0
clean_0_depth_rows <- function(df){
  # print(df$wtn[1])
  # Iterate through all the rows in the dataframe
  len <- nrow(df)
  # Setting an iteration variable i
  i <- 1
  # Iterating through the dataframe as long as it takes to go through the whole
  # table without encountering any comment rows (because the iteration gets
  # reset each time a comment row is found)
  while(i <= len){
    # If there is only 1 row for this well removing the 0-comment value and
    # breaking the loop to return the value as-is
    if(nrow(df) == 1) {
      df$interval_error <- NA_character_
      break
    }
    # If any row contains "0-Comments" in the interval_error column
    if(!is.na(df[i,]$interval_error) & df[i,]$interval_error == "0-depths"){
      
      # If it is the first row, joining the lithology comment with the row after
      # (there is no other row it could refer to) and dropping the comment row
      if(i == 1){
        # Combining the lithology comment
        df[i+1,]$lithology <- paste(df[i+1,]$lithology, df[i,]$lithology)
        
        # Removing the comment row
        df <- df[-i,]
        
        # Resetting i so that we restart iteration to ensure we don't miss any rows
        i <- 1
        
        # If it is the last row, joining the lithology comment with the row
        # before (there is no other row it could refer to) and dropping the
        # comment row
      }else if(i == nrow(df)){
        # Combining the lithology comment with the previous row
        df[i-1,]$lithology <- paste(df[i-1,]$lithology, df[i,]$lithology)
        
        # Removing this row
        df <- df[-i,]
        # Resetting i so that we restart iteration to ensure we don't miss any
        # rows
        i <- 1
        
        # Otherwise, checking which of the immediately adjacent rows has a
        # smaller depth value and assigning it to that one (the comment always
        # refers to the "prvs" row, regardless of how the table is arranged)
      }else{
        # If the next row is also a comment row, assigning to the previous row
        if(!is.na(df[i+1,]$interval_error) & df[i+1,]$interval_error == "0-depths"){
          # Combining the lithology comments
          df[i-1,]$lithology <- paste(df[i-1,]$lithology, df[i,]$lithology)
          
          # Removing the comment row
          df <- df[-i,]
          
          # Resetting i so that we restart iteration to ensure we don't miss any rows
          i <- 1
          # If the prevs row depth is lower than the next row depth, assigning
          # the comment to the prvs row
        }else if(!is.na(df[i-1,]$from_depth < df[i+1,]$from_depth) & (df[i-1,]$from_depth < df[i+1,]$from_depth)){
          # Combining the lithology comments
          df[i-1,]$lithology <- paste(df[i-1,]$lithology, df[i,]$lithology)
          
          # Removing the comment row
          df <- df[-i,]
          
          # Resetting i so that we restart iteration to ensure we don't miss any rows
          i <- 1
          # If the next row depth is lower, assigning the comment to the next
          # row and dropping the comment row
        }else{
          # Combining the lithology comment
          df[i+1,]$lithology <- paste(df[i+1,]$lithology, df[i,]$lithology)
          
          # Removing the comment row
          df <- df[-i,]
          
          # Resetting i so that we restart iteration to ensure we don't miss any rows
          i <- 1
        }
      }
    }else{
      i <- i+1
    }
  }
  return(df)
}

# A function to clean the rows titled 'blank' in the interval error section.
# After examining a number of rows manually, we see that the blank rows always
# refer to full rows higher up in the table. As a result, the function joins the
# comments for each blank row upwards. If only 1 row is left, the interval error
# is reset to NA and the single row is returned as-is
clean_blanks <- function(df){
  # print(df$wtn[1])
  # Iterate through all the rows in the dataframe
  len <- nrow(df)
  # Setting an iteration variable to the length of the table to allow us to
  # iterate upwards from the bottom
  i <- len
  # Iterating through the dataframe as long as it takes to go through the whole
  # table without encountering any blank rows (because the iteration gets reset
  # each time a blank row is found)
  while(i >= 1){
    # If there is only 1 row for this well removing the blank value and breaking
    # the loop to return the value as-is
    if(nrow(df) == 1) {
      df$interval_error <- NA_character_
      break
    }
    # If any row contains "blanks" in the interval_error column
    if(!is.na(df[i,]$interval_error) & df[i,]$interval_error == "Blanks"){
      # If both depth values are empty:
      if(is.na(df[i,]$from_depth) & is.na(df[i,]$to_depth)){
        # Appending the lithology comment to the comment of the prvs row (using
        # a semicolon as a separater, to indicate that these are different not
        # overspilled comments)
        df[i-1,]$lithology <- paste(df[i-1,]$lithology, df[i,]$lithology, sep = "; ")
        # Dropping the current row
        df <- df[-i,]
        # otherwise if only 1 depth value is empty (definition of blanks is that
        # at least one need be empty, so this is the only other case)
      }else{
        # Resetting the missing depth value to be equal to the available depth
        # value, as this row likely refers to a comment or a feature specific to
        # that single depth.
        df[i,]$from_depth <- ifelse(is.na(df[i,]$from_depth), df[i,]$to_depth, df[i,]$from_depth)
        df[i,]$to_depth <- ifelse(is.na(df[i,]$to_depth), df[i,]$from_depth, df[i,]$to_depth)
        # Removing the "blanks" comment so that we can keep the row
        df[i,]$interval_error <- NA_character_
      }
    }else{
      i <- i-1
    }
  }
  return(df)
}

# # A function to clean rows titled "OOS" or out-of-sequence in the interval
# error section. Since OOS rows are typically added to indicate a feature at a
# certain location, this function just changes to to_depth for each OOS row to
# be equal to the from_depth.
clean_oos_rows <- function(df) {
  # print(df$wtn[1])
  # For each row
  for (i in seq(nrow(df))){
    # If there is a row marked os out of order
    if(!is.na(df[i,]$interval_error) & str_detect(df[i,]$interval_error, "OOS|Out of Sequence")){
      # Setting the rows to_depth to be equal to its from depth and removing the oos tag
      df[i,]$to_depth <- df[i,]$from_depth
      # removing the OOS tag
      df[i,]$interval_error <- NA_character_
    }
  }
  # Removing potential duplication before returning
  return(distinct(df))
}

# A function that removes rows where the lithology description is either NA or
# an empty string, and if there is no associated estimate of water bearing flow
remove_uninformative_rows <- function(df){
  # If there is only 1 row, returning it as-is (to ensure it isn't simply kicked
  # from the database)
  if(nrow(df) == 1) {
    return(df)
  }
  # Otherwise removing any rows where the from and to-depths are the same, but
  # no information is available (no value in having them occupy a row space, as
  # they are supposed to indicate information at that depth and it is useless if
  # they don't)
  df |> 
    mutate(ltest = str_squish(lithology), .after='lithology') |> 
    filter(!((from_depth == to_depth) & 
               (is.na(ltest) | (ltest == '')) & 
               is.na(water_bearing_estimated_flow))) |> 
    dplyr::select(-ltest)
}

# A function the resets rows which range from 0 to a certain depth to instead
# range from the nearest to_depth to their final depth, unless no other row
# starting from 0 already exists
clean_error_depths <- function(df){
  # print(df$wtn[1])
  # Getting the to_depths and from_depths in the table
  depth_froms <- df |> arrange(from_depth) |> pull(from_depth)
  depth_tos <- df |> arrange(to_depth) |> pull(to_depth)
  
  # Iterating through the rows
  for (i in seq(nrow(df))){
    # If the row has a from_depth of NA
    if(is.na(df[i,]$from_depth)){
      # Resetting the from_depth for that row to be equal to the nearest
      # depth_to less than it. If there is no lower depth_to, setting the
      # from_depth to 0
      df[i,]$from_depth <- ifelse(
        length(tail(na.omit(depth_tos[df[i,]$to_depth > depth_tos]), n=1)) > 0,
        tail(na.omit(depth_tos[df[i,]$to_depth > depth_tos]), n=1),
        0
      )
      # If the row has a from_depths of 0
    }else if (df[i,]$from_depth == 0){
      # Checking to see if there are any other rows with a 0 from_depth but that
      # have a lower to_depth (using the replace function to ensure that NAs
      # don't break the code). If there are no such rows, doing nothing (this is
      # the only row with a valid 0 value). If there are other such rows,
      # editing this row's from_depth to be equal to the nearest to_depth less
      # than it (there will by definition be a lower depth cuz the above check
      # implicitly checks for this)
      if(any(df[replace(df$from_depth == 0, is.na(df$from_depth == 0), FALSE),]$to_depth < df[i,]$to_depth, na.rm = T)){
        df[i,]$from_depth <- tail(na.omit(depth_tos[df[i,]$to_depth > depth_tos]), n=1)
      }
    }
    # If the row has a missing to_depth
    if(is.na(df[i,]$to_depth)){
      # Restting the to_depth to be equal to the nearest from_depth greater than
      # the current row's from_depth If there are none, just using the current
      # from_depth
      df[i,]$to_depth <- ifelse(length(tail(na.omit(depth_tos[df[i,]$to_depth > depth_tos]), n=1)) > 0,
                                  tail(na.omit(depth_tos[df[i,]$to_depth > depth_tos]), n=1),
                                  df[i,]$from_depth)
    }
  }
  return(df |> arrange(from_depth))
}

# A function that inverts rows where the from_depth is inappropriately higher
# than the to_depth. Since we don't know the actual intention of the row,
# inverting to ensure logical consistency seems like the best option
invert_mismatched_rows <- function(df){
  # Getting the names
  names <- names(df)
  # mapping over all rows and editing the from and to depths wherever the from
  # is bigger than the to
  df <- pmap_dfr(df, function(from_depth, to_depth, ...){
    if(!is.na(from_depth > to_depth) & from_depth > to_depth){
      temp <- from_depth
      from_depth <- to_depth
      to_depth <- temp
    }
    tibble(..., from_depth, to_depth)
    # arranging the returned df in the same column order as before
  }) |> select(names(df))
  # Returning
  return(df)
}

# A function that combines any rows that cover the same depth ranges
join_dup_ranges <- function(df){
  # print(df$wtn[1])
  
  # If there is only 1 row no duplicates can exist so just returning the df
  # as-is
  if(nrow(df) < 2){
    return(df)
  }else{
    # Adding a temporary dup column
    df <- df |> mutate(dup = F)
    
    # Iterating over the dataframe
    for(o in seq(nrow(df))){
      # If it is a duplicate already, skipping it
      if(df[o,]$dup){
        next
      }
      # getting an index to use for iteration
      index <- seq(nrow(df))
      # Otherwise iterating iterating again through the REST of the table (i.e
      # not including the current row we're iterating comparing against)
      for (i in index[-o]){
        # if the depth from and to of the current comparison row match exactly
        # with any row in the table
        if(identical(df[o,]$from_depth, df[i,]$from_depth) & identical(df[o,]$to_depth, df[i,]$to_depth)){
          # Combining the lithology comments
          df[o,]$lithology <- paste(df[o,]$lithology, df[i,]$lithology, sep = "; ")
          # Averaging the water yield columns if both are present
          df[o,]$water_bearing_estimated_flow <- mean(
            c(df[o,]$water_bearing_estimated_flow, df[i,]$water_bearing_estimated_flow),
            na.rm=T
          )
          # Assigning a true value to the temporary duplicate variable for the
          # second of these 2 rows.
          df[i,]$dup <- T 
        }
      }
    }
    # Returning only the unique rows
    return(df |> filter(!dup) |> select(-dup))
  }
}

# A function that ensures that the final well depth in the lithology table is
# consistent with the reported depth of completion in the well table
check_end_depths <- function(df, well, well_depth_column){
  print(df$wtn[1])
  
  # Getting any finished depth value from the well table
  currwell <- well |> 
    filter(well_tag_number %in% df$wtn[1]) 
  
  # If this well is not present in the well table, setting the finished depth to
  # NA
  if(nrow(currwell) == 0){
    this_finished_depth <- NA_real_
  }else{
    this_finished_depth <-currwell |> pull(!!sym(well_depth_column))
  }
  
  # Getting the final row - if there is any to-depth bigger than the largest
  # from-depth, using the to-depth to find the max. otherwise using the max
  # from-depth. The correct query is first saved in an expression
  fin_expr <- ifelse(any(df$to_depth > max(df$from_depth), na.rm = T), 
                     expression(df$to_depth == max(df$to_depth, na.rm = T)), 
                     expression(df$from_depth == max(df$from_depth, na.rm = T)))
  # The expression is then evaluated to get the row
  fin_row <- df[eval(fin_expr),] |> 
    arrange(desc(to_depth)) |> 
    head(1)
  
  # Setting the new finished depth to the same as the current final row for now.
  new_fin <- fin_row
  
  # If a finished well depth exists and is not 0
  if(!(is.na(this_finished_depth) | this_finished_depth == 0)){
    # If both end depths are the same or the current end depth is larger,
    # keeping the current one as is. If not, changing the value to the finished
    # well depth
    new_fin <- fin_row |> 
      mutate(to_depth = ifelse(!is.na(fin_row$to_depth) & (fin_row$to_depth >= this_finished_depth), 
                               fin_row$to_depth, 
                               this_finished_depth))
    
    # If a finished well depth does not exist and the to_depth is either 0 or
    # NA, simply setting the to_depth of this last row to equal to its from
    # depth, as this indicates information lying only at that depth
  }else if(is.na(fin_row$to_depth) | fin_row$to_depth == 0){
    new_fin <- fin_row |> 
      mutate(to_depth = from_depth)
  }
  
  # Updating the final row by finding and dropping the original row and then
  # adding the edited/new ones
  df <- anti_join(df, fin_row, by=names(df)) |> 
    bind_rows(new_fin)
  # Returning
  return(df)
}

# A function that checks the unique values in the casing and screen tables to
# make sure that the matclass queries we use to filter them are still valid
# (quick way to check if the function needs to be updated based on changes made
# by the GWELLs maintainers)
check_matclass_queries <- function(well, casing, screen){
  print("Well-table query:")
  print(paste("Current well column used for finding bedrock depth:", 'bedrock_depth_ft-bgl'))
  print("Bedrock-associated names currently in the well database: ")
  print(names(well)[str_detect(names(well), 'bedrock')])
  cat("===\n\n")
  
  print("Casing-table queries:")
  print(paste("Current query for casing material code:", 'casing_material_code'))
  print("Columns in the casing database: ")
  print(names(casing))
  cat("---\n")
  print(paste("Material class query for classification:", 
              paste(c('steel', 'open_hole'), collapse=', ')))
  print("All unique material classes: ")
  print(unique(tolower(casing$casing_material_code)))
  cat("===\n\n")
  
  print("Screen-table queries:")
  print(paste("Current query for screen assembly type code:", 'screen_assembly_type_code'))
  print("Columns in the casing database: ")
  print(names(screen))
  cat("---\n")
  print(paste("Assembly type query for classification:", 
              paste(c("screen", "k_riser", "k_packer", 
                      "scrn_blank", "tail_pipe"), collapse=', ')))
  print("All unique assembly types classes: ")
  print(unique(tolower(screen$screen_assembly_type_code)))
  cat("===\n\n")
}

# A function that does a preliminary edit of the matclass column based on any
# available data from the well, casing and screen tables (ancilliary information)
classify_matclass_ancilinfo <- function(df, well, casing, screen){
  # print(df$wtn[1])
  
  # Creating a separate matclass column that contains the classification done
  # using no regex
  df <- df |> mutate(matclass_noregex = NA_character_, .after='lithology')
  
  # Turning all character columns to lowercase
  df <- df |> mutate_if(is.character, tolower)
  
  # Getting Values to assist with classification --------
  
  # The bedrock depth from the well database (will return a value or NA if missing)
  this_bedrock <- well[well$well_tag_number %in% df$wtn[1],]$`bedrock_depth_ft-bgl`
  # If there is no data for this row returning NA, otherwise returning the value
  # itself (which may be an actual value or stil just NA)
  this_bedrock <- ifelse(length(this_bedrock) == 0, NA_real_, this_bedrock)
  
  # The relevant casing info for the well, if any 
  this_casing <- casing |> 
    dplyr::select(well_tag_number, casing_material_code, 
                  'casing_from' = `casing_from_ft-bgl`, 
                  'casing_to' = `casing_to_ft-bgl`) |>
    mutate(casing_material_code = tolower(casing_material_code)) |> 
    filter(well_tag_number %in% df$wtn[1])
  # If there is no casing returning an NA row. Otherwise, returning the casings 
  this_casing <- ifelse(nrow(this_casing > 0), list(this_casing), list(this_casing[NA,][1,]))[[1]]
  
  # The relevant screen info for the well, if any
  this_screen <- screen |> 
    select(well_tag_number, screen_assembly_type_code, 
           'screen_from' = `screen_from_ft-bgl`, 
           'screen_to' = `screen_to_ft-bgl`) |>
    mutate(screen_assembly_type_code = tolower(screen_assembly_type_code)) |> 
    filter(well_tag_number %in%df$wtn[1])
  # If there are no screens returning an NA row. Otherwise, returning the casings 
  this_screen <- ifelse(nrow(this_screen > 0), list(this_screen), list(this_screen[NA,][1,]))[[1]]
  
  # All screen assembly types to query (easier to define it outside the loop)
  screen_query <- c("screen", "k_riser", "k_packer", "scrn_blank", "tail_pipe")
  
  # Iterating through all the rows and checking a number of conditions to
  # classify the matclass. The conditions are ordered in terms of their
  # robustness as classification methods, thus the minute any one method
  # succeeeds none of the others will be checked. Conversely, lower methods are
  # only checked if uppper methods fail, due to their lesser robustness
  for(i in seq(nrow(df))){
    # If there is a bedrock depth
    if(!is.na(this_bedrock)){
      # any rows that start AT OR BELOW the bedrock depth or that contain the
      # bedrock depth within their range are classified as bedrock
      if(df[i,]$from_depth >= this_bedrock){
        df[i,]$matclass_noregex <- "bedrock"
        next
        # Otherwise if the row starts above it, it is of an overburden type
        # (this is because bedrock depth is the only robust measure we have, so
        # if it exists, we know exactly what the classification will be above or
        # below it)
      }else{
        df[i,]$matclass_noregex <- "overburden"
        next
      }
    }
    # If there is a steel casing
    else if(!all(is.na(this_casing)) & 
            any(this_casing$casing_material_code %in% "steel", na.rm = T) ){
      # Getting the location of the deepest available steel casing
      steel_cas_loc <- this_casing |> 
        filter(casing_material_code %in% 'steel') |> 
        pull(casing_to) |> 
        as.numeric() |> 
        max(na.rm=T)

      # Writing an intermediary step to catch the infinity that is returned if
      # max only receives NAs as inputs (i.e all the relevant casing depths are
      # missing. In this case, the follow up condition fails (no depth can be
      # less than -Inf) and the row is not updated)
      steel_cas_loc <- ifelse(is.infinite(steel_cas_loc), -Inf, steel_cas_loc)
      
      # Any lithology that ends AT OR ABOVE it in depth is classified as
      # overburden. If there is more than one steel casing, choosing the max
      # depth to, as the determines the lowest point requiring steel casing
      # implying that anything above it must be overburden material
      if(!is.na(df[i,]$to_depth) & df[i,]$to_depth <= steel_cas_loc){
        df[i,]$matclass_noregex <- "overburden"
        next
      }
    }
    # If there is an open_hole casing
    else if ( !all(is.na(this_casing)) & 
              any(this_casing$casing_material_code %in% "open_hole", na.rm = T) ){
      # Getting the smallest to_depth with this type of casing 
      oh_casing_loc <- this_casing |> 
        filter(casing_material_code %in% 'open_hole') |> 
        pull(casing_to) |> 
        as.numeric() |> 
        min(na.rm=T)
      # Any lithology that starts AT OR BELOW it in depth is classified as
      # bedrock If there is more than one open_hole casing, choosing the min
      # depth to, as the determines the highest point requiring open hold casing
      # implying that anything under it must be bedrock material. Note that we
      # don't need an intermediary step to catch the NA here since if all depth
      # values are NA the min function will return Inf, and since nothing can be
      # higher than infinity the update will automatically fail and we'll move
      # on to the next step
      if(!is.na(df[i,]$to_depth) & df[i,]$to_depth >= oh_casing_loc){
        df[i,]$matclass_noregex <- "bedrock"
        next
      }
    }
    # If there is any of a defined set of screens (query set defined above)
    else if ( !all(is.na(this_screen)) & 
              any(this_screen$screen_assembly_type_code %in% screen_query, na.rm = T) ){
      # Getting the location of the deepest available relevant screen
      screen_loc <- this_screen |> 
        filter(screen_assembly_type_code %in% screen_query) |> 
        pull(screen_to) |> 
        as.numeric() |> 
        max(na.rm=T)
      
      # Writing an intermediary step to catch the infinity that is returned if
      # min only receives NAs as inputs (i.e all the relevant casing depths are
      # missing. In this case, the follow up condition fails (no depth can be
      # less than -Inf) and the row is not updated)
      screen_loc <- ifelse(is.infinite(screen_loc), -Inf, screen_loc)
      
      # Any lithology that ends AT OR ABOVE the lowest available screen in depth
      # is classified as overburden.
      if(df[i,]$to_depth <= screen_loc){
        df[i,]$matclass_noregex <- "overburden"
        next
      }
    }
  }
  # Returning
  return(df)
}

# A function for cleaning the matclass column using regular expression parsing
# of the lithology column (this is the workhorse of this cleaning process, as
# such it is quite complex)
classify_matclass_regex <- function(df){
  # First pass
  df <- df |> 
    mutate(matclass = case_when(
      
      # If only one or two words are detected and they are 
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        # bedrock features, or
        (str_detect(lithology, "b\\S*d\\s?\\S*k|b\\/r\\b|b\\.r\\.|rock\\b") |
           # the following rock types, classify as bedrock
           str_detect(lithology, "(b\\S*s\\S*lt|gr\\S*n\\S*te|\\bgran|\\barg|argel|gn\\S*ss|schist|mica|quartz|diorite|slate|\\bsh\\b|shale|shael|shaley|shaly|volcani|volcanic|porphry|porphyr|pourphry|porphery|cryst|\\bchert|f\\S?ldsp\\S?r|lava|metamorhp|\\S+st\\S?ne|s\\S*nd\\s?st[\\Sne]?|conglom|\\S+lite\\b|\\S+mite\\b|coal)")) ~ "bedrock",
      
      # sand/gravel
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        str_detect(lithology, "gra?ve?l?s?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "sa?na?ds?") ~ "sand/gravel",
      
      # clay/sand
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "sa?na?ds?") ~ "clay/sand",
      # clay/silt
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "si?lts?") ~ "clay/silt",
      
      # silt/sand
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        str_detect(lithology, "sa?na?ds?") &
        str_detect(lithology, "si?lts?") ~ "silt/sand",
      
      # silt/gravel
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,1}$") & 
        str_detect(lithology, "gra?ve?l?s?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "si?lts?") ~ "silt/gravel",
      
      # till
      str_detect(lithology, "till|dri?ft|hard\\s?pan") ~ "till",
      
      # clay
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*$") & 
        str_detect(lithology, "cl\\S*ys?") ~ "clay",
      
      # silt
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*$") & 
        str_detect(lithology, "si?lts?") ~ "silt",
      
      # fill/soil/organic
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*$") & 
        str_detect(lithology, "wood|rot|fill|alluv|loam|soil|earth|organic|peat|muskeg|muck|loam|mud|alluv") ~ "fill/soil/organic",
      
      # unknown/overburden
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*$") & 
        str_detect(lithology, "dug|pit|overb\\S+n|surfic|aggreg|ovrbur|overbu|o.b\\b|.ver.*b\\S+den") ~ "overburden"
    ))
  
  # First, classifying bedrock
  df <- df |> 
    mutate(matclass = case_when(
      # First, if there is already text in the matclass column, leaving it
      # as-is. In case the text is bedrock, no further editing is editing, if it
      # is overburden, this set of regex is not useful so needs to be skipped
      # anyway If there is only 1 word in the lithology table and there are:
      # !is.na(matclass) ~ matclass,
      
      # If only one to three words are detected
      str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,2}$") & 
        # And they don't contain any of these terms
        str_detect(lithology, "^((?!till|soil|alluv).)*$") & 
        # but it does contain bedrock features
        (str_detect(lithology, "b\\S*d\\s?\\S*k|b\\/r\\b|b\\.r\\.|rock\\b") |
           # or one of the following rock types
           str_detect(lithology, "(b\\S*s\\S*lt|gr\\S*n\\S*te|\\bgran|\\barg|argel|gn\\S*ss|schist|mica|quartz|diorite|slate|\\bsh\\b|shale|shael|shaley|shaly|volcani|volcanic|porphry|porphyr|pourphry|porphery|cryst|\\bchert|f\\S?ldsp\\S?r|lava|metamorhp|\\S+st\\S?ne|s\\S*nd\\s?st[\\Sne]?|conglom|\\S+lite\\b|\\S+mite\\b|coal)")) ~ "bedrock",
      
      # If it contains indications of fractures
      str_detect(lithology, "facture|faults?|fract?\\S*") ~ "bedrock",
      
      # If it contains any of the bedrock keywords along with a frac-yield indication
      str_detect(lithology, "(be?d\\s?o?ro?c?k|b\\/r\\b|b\\.r\\.|b\\S*d\\S*k|b\\S*s\\S*lt|gr\\S*n\\S*te|\\bgran|\\barg|argel|gn\\S*ss|schist|mica|quartz|diorite|slate|\\bsh\\b|shale|shael|shaley|shaly|volcani|volcanic|porphry|porphyr|pourphry|porphery|cryst|\\bchert|f\\S?ldsp\\S?r|lava|metamorhp|\\S+st\\S?ne|s\\S*nd\\s?st[\\Sne]?|conglom|\\S+lite\\b|\\S+mite\\b|coal)\\w*\\s@\\d") ~ "bedrock",
      
      # If the text doesn't contain any of the following words/expressions
      str_detect(lithology, "^((?!bo?u?lde?r|float|cobble|gra?ve?l?|cl\\S*ys?|si?lts?\\b|sa?na?ds?\\b|till).)*$") & 
        # but does contain any of the following rock types or bedrock terms
        (str_detect(lithology, "b\\S*d\\s?\\S*k|b\\/r\\b|b\\.r\\.|rock\\b") |
           # Basalt, granit, argel, gneiss, mica, quartz, diorite, slate
           str_detect(lithology, "b\\S*s\\S*lt|granite|\\bgran|\\barg|argel|gn\\S*ss|schist|mica|quartz|diorite|slate") |
           # Shale, volcanic, poryphyr, cryst, chert
           str_detect(lithology, "\\bsh\\b|shale|shael|shaley|shaly|volcani|volcanic|porphry|porphyr|pourphry|porphery|cryst|\\bchert") |
           # Feldspar, lava, metamorphic, sandstone, conglomerated (non
           # surficial) rock, *lite, *mite, coal
           str_detect(lithology, "f\\S?ldsp\\S?r|lava|metamorhp|\\S+st\\S?ne|s\\S*nd\\s?st[\\Sne]?|conglom|\\S+lite\\b|\\S+mite\\b|coal")) ~ "bedrock",
      
      # If it doesn't contain indications of loose materials or screens but does
      # say ss or s.s (but not in partcles or traces)
      str_detect(lithology, "^((?!bo?u?lde?r|float|cobble|gra?ve?l?|si?lts?\\b|sa?na?ds?\\b|till).)*$") &
        str_detect(lithology, "(?<!(pa?rti?cle?s?|tra?ce?s?|bi?ts).{0,10})(\\bss\\b|\\bs\\.s\\b)(?![\\s\\S]*(pa?rti?cle?s?|tra?ce?s?|bi?ts))") &
        str_detect(lithology, "^((?!screen).)*$") ~ "bedrock",
      
      # If it doesn't contain indications of silt gravel or till but still has
      # conglomerated material
      str_detect(lithology, "^((?!si?lts?\\b|cl\\S*ys?).)*$") & 
        str_detect(lithology, "^((?!gra?ve?l?|sa?na?ds?\\b).)*$") &
        str_detect(lithology, "^((\\bti?ll).)*$") &
        str_detect(lithology, "conglom|comgl") ~ "bedrock",
      
      # If there are indications of a hard formation with a yield value but no
      # loose material like gravel silt or till
      str_detect(lithology, "(ha?rd).\\bfo?rma?tion") & 
        str_detect(lithology, "(\\d+\\s?gp[mhsd])*") & 
        str_detect(lithology, "^((?!gra?ve?l?|sa?na?ds?\\b).)*$") & 
        str_detect(lithology, "^((si?lts?\\b|cl\\S*ys?|till).)*$") ~ "bedrock",
      
      # If there are indications of broken rock but no loose material like
      # gravel silt or till
      str_detect(lithology, "^((?!bo?u?lde?r|float|cobble).)*$") & 
        str_detect(lithology, "^((?!gra?ve?l?|sa?na?ds?\\b).)*$") & 
        str_detect(lithology, "^((si?lts?\\b|cl\\S*ys?).)*$") &
        str_detect(lithology, "bro?ke?n\\s?ro?ck") ~ "bedrock",
      
      # If it is classified as a reservoir tank reserve or cistern
      str_detect(lithology, "reservoir|ta?nk|reserv|cistern") ~ "bedrock"
    ))
  
  # till
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # If it does not contains indications of gravel rocks or sand and is
      # glacial
      str_detect(lithology, "^((?!(sa?na?ds?|gra?ve?l?|\\bsto?ne?s?|\\brocks?\\b)).)*$") & 
        str_detect(lithology, "glacial") ~ "till",
      # If it contains indications of gravel/cobble/pebbles/rocks and clay or
      # silts
      str_detect(lithology, "gra?ve?l?e?y?|\\bcob(ble)?\\W?|pebb?\\W?|bo?u?lde?r|float|wood|\\bsto?ne?s?y?|\\brocks?y?\\b") & 
        str_detect(lithology, "cl\\S*ys?e?y?|si?lts?") ~ "till",
      # If it is compact/cemented clay or silts, sand and gravel
      str_detect(lithology, "consol|compact|cement|hard|binder|tight|pa?cked") &
        str_detect(lithology, "cl\\S*ys?|si?lts?") &
        str_detect(lithology, "sa?na?ds?|gra?ve?l?|\\bcob(ble)?\\W?|pebb?\\W?|bo?u?lde?r|float") ~ "till",
      # Compacted claysilts
      str_detect(lithology, "consol|compact|cement|hard|binder|tight|packed") & 
        str_detect(lithology, "(cl\\S*ys?)") &
        str_detect(lithology, "si?lts?") ~ "till",
      # Glacial and clay
      str_detect(lithology, "glacial") &
        str_detect(lithology, "cl\\S*ys?") ~ "till",
      TRUE ~ matclass
    ))
  
  # clay/sand
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # Just two or three words and they are clay and sand
      str_detect(lithology, "sa?na?ds?y?\\b") & 
        str_detect(lithology, "cl\\S*ys?e?y?\\b") & 
        str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,2}$") ~ "clay/sand",
      # sand with clays and none of the other stuff
      str_detect(lithology, "sa?na?ds?e?y?") &
        str_detect(lithology, "cl\\S*ys?e?y?") &
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") &
        str_detect(lithology, "^((?!gra?ve?l?\\S*y?|\\bsto?ne?y?s?|si?lts?e?y?|till).)*$") ~ "clay/sand",
      TRUE ~ matclass
    ))
  
  # clay/silt
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # Just clay & silt and only 2 or 3 words
      str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,2}$") ~ "clay/silt",
      # clays and silts and none of the other stuff
      str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") &
        str_detect(lithology, "^((?!gra?ve?l?\\S*y?|\\bsto?ne?y?s?|sa?na?ds?e?y?|till).)*$") ~ "clay/silt",
      TRUE ~ matclass
    ))
  
  # CLAY/SILT/sand
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # clay and silt and seams of sand
      str_detect(lithology, "cla?y?s?\\b") &
        str_detect(lithology, "si?lts?\\b") &
        str_detect(lithology, "(with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin|seams?|lens?|string(er)?).*(sa?na?ds?)\\b|(sa?na?ds?)\\b.*(with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin|seams?|lens?|string(er)?)") &
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") &
        str_detect(lithology, "^((?!gra?ve?l?\\S*y?|\\bsto?ne?y?s?|sa?na?ds?e?y?|till).)*$") ~ "CLAY/SILT/sand",
      # clay/silt and sandy
      str_detect(lithology, "cla?y?s?\\b") &
        str_detect(lithology, "si?lts?\\b") &
        str_detect(lithology, "sa?na?de?y") & 
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") &
        str_detect(lithology, "^((?!gra?ve?l?\\S*y?|\\bsto?ne?y?s?|sa?na?ds?e?y?|till).)*$") ~ "CLAY/SILT/sand",
      TRUE ~ matclass
    ))
  
  # silt/sand
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # Just sand & silt and only 2 or 3 words
      str_detect(lithology, "sa?na?ds?") &
        str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,2}$") ~ "silt/sand",
      # # sand and silt and none of the other stuff
      str_detect(lithology, "sa?na?ds?") &
        str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?|cl\\S*ys?|gra?ve?l?\\S*y?|\\bsto?ne?y?s?|cl\\S*ys?|till).)*$") ~ "silt/sand",
      TRUE ~ matclass
    ))
  
  # silt/gravel
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # gravel with silt and only 2 or 3 words
      str_detect(lithology, "gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "si?lts?") & 
        str_detect(lithology, "^[A-Za-z]+\\s*\\S*(\\s*\\S*[A-Za-z]+){0,2}$") ~ "silt/gravel",
      # gravel with silt and none of the other non-coarse stuff
      str_detect(lithology, "gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "si?lts?") & 
        str_detect(lithology, "^((?!cl\\S*ys?|sa?na?ds?y?|till).)*$") ~ "silt/gravel",
      TRUE ~ matclass
    ))
  
  # SAND/GRAVEL/silt
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # Sand/gravel/stones with silt seams and no other materials
      str_detect(lithology, "sa?na?ds?\\b|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "((si?lts?|di?rty?).*(seams?|lens?|string(er)?|with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin))|((seams?|lens?|string(er)?|with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin).*(si?lts?|di?rty?))") & 
        str_detect(lithology, "^((?!free|cl\\S*ys?|till).)*$") ~ "SAND/GRAVEL/silt",
      # sand/gravel/stones and silty
      str_detect(lithology, "sa?na?ds?\\b|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "si?lte?y") &
        str_detect(lithology, "^((?!free|cl\\S*ys?|till).)*$") ~ "SAND/GRAVEL/silt",
      TRUE ~ matclass
    ))
  
  # SAND/GRAVEL/clay
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # sand/gravel/stones and seams of clay
      str_detect(lithology, "sa?na?ds?\\b|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "((seams?|lens?|string(er)?|with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin).*cl\\S*ys?)|cl\\S*ys?.*(seams?|lens?|string(er)?|with|\\bw|\\bw\\/|&|\\band|some|minor|small|little|thin)") & 
        str_detect(lithology, "^((?!si?lts?|till).)*$") ~ "SAND/GRAVEL/clay",
      # sand/gravel/stones and clayey
      str_detect(lithology, "sa?na?ds?\\b|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") &
        str_detect(lithology, "cla?ye?y") &
        str_detect(lithology, "^((?!si?lts?|till).)*$") ~ "SAND/GRAVEL/clay",
      TRUE ~ matclass
    ))
  
  # SILT/sand/gravel
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # slight gravel or sand indications and silt
      str_detect(lithology, "((&|wi?th|\\bw|\\bw/|&|\\band|some|minor|small|little|thin|seams?|lens?|string(er)?).*(gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?)|(sa?na?ds?))|((gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?)|(sa?na?ds?).*(&|wi?th|\\bw|\\bw/|&|\\band|some|minor|small|little|thin|seams?|lens?|string(er)?))") & 
        str_detect(lithology, "si?lts?") ~ "SILT/sand/gravel",
      # silt with some sandy/gravelly
      str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^((?!free|occasional).)*$") &
        str_detect(lithology, "gra?ve?l?\\S*y|sa?na?de?y\\b|\\bsto?ne?e?y|floate?y|\\bcob(bl)?e?y|pebb?\\S*y?") ~ "SILT/sand/gravel",
      TRUE ~ matclass
    ))
  
  # silt
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # silt and one word
      str_detect(lithology, "si?lts?") & 
        str_detect(lithology, "^([A-Za-z]+\\s*\\S*)$") ~ "silt",
      # Clayey/sandy silt
      str_detect(lithology, "si?lts?") &
        str_detect(lithology, "(cl\\S*ye?y|sa?na?de?y)") ~ "silt",
      # silts and no other things
      str_detect(lithology, "si?lts?") &
        str_detect(lithology, "^((?!sa?na?ds?|gra?ve?l?|\\bsto?ne?s?).)*$") &
        str_detect(lithology, "^((?!\\brocks?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") &
        str_detect(lithology, "^((?!cl\\S*ys?).)*$") ~ "silt",
      TRUE ~ matclass
    ))
  
  # clay
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # clay and one words
      str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "^([A-Za-z]+\\s*\\S*)$") ~ "clay",
      # silty/sandy clays
      str_detect(lithology, "cl\\S*ys?") &
        str_detect(lithology, "(si?lte?y|sa?na?de?y)") ~ "silt",
      # clay and not sand, stones, gravel, rocks or boulders
      str_detect(lithology, "cl\\S*ys?e?y?") &
        str_detect(lithology, "^((?!sa?na?ds?y?|till|si?lts?|gra?ve?l?\\S*y?|\\bsto?ne?y?s?).)*$") &
        str_detect(lithology, "^((?!\\brocks?y?\\b|bo?u?lde?r|float|\\bcob(ble)?\\W?|pebb?\\W?).)*$") ~ "clay",
      # gumbo???
      str_detect(lithology, "gumbo") ~ "clay",
      TRUE ~ matclass
    ))
  
  # sand/gravel
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # If it just two words and they are sand or gravel
      str_detect(lithology, "sa?na?ds?\\b") & 
        str_detect(lithology, "gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "^(\\s*\\S*[A-Za-z]+\\s*\\S*){0,2}$") ~ "sand/gravel",
      
      # If the words sand or gravel exists, but not clay or silt and their
      # respective variants or bentonite
      str_detect(lithology, "sa?na?ds?\\b|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "^((?!cl\\S*ys?e?y?).)*$") & 
        str_detect(lithology, "^((?!si?lts?y?\\b$).)*$") & 
        str_detect(lithology, "((?!be?nto?nite?).)*$")~ "sand/gravel",
      
      # Gravel/stone/rocks but not clay silt or binder - (what about boulder???)
      str_detect(lithology, "gra?ve?l?\\S*|\\bsto?ne?s?|\\bro?cks?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "^((?!cl\\S*ys?e?y?).)*$") & 
        str_detect(lithology, "^((?!si?lts?y?).)*$") & 
        str_detect(lithology, "^((?!bi?nde?r).)*$") ~ "sand/gravel",
      
      # Sand or gravel and silt/clay free
      str_detect(lithology, "sa?na?ds?|gra?ve?l?|\\bsto?ne?s?|float|\\bcob(ble)?\\W?|pebb?\\W?") & 
        str_detect(lithology, "(no\\s*(cl\\S*ys?e?y?|si?lts?y?))|((cl\\S*ys?e?y?|si?lts?y?)\\s*free)") ~ "sand/gravel",
      TRUE ~ matclass
    ))
  
  # fill/soil/organic
  df <- df |> 
    mutate(matclass = case_when(
      !is.na(matclass) ~ matclass,
      # soil/earth/dirt/organic/peat/muck etc
      str_detect(lithology, "wood|rot|fill|alluv|loam|soil|earth|dirty?|organic|peat|muskeg|muck|mud|alluv") ~ "fill/soil/organic",
      TRUE ~ matclass
    ))
  
  # unknown overburden
  df <- df |> 
    mutate(matclass = case_when(
      # If any already exist, just leaving them
      !is.na(matclass) ~ matclass,
      # If the well has indeed been dug or contains indications of the words
      # overburden/surficial/aggregated
      str_detect(lithology, "dug|pit|overb\\S+n|surfic|aggreg|ovrbur|overbu|o.b\\b|.ver.*b\\S+den") ~ "overburden",
      TRUE ~ matclass
    ))
  
  # Unknown
  df <- df |> 
    mutate(matclass = case_when(
      # For any matclasses not yet classified, looking to see if they've been
      # classified using the noregex features and if so copying those
      # classifications
      is.na(matclass) & !is.na(matclass_noregex) ~ matclass_noregex,
      # Otherwise labelling them as unknown
      is.na(matclass) ~ "unknown",
      TRUE ~ matclass
    ))
  return(df)
}
