# !usr/bin/env Rscript

# Author: Saeesh Mangwani 
# Date: 2020-09-10 

# Description: A script containing functions which call the regex parsing
# functions on the bedrock dataset to extract fracture and yield data and
# organize it into a table

# ==== Loading libraries ====
library(stringi)

# ==== Initializing Global Variables ====

# Creating a global variable named error_wtns that stores the tag numbers for
# wells that throw an error in the classification process
error_wells <- as_tibble(list("wtn" = character(), "error_comment" = character()))

# ==== Data Extraction and Table Organization Function (Highest Level) ====

# A function that takes a dataframe, goes through all of its lithology comments
# and adds values and rows using data extraction functions where applicable
extract_litho_data <- function(df){
  # Print the wtn being worked on
  # print(df$wtn[1])
  # Creating an empty out_table to store the resulting data
  # out_table <- df[0,] %>% mutate_all(as.character)
  out_table <- df
  
  # Iterating through all the rows in the passed dataframe
  for(row in rows(df)){
    print(row$record_index)
    #checking for frac-yield pairs
    tryCatch({
      pairs <- getYieldFracPairs(row$lithology)
    },error = function(e){
      message <- paste("Error in extracting fracture-yield pairs for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = as.character(row$wtn[1]), "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If pairs were present, remove the detected string from the string being
    # considered
    temp <- ifelse(
      nrow(pairs) > 0, 
      str_remove_all(row$lithology, str_replace_all(paste(pairs$string, collapse = "|"), "\\(", "\\\\(")), 
      row$lithology
    )
    
    # For fractures
    tryCatch({
      fractures <- getFracVals(temp)
    },error = function(e){
      message <- paste("Error in extracting fracture information for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = as.character(row$wtn[1]), "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If fractures were present, removing them from the string being considered
    temp <- ifelse(
      nrow(fractures) > 0, 
      str_remove_all(temp, str_replace_all(paste(fractures$string, collapse = "|"), "\\(", "\\\\(")), 
      temp
    )
    # Having now removed any strings that were used to identify fractures or
    # yield value pairs, we finally check for yields alone from the remnant
    # string
    tryCatch({
      yields <- getYieldVals(temp)
    },error = function(e){
      message <- paste("Error in extracting yield information for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = as.character(row$wtn[1]), "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If none of the three sorts of matches are found, moving on
    if(all(is.na(pairs)) & all(is.na(fractures)) & all(is.na(yields))){
      print("nothing found")
      next
    }
    
    # If there exist any fractures that were found
    tryCatch({
      if(nrow(fractures) > 0){
        print("checking fractures")
        # Iterating through all the fractures 
        for(frac in rows(fractures)){
          # Does the frac depth found lie within the depth range associated with
          # any row in this table (whose fracture columns are also empty)
          rowmatch <- find_depth_range(frac$depth, out_table, fracEmpty = T) |> 
            mutate(across(everything(), as.character))
          # If there is an empty row match:
          if(nrow(rowmatch) > 0){
            # Getting the index of the matched row to edit its values
            editidx <- rowmatch$record_index
            # Edit the matched row to add the fracture values at the correct place
            rowmatch$fracture_from <- frac$depth
            rowmatch$fracture_to <- if_else(is.na(frac$depth2) | (frac$depth2 == ""), 
                                       frac$depth, frac$depth2)
            # Adding a comment indiciating that this row contains only fracture
            # information
            rowmatch$fracture_type <- "fracture"
            # updating the row in the output table
            out_table[out_table$record_index %in% editidx,] <- rowmatch
          }else{
            # Otherwise, creating a new row for this information
            row2 <- row
            # Setting some values to NA as it doesn't make sense to have these
            # copied over
            row2$record_index <- NA_character_
            # Adding relevant values
            row2$depth_from <- frac$depth
            row2$depth_to <- if_else(is.na(frac$depth2) | frac$depth2 == "", 
                                     frac$depth, frac$depth2)
            row2$fracture_from <- frac$depth
            row2$fracture_to <- if_else(is.na(frac$depth2) | frac$depth2 == "", 
                                        frac$depth, frac$depth2)
            # A comment to highlight the fact that this is a new addition
            row2$lithology <- "row added to account for fracture. No row-specific comment"
            # Adding a comment indiciating that this row contains only fracture
            # information
            row2$fracture_type <- "fracture"
            # Appending this row to the out_table, to be added to the dataframe
            # later
            row2 <- map(row2, as.character)
            out_table <- bind_rows(out_table, row2)
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for fractures", df$wtn[1]))
      error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for fractures")
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    
    # If there exist any pairs that were found
    tryCatch({
      if(nrow(pairs) > 0){
        print("checking pairs")
        # Iterating through all the pairs discovered
        for(pair in rows(pairs)){
          # Does the pair depth found lie within the depth range associated with
          # any row in this table (whose fracture columns are also empty)
          rowmatch <- find_depth_range(pair$depth, out_table, fracEmpty = T) |> 
            mutate(across(everything(), as.character))
          
          # If there is a row match, and it is empty:
          if(nrow(rowmatch) > 0 & !all(is.na(rowmatch))){
            # Getting the index of the matched row to edit its values
            editidx <- rowmatch$record_index
            # Edit the matched row to add the fracture/yield values at the correct place
            rowmatch$fracture_from <- pair$depth
            rowmatch$fracture_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), 
                                            pair$depth, pair$depth2)
            rowmatch$single_frac_yield <- pair$yield
            rowmatch$unit <- pair$yield_unit1
            # Adding a comment indiciating that this row contains fracture and
            # yield information
            rowmatch$fracture_type <-  "fracture/yield"
            # updating the row in the output table
            out_table[out_table$record_index %in% editidx,] <- rowmatch
          }else{
            row2 <- row
            # Setting some values to NA as it doesn't make sense to have these
            # copied over
            row2$record_index <- NA_character_
            # Adding relevant values
            row2$depth_from <- pair$depth
            row2$depth_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
            row2$fracture_from <- pair$depth
            row2$fracture_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
            row2$single_frac_yield <- pair$yield
            row2$single_frac_yield2 = if_else(is.na(pair$yield2) | (pair$yield2 == ""), pair$yield, pair$yield2)
            row2$unit <- pair$yield_unit1
            # A comment to highlight the fact that this is a new addition
            row2$lithology <- "row added to account for fracture. No row-specific comment"
            # Adding a comment indiciating that this row contains fracture and yield information
            row2$fracture_type <- "fracture/yield"
            
            # Appending this row to the out_table, to be added to the dataframe later
            row2 <- map(row2, as.character)
            out_table <- bind_rows(row2, out_table)
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for pairs", df$wtn[1]))
      error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for pairs")
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If a yield value is found, checking to see if values already exist in the
    # row. If not, adding it to the row. If yield values exist, creating a new
    # row to store it as a cumulative, rather than a single yield. If multiple
    # values are found, the function returns the word "review" instead of a
    # value itself.
    tryCatch({
      if( !is.na(yields$yield) ){
        print("checking yields")
        # If the current row's yield value is empty, overwriting that value
        if(is.na(row$single_frac_yield)){
          # Current rows record id
          editidx <- row$record_index
          # Current row's assigned fracture type
          curr_ftype <-  out_table[out_table$record_index %in% editidx,]$fracture_type
          # Setting to yield if there is nothing presently there
          out_table[out_table$record_index %in% editidx,]$fracture_type <- case_when(
            is.na(curr_ftype) ~ 'yield', 
            curr_ftype == 'fracture' ~ 'fracture/yield',
            T ~ curr_ftype
          )
          out_table[out_table$record_index %in% editidx,]$single_frac_yield <-  yields$yield
          out_table[out_table$record_index %in% editidx,]$single_frac_yield2 <-  yields$yield2
          out_table[out_table$record_index %in% editidx,]$unit <-  yields$unit
        # Otherwise, if the current row already has a yield value, creating a
        # new row to add
        }else{
          row2 <- row
          # Setting some values to NA as it doesn't make sense to have these
          # copied over
          row2$record_index <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the
          # whole well
          row2$depth_from <- "0"
          row2$depth_to <- as.character(max(make_numeric(out_table$depth_to), na.rm = T))
          row2$single_frac_yield <- yields$yield
          row2$unit <- yields$yield_unit
          # A comment to highlight the fact that this is a new addition
          row2$lithology <- "row added to account for a detected well yield. No row-specific comment"
          # Adding a comment indiciating that this row contains only yield
          # information
          row2$fracture_type <- "yield"
          # Appending the row to the out_table
          row2 <- map(row2, as.character)
          out_table <- bind_rows(row2, out_table)
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for yields", df$wtn[1]))
      error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for yields")
      error_wells <<- bind_rows(error_wells, error_row)
    })
  }
  return(out_table %>% 
           arrange(make_numeric(depth_from), make_numeric(depth_to), make_numeric(record_index)))
}

# A function that takes a dataframe, goes through the general comment associated
# with the wtn whose data is in the dataframe and uses data extraction functions
# to add rows/values where applicable
extract_comment_data <- function(df){
  print(df$wtn[1])
  # Ensuring that all columns are characters
  df <- df %>% mutate_all(as.character)
  # Since there is only one comment associated with all the rows, extracting it
  comment <- df$general_remarks[1]
  
  # Getting pair information
  tryCatch({
    pairs <- getYieldFracPairs(comment)
  },error = function(e){
    message <- paste("Error in extracting fracture-yield pairs from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If pairs were present, remove the detected string from the string being
  # considered
  temp <- ifelse(
    nrow(pairs) > 0, 
    str_remove_all(comment, str_replace_all(paste(pairs$string, collapse = "|"), "\\(", "\\\\(")), 
    comment
  )
  
  # Getting fracture information
  tryCatch({
    fractures <- getFracVals(comment)
  },error = function(e){
    message <- paste("Error in extracting fracture information from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If fractures were present, removing them from the string being considered
  temp <- ifelse(
    nrow(fractures) > 0, 
    str_remove_all(temp, str_replace_all(paste(fractures$string, collapse = "|"), "\\(", "\\\\(")), 
    temp
  )

  # Having now removed any strings that were used to identify fractures or yield
  # value pairs, we finally check for yields alone from the remnant string
  tryCatch({
    yields <- getYieldVals(temp)
  },error = function(e){
    message <- paste("Error in extracting yield information from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" =  as.character(df$wtn[1]), "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If nothing is detected, returning the table as-is
  if(all(is.na(pairs)) & all(is.na(fractures)) & all(is.na(yields))){
    print("nothing found")
    return(df %>% 
             arrange(make_numeric(depth_from), make_numeric(depth_to), make_numeric(record_index)))
  }
  
  # If fractures are detected
  tryCatch({
    if(nrow(fractures)> 0){
      print("checking fractures")
      # iterating through the fractures
      for(frac in rows(fractures)){
        # Does the pair depth found lie within the depth range associated with
        # any row in this table (whose fracture columns are also empty)
        rowmatch <- find_depth_range(frac$depth, df, fracEmpty = T) |> 
          mutate(across(everything(), as.character))
        # If the found fractures are containined by a row already within the
        # table, and there are no fracture values already associated with the
        # row for this range, editing this row by adding the fracture values
        if(nrow(rowmatch) > 0){
          # Getting the index of the matched row to edit its values
          editidx <- rowmatch$record_index
          # Edit the matched row to add the fracture/yield values at the correct place
          rowmatch$fracture_from <- frac$depth
          rowmatch$fracture_to <- if_else(is.na(frac$depth2) | (frac$depth2 == ""), 
                                          frac$depth, frac$depth2)
          # Adding a comment indiciating that this row contains fracture and
          # yield information
          rowmatch$fracture_type <-  "fracture"
          # updating the row in the output table
          df[df$record_index %in% editidx,] <- rowmatch
        # Otherwise, creating a new row
        }else{
          # Copying any row and setting some values to NA since we don't
          # actually have info about them as well as setting its depth range to
          # just the range of the fracture
          row <- df[1, ]
          # Setting some values to NA as it doesn't make sense to have these copied over
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$matclass <- NA_character_
          row$record_index <- NA_character_
          row$single_frac_yield <-NA_character_
          row$single_frac_yield2 <- NA_character_
          row$cum_frac_yield <- NA_character_
          row$unit <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row$depth_from <- frac$depth
          row$depth_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          row$fracture_from <- frac$depth
          row$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          row$fracture_type <- "fracture"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for fractures", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for fractures")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If pairs are detected
  tryCatch({
    if(nrow(pairs)> 0){
      print("checking pairs")
      # iterating through the pairs
      for(pair in rows(pairs)){
        # Does the pair lie within any of the existing lithology ranges and the
        # associated fracture column for this row is empty or exactly the same
        rowmatch <- find_depth_range(pair$depth, df, fracEmpty = T) |> 
          mutate(across(everything(), as.character))
        # If the found pairs contain data that are already present in the table,
        # skipping this pair
        if(check_pair_row_match(pair, df)){
          next
        # Otherwise, using the matched row if present
        }else if(nrow(rowmatch) > 0){
          # Getting the index of the matched row to edit its values
          editidx <- rowmatch$record_index
          # Edit the matched row to add the fracture/yield values at the correct place
          rowmatch$fracture_from <- pair$depth
          rowmatch$fracture_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), 
                                          pair$depth, pair$depth2)
          # Adding a comment indiciating that this row contains fracture and
          # yield information
          rowmatch$single_frac_yield <- pair$yield
          rowmatch$single_frac_yield2 <-if_else(is.na(pair$depth2) | (pair$depth2 == ""), 
                                                pair$yield, pair$yield2)
          rowmatch$unit <-  pair$yield_unit1
          rowmatch$fracture_type <-  "fracture/yield"
          # updating the row in the output table
          df[df$record_index %in% editidx,] <- rowmatch
          # Otherwise, creating a new row
        }else{
          # Copying any row and setting some values to NA since we don't
          # actually have info about them as well as setting its depth range to
          # just the range of the fracture
          row <- df[1, ]
          # Setting some values to NA as it doesn't make sense to have these
          # copied over
          row$record_index <- NA_character_
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$matclass <- NA_character_
          row$cum_frac_yield <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the
          # whole well
          row$depth_from <- pair$depth
          row$depth_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          row$fracture_from <- pair$depth
          row$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          row$single_frac_yield <- pair$yield
          row$single_frac_yield2 <- if_else(is.na(pair$yield2), pair$yield, pair$yield2)
          row$unit <- pair$yield_unit1
          row$fracture_type <- "fracture/yield"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for pairs", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for pairs")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If yields are detected - for the general comment we assume that it refers to
  # the cumulative yield (too unpredictable to assign to any single one)
  tryCatch({
    if(!is.na(yields$yield)){
      print("checking yields")
      # Selecting the last row in the dataframe
      df <- arrange(df, make_numeric(depth_from))
      last <- df[nrow(df),]
      # Updating the cumulative yield value in this row, with the value
      # extracted
      last$cum_frac_yield <- yields$yield
      last$unit <- ifelse(is.na(yields$unit), last$unit, yields$unit)
      last$fracture_type <- if_else(is.na(last$fracture_from), "yield", "fracture/yield")
      # Updating the table with this new row
      df[nrow(df),] <- last
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for yields", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for yields")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # Return the manipulated table
  return(df %>% arrange(make_numeric(depth_from), make_numeric(depth_to), make_numeric(record_index)))
}


# ==== Regular Expression Parsing Functions (Mid-level helpers) ====

# Construct the list defining fracture/yield regex expressions:
getRegexPatterns <- function(){
  # Basic expressions ----------
  
  # Numeric pattern - to capture integers, decimals and fractions
  num <- '(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)'
  # Captures units of yield
  yUnits <- '(gpm|gph|gpd|usgpm|ukgpm|gal\\.?p?\\.?m?)'
  # Captures units indicating a fracture
  fUnits <- "(fe{0,2}t|')"
  # Standard connector (connectors indicating something is yield/frac pair)
  connector <- '\\s?,?\\s?(@|at|in|w|wi?th?|-+|\\s?\\()\\s?,?\\s?'
  
  # Intermediate expressions (assist in yield/frac extraction) ----------
  
  # Captures a numeric range (like 25-35, or 1 1/2 to 1.75)
  numRange <- paste0('\\s?(?:', num, '(?:\\s?(-|to)\\s?', num, ')?)\\s?')
  
  # Captures a numeric range with possible interjecting units specific to yield
  yieldNumRange <- paste0(num, '\\s?(?:', yUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?')
  
  # Captures a numeric range with possible interjecting units specific to fracture depths
  fracNumRange <- paste0(num, '\\s?(?:', fUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?')
  
  # Complex expressions (capture different types of yield/frac text data):
  
  # Yield to fracture pattern
  yieldToFrac <- paste0(yieldNumRange, yUnits, connector, fracNumRange, fUnits)
  
  # Fracture to yield pattern
  fracToYield <- paste0(fracNumRange, fUnits, connector, yieldNumRange, yUnits)
  
  # Returning patterns
  list(
    'num' = num,
    'yUnits' = yUnits,
    'fUnits' = fUnits,
    'connector' = connector,
    'numRange' = numRange,
    'yieldNumRange' = yieldNumRange,
    'fracNumRange' = fracNumRange,
    'yieldToFrac' = yieldToFrac,
    'fracToYield' = fracToYield
  )
}

# Attempt 2 at the getYieldFracPairs function
getYieldFracPairs <- function(comment){
  # Getting regex patterns as a list
  p <- getRegexPatterns()
  # Matching both types of patterns to the comment, and restructuring to a named
  # dataframe
  ytf <- str_match_all(comment, p$yieldToFrac) |> 
    as.data.frame() |> 
    setNames(c('string', 
               'yield', 'yunit1', 'ycnct', 'yield2', 'yunit2', 
               'cnct', 
               'depth', 'funit1', 'dcnct', 'depth2', 'funit2'))
  fty <-  str_match_all(comment, p$fracToYield) |> 
    as.data.frame() |> 
    # Note the inverted row names
    setNames(c('string', 
               'depth', 'funit1', 'dcnct', 'depth2', 'funit2',
               'cnct', 
               'yield', 'yunit1', 'ycnct', 'yield2', 'yunit2'))
  # Joining to create a single dataframe (with the rows named, this puts all the
  # correct info together)
  pairs <- bind_rows(ytf, fty)
  
  # If no rows are found, returning an empty table
  if(nrow(pairs) == 0){
    out <- pairs |> 
      dplyr::select(string, yield, yield2, 'yield_unit' = yunit2, depth, depth2)
    return(out)
  }
  
  # Synthesizing the yield unit (don't do depth units because it is always feet)
  pairs <- pairs |> 
    # Defaulting to the main one matched, unless it is missing
    mutate(yield_unit = ifelse(is.na(yunit2), yunit1, yunit2)) |> 
    # Only relevant columns
    dplyr::select(string, yield, yield2, yield_unit, depth, depth2) |>
    distinct()
  return(pairs)
}

# A function that applies regular expression matching to return a vector of
# yield values extracted from a given comment
getYieldVals <- function(comment){
  # Looking for yield values using the yield-only regex. First looking for the
  # world "yield" and if found splitting the string on that word to only take
  # the latter half of the comment
  pattern <- "((?:pu?mp|yi?e?ld)[A-Za-z _\\.,:\\-]{0,10})?(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)+\\s*(&|,|-|and|to)?\\s*(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)?\\s*(gpm|gph|gpd|usgpm|ukgpm)(?![\\s\\S]*\\d+\\s?(?:ho?u?r|min))"
  
  # An empty dataframe to store the output of this function
  output <- tibble(yield = NA_character_, yield2 = NA_character_, unit = NA_character_, .rows = 1)
  
  # Using the pattern to extract matches
  yields <- str_match_all(comment, pattern)[[1]] |> 
    as.data.frame()
  
  # If no values are found or the values are all missing, returning an empty
  # dataframe
  if(all(is.na(yields))) return(output) 
  
  # If the word pump or heave or any associated values is located in any row,
  # removing that row from the row of yields
  yields <- yields |> 
    # Removing rows where all data points are missing
    filter(!if_all(everything(), is.na)) |> 
    # Removing rows where the words pump or heave are detected
    filter(is.na(V2) | !str_detect(V2, "(p\\.l\\.?)|(pump(?:ing)?\\D*)|(?:heav(?:ing)?\\D*)"))
  
  # With the above filter complete, checking again if the table is empty and
  # returning an empty dataframe if yes
  if(all(is.na(yields))) return(output) 
  
  # Returning only the largest yield measure found (ideally there will be just
  # one, or they're all equal) 
  output$yield <- max(make_numeric(yields$V3), na.rm=T) %>%
    ifelse(is.infinite(.), NA_character_, .)
  output$yield2 <- max(make_numeric(yields$V5), na.rm=T) %>%
    suppressWarnings()  %>%
    ifelse(is.infinite(.), NA_character_, .)
  output$unit <- unique(yields$V6) |> na.omit() %>%
    ifelse(length(.)==0, NA_character_, .)
  return(output)
}

# A function that applies regular expression matching to return a tibble of
# fracture values extracted from a given comment
getFracVals <- function(comment){
  # Looking for fracture values using the fracture-only regex. This is the first
  # step of a multi part process
  pattern <- "((be?dro?ck|[fg]rac\\w*|seam|moisture|h2[o0]|w\\.?b|(?:pump(?:\\w|\\s)+)?(water|wet\\s?spot,?))(?:[\\w\\s&,-\\/#]){0,50}(:|at|@|-|\\s|from)?)\\s*(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)\\s?(ft|feet|gpm|gph|gpd|usgpm|ukgpm|')?\\s?((&|,|-|and|to|at|with)\\s?(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)\\s?(ft|feet|'|gpm|gph|gpd|usgpm|ukgpm)?)*(\\\\|\"|st|nd|rd|th|psi)?"
  fractures <- str_extract_all(comment, pattern)[[1]]
  # Replacing an h2o with water (numbers get read poorly)
  fractures <- str_replace_all(fractures, 'h2[o0]', 'water')
  # If the expression above captures anything, then proceeding with the rest of
  # the steps. Otherwise ignoring and proceeding to the return statement
  if ( (sum(!is.na(fractures)) > 0) & (length(fractures) > 0) ){
    
    # Removing any captured expressions that contain the word "pump" or related,
    # as this refers to pumping depth not fracture depth and we don't want that.
    # Creating a temp var to assist with this process
    temp <- c()
    for(i in seq(fractures)){
      # If the word pump is detected, skipping this row
      if(str_detect(fractures[i], "pu?mp")){
        next
        # If words related to pressure are detected
      }else if(str_detect(fractures[i], "#|pre?ssu?re|psi|ppm|gpg")){
        # Checking for and extracing only the numerics related to depth (not
        # pressure or psi)
        extract <- str_match_all(fractures[i], "(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)\\s*(ft|feet|')")[[1]]
        # If there are no appropriate numerics, skipping this row. Otherwise
        # saving only the correct numerics back into the temp var.
        if(nrow(extract) == 0) next else temp <- append(temp, paste("frac @ ", extract[,1], collapse = ", "))
        # If there are no issues, simply adding the row to the temp table If the
        # word increasing is detected and no yield units are given, the  we
        # assume this to be a yield only and drop it
      }else if(str_detect(fractures[i], "incre?a?s") & !str_detect(fractures[i], "ft|feet|'")){
        next
        # If the statement fractures with any of these symbols, it is discarded
      }else if(str_detect(fractures[i], "(\\\\|\"|st|nd|rd|th|psi|\\/)$")){
        next
      }else{
        temp <- append(temp, fractures[i])
      }
    }
    # Passing the now filtered expressions back into the fracs variable
    fractures <- temp
    # Collapsing them into one string separated by spaces
    fractures <- paste(fractures, collapse = ' ')
    
    # Now using second regex to extract the actual numeric values from the
    # character strings extracted. Note that it ignores any values followed by
    # yield units
    fractures <- str_match_all(fractures, "(?<!#)(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)+\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?(?:\\s*(-|to)\\s*(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)?\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?)?")[[1]]
    # Turning this into a named dataframe
    fractures <- suppressMessages(as_tibble(fractures,.name_repair = "unique"))
    names(fractures) <- c("string", "depth", "to", "depth2")
    
    # If there are any rows that were detect, checking the units associated with
    # any numeric values captured and removing any rows where the units are
    # yield (instead of depth) related
    if(nrow(fractures) > 0){
      fractures <- fractures %>% 
        mutate(depth = make_numeric(depth)) %>% 
        mutate(depth2 = make_numeric(depth2)) %>% 
        # Removing any rows where the numeric threw an error.
        filter(!is.na(depth)) %>% 
        filter(!str_detect(string, "gpm|gph|gpd|usgpm|ukgpm"))
    }
  }else{
    # If there are no fractures detected, returning an empty named dataframe using the same structure as the one that would be created above
    fractures <- tibble(string = character(), depth = character(), to = character(), depth2 = character(), .rows = 0)
    return(fractures)
  }
  # If the length of the resulting dataframe is now 0 after all the filtering,
  # or if the corrected numerics are returned as invalid, returning it empty
  if (nrow(fractures) == 0){
    fractures <- tibble(string = character(), depth = character(), to = character(), depth2 = character(), .rows = 0)
    return(fractures)
  }
  # Otherwise formatting and returning the fractures
  return(fractures %>% mutate_all(as.character) %>% mutate(string = str_squish(string)) %>% distinct() %>% arrange(make_numeric(depth)))
}



# ==== Helper Functions (Lowest Level) ====

# Function to help extract and iterate through the rows of a dataframe
rows <-  function(df) {
  lapply(seq_len(nrow(df)), function(i) unclass(df[i,,drop=F]))
}

# A helper function to select the right unit when reassigning a yield unit. It checks for whether units have otherwise been reported for other yield
# values and if so, uses the most common reported unit. Otherwise, it just uses the default unit
select_unit <- function(unit_vector, depth = F){
  # If there are no units reported, returning gpm. Otherwise returning the unit most frequently reported
  output <- suppressWarnings(case_when(
    !isTRUE(depth) & is.na(max(unit_vector, na.rm = T)) ~ "gpm", 
    !isTRUE(depth) ~ max(unit_vector, na.rm = T),
    isTRUE(depth) & is.na(max(unit_vector, na.rm = T)) ~ "ft", 
    isTRUE(depth) ~ max(unit_vector, na.rm = T)
  ))
  return(output)
}

# Function that takes a dataframe and completes its cumulative and single yield columns
fill_yield_vals <- function(df){
  # Arranging the dataframe in the right order and turning the yield columns to numeric
  df <- df %>% arrange(make_numeric(record_index), make_numeric(depth_from), make_numeric(depth_to)) %>% 
    mutate(single_frac_yield = make_numeric(single_frac_yield)) %>% 
    mutate(single_frac_yield2 = make_numeric(single_frac_yield2)) %>% 
    mutate(cum_frac_yield = make_numeric(cum_frac_yield))
  
  # A variable that stores the present cumulative yield and single yield value
  curr_cumulative <- 0
  curr_single <- 0
  
  # Iterating through all the rows in the dataframe
  df_rows <- rows(df)
  for (i in seq(df_rows)){
    # Updating first the value of the current single. If there is either a single_frac_yield or a single_frac_value2 present for this, it is assigned as the current single
    curr_single <- case_when(!is.na(df_rows[[i]]$single_frac_yield2) ~ df_rows[[i]]$single_frac_yield2,
                             !is.na(df_rows[[i]]$single_frac_yield) ~ df_rows[[i]]$single_frac_yield,
                             # If there is no single value, but there is a cumulative value, the current single is the difference between the
                             # cumulative on this row and the cumulative stored so far
                             is.na(df_rows[[i]]$single_frac_yield) & !is.na(df_rows[[i]]$cum_frac_yield) ~ (df_rows[[i]]$cum_frac_yield - curr_cumulative),
                             # if neither are present, the current single is 0
                             is.na(df_rows[[i]]$single_frac_yield) & is.na(df_rows[[i]]$cum_frac_yield) ~ 0)
    # Next updating the current cumulative. If neither balue is present, the value is not updated as we assume a 0 addition to the cumulative yield on
    # this row
    curr_cumulative <- case_when(is.na(df_rows[[i]]$single_frac_yield) & is.na(df_rows[[i]]$cum_frac_yield) ~ curr_cumulative,
                                 # If there is no single value, but there is a current cumulative value, the new cumulative value simply
                                 # replaces/updates the old one
                                 is.na(df_rows[[i]]$single_frac_yield) & !is.na(df_rows[[i]]$cum_frac_yield) ~ df_rows[[i]]$cum_frac_yield,
                                 #If one of the single values are present, the sum of that plus the cumulative stored so far is the current
                                 # cumulative
                                 !is.na(df_rows[[i]]$single_frac_yield2) ~ (curr_cumulative + df_rows[[i]]$single_frac_yield2),
                                 !is.na(df_rows[[i]]$single_frac_yield) ~ (curr_cumulative + df_rows[[i]]$single_frac_yield))
    
    # With the values now updated, overwriting the values stored in the row with the clarified values
    df_rows[[i]]$cum_frac_yield <- curr_cumulative
    df_rows[[i]]$single_frac_yield <- ifelse(is.na(df_rows[[i]]$single_frac_yield), curr_single, df_rows[[i]]$single_frac_yield)
    df_rows[[i]]$single_frac_yield2 <- ifelse(is.na(df_rows[[i]]$single_frac_yield2), curr_single, df_rows[[i]]$single_frac_yield2)
  }
  out_table <- bind_rows(df_rows)
  return(out_table)
}

# A function that checks whether the data from a frac yield pair is already present within a row in the df
check_pair_row_match <- function(pair, df){
  depth <- pair$depth
  depth2 <- ifelse(is.na(pair$depth2), pair$depth, pair$depth2)
  yield <- pair$yield
  yield2 <- ifelse(is.na(pair$yield2), pair$yield, pair$yield2)
  
  # building a boolean index that indiciates if there is any row where all these values are the same and returning it
  index <- (df$depth_from %in% depth) & (df$depth_to %in% depth2) & (df$single_frac_yield %in% yield) & (df$single_frac_yield2 %in% yield2)
  return(any(index))
}

# A function that takes the depth of an inputted depth and returns the row with
# the smallest from_depth within whose depth range it falls, with an option to
# also filter for those rows where the fracture data are empty
find_depth_range <- function(depth, df, fracEmpty=T){
  # Ensuring all values are numeric
  df$depth_from <- make_numeric(df$depth_from)
  df$depth_to <- make_numeric(df$depth_to)
  depth <- make_numeric(depth)
  
  # Filtering out only those rows where the inputted depth is >= the from_depth
  # AND is <= the to_depth
  temp <- df[depth >= df$depth_from,]
  temp <- temp[depth <= temp$depth_to,]
  
  # If there are rows in which meet this condition:
  if(nrow(temp) > 0){
    # Filtering only those whose fracture data is still empty if requested
    if(fracEmpty) temp <- temp[is.na(temp$fracture_from),]
    # If there is more than one appropriate row remaining, selecting the one
    # with the lowest from_depth.
    temp <- temp[temp$depth_from == suppressWarnings(min(temp$depth_from)),]
  }
  # Returning the row
  return(temp)
}

# A function that checks for the connector used to extablish the relationship between depth and yield in a pair. It checks whether the connector for
# the present row is the same as the most common. It returns a boolean value
is_common_connector <- function(i, pairs){
  # If there is no single type of connector that is predominant, returning FALSE as then this test is meaningless
  if(all(table(str_squish(pairs[,8])) == 1)){
    return(FALSE)
  }else{
    # Otherwise, checking to see if the connector at the current row is not
    # equal to the most common connector. If it is the same, returning T,
    # otherwise F
    return(str_squish(pairs[i,8]) == names(sort(table(str_squish(pairs[,8])), decreasing = T)[1]))
  }
}

# A helper function for converting to numeric that also easily converts mixed
# fractions, which is sometimes how values are reported in the data
make_numeric <- function(x){
  x <- sub(' ', '+', x, fixed=TRUE)
  tryCatch({
    return(as.numeric(unlist(lapply(x, function(x) eval(parse(text=x))))))
  },error = function(e){
    return(NA_real_)
  })
}
