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
  currwtn <- as.character(df$wtn[1])
  print(currwtn)
  # Making relevant columns numeric
  df <- df |> 
    mutate(across(matches('depth|yield|fracture_(from|to)'), make_numeric))
  # Creating an editable copy to export
  out_table <- df
  
  # Iterating through all the rows in the passed dataframe
  for(rid in df$record_index){
    # print(rid)
    lithoComment <- df |> filter(record_index==rid) |> pull(lithology)
    #checking for frac-yield pairs
    tryCatch({
      pairs <- getYieldFracPairs(lithoComment)
    },error = function(e){
      print(rid)
      message <- paste("Error in extracting fracture-yield pairs at rowid", rid, "from the lithology comment")
      print(message)
      error_row <- list("wtn" = currwtn, "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If pairs were present, remove the detected string from the string being
    # considered
    temp <- ifelse(
      nrow(pairs) > 0, 
      str_remove_all(lithoComment, str_replace_all(paste(pairs$string, collapse = "|"), "\\(", "\\\\(")), 
      lithoComment
    )
    
    # For fractures
    tryCatch({
      fractures <- getFracVals(temp)
    },error = function(e){
      message <- paste("Error in extracting fractures at rowid", rid, "from the lithology comment")
      print(message)
      error_row <- list("wtn" = currwtn, "error_comment" = message)
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
      message <- paste("Error in extracting yields at rowid", rid, "from the lithology comment")
      print(message)
      error_row <- list("wtn" = currwtn, "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If none of the three sorts of matches are found, moving on to the next row
    if(all(is.na(pairs)) & all(is.na(fractures)) & all(is.na(yields))){
      print("nothing found")
      next
    }
    
    # If there exist any pairs that were found
    tryCatch({
      if(nrow(pairs) > 0){
        print("checking pairs")
        # Iterating through all the pairs discovered
        for(pair in rows(pairs)){
          # Does the pair depth found lie within the depth range associated with
          # any row in this table (whose fracture columns are also empty)
          rowmatch <- find_depth_range(pair$depth, out_table, fracEmpty = T)
          
          # If there is a row match, and it is empty:
          if(nrow(rowmatch) > 0){
            # Getting the index of the matched row to edit its values
            editidx <- rowmatch$record_index
            # Edit the matched row to add the fracture/yield values at the
            # correct place
            rowmatch$fracture_from <- pair$depth
            rowmatch$fracture_to <- if_else(is.na(pair$depth2), 
                                            pair$depth, pair$depth2)
            rowmatch$single_frac_yield <- pair$yield
            rowmatch$single_frac_yield2 <- pair$yield2
            rowmatch$unit <- pair$yunit
            # Adding a comment indiciating that this row contains fracture and
            # yield information
            rowmatch$fracture_type <-  "fracture/yield"
            # updating the row in the output table
            out_table[out_table$record_index %in% editidx,] <- rowmatch
          }else{
            # Creating a new row as a copy of the current row
            newrow <- rows(df)[(df$record_index %in% rid)][[1]]
            # Setting some values to NA as it doesn't make sense to have these
            # copied over
            newrow$record_index <- '999999999'
            # Adding/Editing relevant values
            newrow$depth_from <- pair$depth
            newrow$depth_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
            newrow$fracture_from <- pair$depth
            newrow$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
            newrow$single_frac_yield <- pair$yield
            newrow$single_frac_yield2 = pair$yield2
            newrow$unit <- pair$yunit
            # A comment to highlight the fact that this is a new addition
            newrow$lithology <- "row added to account for fracture. No row-specific comment"
            # Adding a comment indiciating that this row contains fracture and yield information
            newrow$fracture_type <- "fracture/yield"
            
            # Appending this row to the out_table, to be added to the dataframe later
            out_table <- bind_rows(newrow, out_table)
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for pairs", df$wtn[1]))
      error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for pairs")
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If there exist any fractures that were found
    tryCatch({
      if(nrow(fractures) > 0){
        print("checking fractures")
        # Iterating through all the fractures 
        for(frac in rows(fractures)){
          # Does the frac depth found lie within the depth range associated with
          # any row in this table (whose fracture columns are also empty)
          rowmatch <- find_depth_range(frac$depth, out_table, fracEmpty = T)
          # If there is an empty row match:
          if(nrow(rowmatch) > 0){
            # Getting the index of the matched row to edit its values
            editidx <- rowmatch$record_index
            # Edit the matched row to add the fracture values at the correct place
            rowmatch$fracture_from <- frac$depth
            rowmatch$fracture_to <- if_else(is.na(frac$depth2), 
                                            frac$depth, frac$depth2)
            # Adding a comment indiciating that this row contains only fracture
            # information
            rowmatch$fracture_type <- "fracture"
            # updating the row in the output table
            out_table[out_table$record_index %in% editidx,] <- rowmatch
          }else{
            # Creating a new row as a copy of the current row
            newrow <- rows(df)[(df$record_index %in% rid)][[1]]
            # Setting some values to NA as it doesn't make sense to have these
            # copied over
            newrow$record_index <- '999999999'
            # Adding relevant values
            newrow$depth_from <- frac$depth
            newrow$depth_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
            newrow$fracture_from <- frac$depth
            newrow$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
            # A comment to highlight the fact that this is a new addition
            newrow$lithology <- "row added to account for fracture. No row-specific comment"
            # Adding a comment indiciating that this row contains only fracture
            # information
            newrow$fracture_type <- "fracture"
            # Appending this row to the out_table, to be added to the dataframe
            # later
            out_table <- bind_rows(out_table, newrow)
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for fractures", df$wtn[1]))
      error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for fractures")
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If a yield value is found, checking to see if values already exist in the
    # row. If not, adding it to the row. If yield values exist, creating a new
    # row to store it as a cumulative, rather than a single yield. If multiple
    # values are found, the function returns the word "review" instead of a
    # value itself.
    tryCatch({
      if(nrow(yields) > 0){
        print("checking yields")
        # Getting only the current row (only this one can be edited if
        # necessary, since we don't know which depth this yield value is
        # associated with)
        rowmatch <- out_table |> filter(record_index == rid)
        # Current single frac yield
        curr_yield <- rowmatch$single_frac_yield
        # Current fracture type
        curr_ftype <- rowmatch$fracture_type
        # If the current yield is empty, overwriting it
        if(is.na(curr_yield)){
          rowmatch$single_frac_yield <- yields$yield1
          rowmatch$single_frac_yield2 <- yields$yield2
          rowmatch$unit <- yields$yunit
          # Fracture type depends on what is already there
          rowmatch$fracture_type <- case_when(
            is.na(curr_ftype) ~ 'yield', 
            # If it is a fracture, making it a fracture/yield
            curr_ftype == 'fracture' ~ 'fracture/yield',
            T ~ curr_ftype
          )
          
          # Updating row in the out-table
          out_table[out_table$record_index == rid,] <- rowmatch
          # Otherwise, if the current row already has a yield value, creating a
          # new row to add
        }else{
          # Creating a new row as a copy of the current row
          newrow <- rows(df)[(df$record_index %in% rid)][[1]]
          # Setting some values to NA as it doesn't make sense to have these
          # copied over
          newrow$record_index <- '999999999'
          # Adding relevant values. Setting the depth range to the depth of the
          # whole well
          newrow$depth_from <- 0
          newrow$depth_to <- max(out_table$depth_to, na.rm = T)
          newrow$single_frac_yield <- yields$yield1
          newrow$single_frac_yield2 <- yields$yield2
          newrow$unit <- yields$yunit
          # A comment to highlight the fact that this is a new addition
          newrow$lithology <- "row added to account for a detected well yield. No row-specific comment"
          # Adding a comment indiciating that this row contains only yield
          # information
          newrow$fracture_type <- "yield"
          # Appending the row to the out_table
          out_table <- bind_rows(newrow, out_table)
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
           arrange(depth_from, depth_to, record_index))
}

# A function that takes a dataframe, goes through the general comment associated
# with the wtn whose data is in the dataframe and uses data extraction functions
# to add rows/values where applicable
extract_comment_data <- function(df){
  currwtn <- as.character(df$wtn[1])
  print(currwtn)
  # Making relevant columns numeric
  df <- df |> 
    mutate(across(matches('depth|yield|fracture_(from|to)'), make_numeric))
  # Creating an editable copy to export
  # out_table <- df
  # Since there is only one comment associated with all the rows, extracting it
  comment <- df$general_remarks[1]
  
  # Getting pair information
  tryCatch({
    pairs <- getYieldFracPairs(comment)
  },error = function(e){
    message <- paste("Error in extracting fracture-yield pairs at well", currwtn, "from general remarks")
    print(message)
    error_row <- list("wtn" = currwtn, "error_comment" = message)
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
    fractures <- getFracVals(temp)
  },error = function(e){
    message <- paste("Error in extracting fractures at well", currwtn, "from general remarks")
    print(message)
    error_row <- list("wtn" = currwtn, "error_comment" = message)
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
    message <- paste("Error in extracting yields at well", currwtn, "from general remarks")
    print(message)
    error_row <- list("wtn" = currwtn, "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If nothing is detected, returning the table as-is
  if(all(is.na(pairs)) & all(is.na(fractures)) & all(is.na(yields))){
    print("nothing found")
    return(df %>% arrange(depth_from, depth_to, record_index))
  }
  
  # If pairs are detected
  tryCatch({
    if(nrow(pairs)> 0){
      print("checking pairs")
      # iterating through the pairs
      for(pair in rows(pairs)){
        # Does the pair depth found lie within the depth range associated with
        # any row in this table (whose fracture columns are also empty)
        rowmatch <- find_depth_range(pair$depth, df, fracEmpty = T)
        # If there is a row match, and it is empty:
        if (nrow(rowmatch) > 0){
          # Getting the index of the matched row to edit its values
          editidx <- rowmatch$record_index
          # Edit the matched row to add the fracture/yield values at the correct place
          rowmatch$fracture_from <- pair$depth
          rowmatch$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          # Adding a comment indiciating that this row contains fracture and
          # yield information
          rowmatch$single_frac_yield <- pair$yield
          rowmatch$single_frac_yield2 <- pair$yield2
          rowmatch$unit <- pair$yunit
          rowmatch$fracture_type <-  "fracture/yield"
          # updating the row in the output table
          df[df$record_index == editidx,] <- rowmatch
          # Otherwise, creating a new row
        }else{
          # Copying any row and setting some values to NA since we don't
          # actually have info about them as well as setting its depth range to
          # just the range of the fracture
          newrow <- rows(df)[[1]]
          # Setting some values to NA as it doesn't make sense to have these
          # copied over
          newrow$record_index <- '999999999'
          newrow$lithology <- "row added to account for fracture. No row-specific comment"
          newrow$matclass <- NA_character_
          newrow$cum_frac_yield <- NA_real_
          # Adding relevant values. Setting the depth range to the depth of the
          # whole well
          newrow$depth_from <- pair$depth
          newrow$depth_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          newrow$fracture_from <- pair$depth
          newrow$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          newrow$single_frac_yield <- pair$yield
          newrow$single_frac_yield2 <- pair$yield2
          newrow$unit <- pair$yunit
          newrow$fracture_type <- "fracture/yield"
          df <- bind_rows(newrow, df)
        }
      }
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for pairs", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for pairs")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If fractures are detected
  tryCatch({
    if(nrow(fractures)> 0){
      print("checking fractures")
      # iterating through the fractures
      for(frac in rows(fractures)){
        # Does the pair depth found lie within the depth range associated with
        # any row in this table (whose fracture columns are also empty)
        rowmatch <- find_depth_range(frac$depth, df, fracEmpty = T)
        # If the found fractures are containined by a row already within the
        # table, and there are no fracture values already associated with the
        # row for this range, editing this row by adding the fracture values
        if(nrow(rowmatch) > 0){
          # Getting the index of the matched row to edit its values
          editidx <- rowmatch$record_index
          # Edit the matched row to add the fracture values at the correct place
          rowmatch$fracture_from <- frac$depth
          rowmatch$fracture_to <- if_else(is.na(frac$depth2), 
                                          frac$depth, frac$depth2)
          # Adding a comment indiciating that this row contains only fracture
          # information
          rowmatch$fracture_type <- "fracture"
          # updating the row in the output table
          df[na.omit(df$record_index) == editidx,] <- rowmatch
        # Otherwise, creating a new row
        }else{
          # Copying any row and setting some values to NA since we don't
          # actually have info about them as well as setting its depth range to
          # just the range of the fracture
          newrow <- rows(df)[[1]]
          # Setting some values to NA as it doesn't make sense to have these copied over
          newrow$lithology <- "row added to account for fracture. No row-specific comment"
          newrow$matclass <- NA_character_
          newrow$record_index <- '999999999'
          newrow$single_frac_yield <- NA_real_
          newrow$single_frac_yield2 <- NA_real_
          newrow$cum_frac_yield <- NA_real_
          newrow$unit <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          newrow$depth_from <- frac$depth
          newrow$depth_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          newrow$fracture_from <- frac$depth
          newrow$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          newrow$fracture_type <- "fracture"
          df <- bind_rows(newrow, df)
        }
      }
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for fractures", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for fractures")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If yields are detected - for the general comment we assume that it refers to
  # the cumulative yield (too unpredictable to assign to any single one)
  tryCatch({
    if(nrow(yields)> 0){
      print("checking yields")
      # Selecting the last row in the dataframe
      df <- arrange(df, depth_from)
      last <- df[nrow(df),]
      # Updating the cumulative yield value in this row, with the value
      # extracted
      last$cum_frac_yield <- yields$yield1
      last$unit <- ifelse(is.na(yields$yunit), last$unit, yields$yunit)
      # last$fracture_type <- if_else(is.na(last$fracture_from), "yield", "fracture/yield")
      # Updating the table with this row
      df[nrow(df),] <- last
    }
  },
  error = function(e){
    print(paste("Error in processing row while looking for yields", df$wtn[1]))
    error_row <- list("wtn" = as.character(df$wtn[1]), "error_comment" = "Error in processing row while looking for yields")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # Return the manipulated table
  return(df %>% arrange(depth_from, depth_to, record_index))
}


# ==== Regular Expression Parsing Functions (Mid-level helpers) ====

# Helper function to construct and deliver all the regex patterns used for
# matching. Separated here to have a standardized framework for regex parsing,
# to make editing and readability easier. 
getRegexPatterns <- function(){
  # Basic/helper expressions ----------
  
  # Numeric pattern - to capture integers, decimals and fractions
  num <- '(\\.?\\d+(?:\\s?\\d+\\/\\d+)?\\.?\\/?\\d*)'
  # Captures units of yield
  yUnits <- '(gpm|gph|gpd|usgpm|ukgpm|gal\\.?p?\\.?m?)'
  # Captures units indicating a fracture (depth)
  fUnits <- "(fe{0,2}t|')"
  # Standard connector (connectors indicating something is yield/frac pair)
  connector <- '(@|at|in|w|wi?th?|-+|\\s?\\()'
  # Range connector (specifically for numeric ranges)
  rangeConnector <- "(-+|to)"
  # Sequence connector (specifically for sequential numeric values)
  seqConnector <- "(\\s?and\\s?|\\s?&\\s?|,\\s?|;\\s?|\\s)"
  # Fracture indication keywords
  fracIndics <- '(gpm?h?d?|flo?w|[fg]rac\\s?#?\\s?\\w{0,10}|seam|moisture|h2[o0]|w\\.?b|wa?te?r|wet\\s?spo?t,?)'
  # Yield indication keywords
  yieldIndics <- '((?:yi?e?ld|app?ro?x|p\\.l\\.?|pu?mp(?:ing)?|heaving)[A-Za-z _\\.,:\\-]{0,20})'

  # Query expressions 1. - Frac/Yield Pairs:
  
  # Yield to fracture pattern
  yieldToFrac <- paste0(
    #  Captures a numeric range with possible interjecting units specific to
    #  yield
    paste0(num, '\\s?(?:', yUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?'), 
    yUnits, 
    '\\s?,?\\s?', connector, '\\s?,?\\s?',
    # Captures a numeric range with possible interjecting units specific to
    # fracture depths
    paste0(num, '\\s?(?:', fUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?'), 
    fUnits
  )
  
  # Fracture to yield pattern - same as above but inverted numeric ranges being
  # detected
  fracToYield <- paste0(
    paste0(num, '\\s?(?:', fUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?'), 
    fUnits, 
    '\\s?,?\\s?', connector, '\\s?,?\\s?',
    paste0(num, '\\s?(?:', yUnits, '?\\s?(-|to)\\s?', num, '\\s?)?\\s?'), 
    yUnits
  )
  
  # Query expressions 2 - Fractures
  
  # Fractures that are recorded as a range (12-18 ft)
  fracSeqs <- paste0(
    # Fracture indices and a fracture indication connector
    fracIndics, '\\s?(:|at|@|-|from|\\s)\\s?', 
    # capture a number or a numeric range (optional) specific to fracture depths
    '(?:', num, '\\s?', fUnits, '?(?:\\s?', rangeConnector, '\\s?', num, '\\s?', fUnits, '?)?)',
    # Additionally capture other numberic ranges as above provided in sequence
    # (as many as present)
    '(?:', seqConnector, 
    '\\s?', num, '\\s?', fUnits, '?(?:\\s?', rangeConnector, '\\s?', num, '\\s?', fUnits, '?)?)*\\s?',
    # Escape group for yield units (plus pressure units), to make sure this only
    # gets fractures
    '(?:\\s?', str_replace(yUnits, '\\)$', '|psi)'), '?)'
  )
  
  # Query expressions 3 - Yields
  yieldVals <- paste0(
    yieldIndics, '?', 
    # capture a number or a numeric range (optional) specific to yield units
    # (must end with a yield unit to be valid, hard condition)
    '(?:', num, '\\s?', yUnits, '?(?:\\s?', rangeConnector, '\\s?', num, '\\s?)?)', yUnits
    # '(?:', num, '\\s?', yUnits, '?(?:\\s?', rangeConnector, '\\s?', num, '\\s?', yUnits, '?)?)\\s?',
    # Exclusion group to remove yields units associated with a rate (these are
    # pumping effort, not actual yield) as well as any numeric ranges associated
    # with a depth
    # "(?!\\s?\\d*(?:(?:[\\s\\S]{0,10}(?:ho?u?r|mins?))|(?:", fUnits, ")))"
  )
  
  # Returning patterns
  list(
    'num' = num,
    'yUnits' = yUnits,
    'fUnits' = fUnits,
    'connector' = connector,
    'yieldToFrac' = yieldToFrac,
    'fracToYield' = fracToYield,
    'fracSeqs' = fracSeqs,
    'yieldVals' = yieldVals
  )
}

# Extracts string columns containing yield/fracture pairs, a common pattern in
# the gwells data and the most specific and interpretable form
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
      dplyr::select(string, yield, yield2, 'yunit' = yunit2, depth, depth2)
    return(out)
  }
  
  # Synthesizing the yield unit (don't do depth units because it is always feet)
  pairs <- pairs |> 
    # Defaulting to the main one matched, unless it is missing
    mutate(yunit = ifelse(is.na(yunit2), yunit1, yunit2)) |> 
    # Only relevant columns
    dplyr::select(string, yield, yield2, yunit, depth, depth2) |>
    # Converting relevant columns to numeric
    mutate(across(matches('yield|depth'), make_numeric)) |> 
    # Arranging by depth
    arrange(depth, yield) |> 
    # Getting only unique values based on the captured data columns
    distinct(pick(-string), .keep_all = T)
  return(pairs)
}

# Extracts individual values, ranges, or sequences that likely indicate
# fractures, using specific fracture keywords to detect the right numeric
# sequences. Most effective after sub-strings associated with yield/frac pairs
# have been removed
getFracVals <- function(comment){
  # Getting regex patterns
  p <- getRegexPatterns()
  
  # Extracting fracture-strings from the comment
  matches <- str_match_all(comment, pattern = p$fracSeqs) |> 
    as.data.frame()
  
  match_strings <- matches |> 
    # If the last column is not missing it contains a yield unit match, so
    # dropping this match (isn't a fracture)
    filter(is.na(X15)) |> 
    # Getting only the matched strings (this table can't effectively separate
    # numbers when there are multiple)
    pull(X1)
  
  # If there are no match strings, returning an empty table
  if(length(match_strings) == 0){
    return(
      tibble(
        'string'=character(0),
        'numString'=character(0), 
        'depth'=numeric(0), 
        'depth2'=numeric(0)
      )
    )
  }
  
  # Removing any yield units from the match strings (don't want these to be
  # deleted from the raw string after detection, so that single yield values can
  # be identified in the next step)
  match_strings <- str_squish(str_remove_all(match_strings, p$yUnits))
  
  # Pattern for capturing numeric ranges for fracture units
  numRange <- paste0('(?<!\\s?#\\s?)(?:', p$num, '\\s?', p$fUnits, 
                     '?(?:\\s?(-|to)\\s?', 
                     p$num, '\\s?', p$fUnits, '?)?)\\s?')
  # Extracting numbers from detected strings, to get all identified fractures
  # (extracts either single numbers or ranges if present)
  depthTabs <- match_strings |> 
    # Replacing any "h2o" with water prior to matching
    str_replace_all('h2[o0]', 'water') |> 
    str_match_all(numRange)
  
  # Formatting to a finalized fracture table
  fracs <- map2_dfr(depthTabs, match_strings, \(x, y){
    as.data.frame(x) |> 
      setNames(c('numString', 'depth', 'dunit1', 'to', 'depth2', 'dunit2')) |> 
      mutate(string = y, .before='numString') |> 
      # Making captured numbers numeric
      mutate(across(contains('depth'), make_numeric)) |> 
      # Returning only relevant columns
      dplyr::select(string, numString, depth, depth2)
  })
  return(fracs)
}

# Extracts individual values or ranges that indicate yield. Most effective after
# sub-strings associated with yield/frac pairs and fractures have been removed
getYieldVals <- function(comment){
  # Getting regex patterns
  p <- getRegexPatterns()
  
  # Extracting Yield-strings from the comment
  yields <- str_match_all(comment, pattern = p$yieldVals) |> 
    as.data.frame() |> 
    setNames(c('string', 'yIndic', 'yield1', 'yunit2', 'to', 'yield2', 'yunit'))
  
  # If the yield indicator contains any pumping or heaving keywords, removing
  # these matches as they aren't yield but pumping details (this works even for
  # a 0-length table)
  yields <- yields |> 
    filter(is.na(yIndic) | !str_detect(yIndic, 'p\\.l\\.?|pu?mp(?:ing)?|heaving'))
  
  # If no rows are found, returning an empty table
  if(nrow(yields) == 0){
    out <- yields |> 
      dplyr::select(string, yield1, yield2, yunit)
    return(out)
  }
  # Synthesizing the yield unit (don't do depth units because it is always feet)
  yields <- yields |> 
    # Defaulting to the main one matched, unless it is missing
    mutate(yunit = ifelse(is.na(yunit), yunit2, yunit)) |> 
    # Only relevant columns
    dplyr::select(string, yield1, yield2, yunit) |>
    # Converting relevant columns to numeric
    mutate(across(contains('yield'), make_numeric)) |> 
    # Getting only unique values based on the captured data columns
    distinct(pick(-string), .keep_all = T)
  
  # There should only be one matched for any given comment. If there are
  # multiple, returning the largest one (assuming this is the cumulative yield)
  if(nrow(yields) > 1){
    print("Multiple yields found")
    print(yields$string)
    yields <- yields |> filter(yield1 == max(make_numeric(yield1)))
  }
  return(yields)
}

# Testing
# test <- read_delim('test.txt', delim = '\n', col_names = F)
# comment <- test$X1
# comment <- sample(litho$lithology, 10)
# res <- map(comment, getYieldFracPairs)
# idx <- which(map(res, nrow) > 0)
# res[idx]
# comment[idx]
# res <- Filter(\(x)nrow(x) > 0, resFull)

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
