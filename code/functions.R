################################# FUNCTIONS ##################################
# This script stores the functions used in the app 
##############################################################################

<<<<<<< HEAD
=======
############################################################################################################################################################
# App Content functions
# Author: Dionne Argyropoulos
############################################################################################################################################################

###############################################################################
# renderDetailsList function
# --------------------------
#
# This function makes the table in a Fluent UI format. 
#
# PARAMETERS: 
#   - DATA FRAME: any processed data frame
#
# OUTPUT:
#   - Table
###############################################################################

renderDetailsList <- function(df) {
  div(
    class = "ms-Grid-row",
    div(
      class = "ms-Grid-col ms-sm12",  # Use ms-sm12 for full width on small screens
      Stack(
        tokens = list(childrenGap = 10),
        horizontal = TRUE,
        div(
          style = "max-height: 600px; overflow: auto; width: 100%;",
          DetailsList(
            items = df,
            columns = tibble(fieldName = names(df), name = names(df)),
            constrainMode = 0,
            checkboxVisibility = 2,
            styles = list(
              root = list(
                width = "100%",  # Ensure table width is constrained within the available space
                minWidth = "fit-content",  # Allow table to grow to fit content
                overflowX = "auto"  # Enable horizontal scrolling only when necessary
              )
            )
          )
        )
      )
    )
  )
}

###############################################################################
# fluent_two_cols function
# --------------------------
#
# This function creates two columns in the Fluent UI format.
#
# PARAMETERS: 
#   - first_col: A list of content for the first column
#   - second_col: A list of content for the second column
#   - first_width: Percent width of the column space (default: 50%)
#   - second_width: Percent width of the column space (default: 50%)
#
# OUTPUT:
#   - Two columns 
###############################################################################

fluent_two_cols <- function(
    first_col, 
    second_col, 
    first_width = "50%", 
    second_width = "50%"
) {
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 40),
    children = list(
      # First Column
      div(
        tokens = list(childrenGap = 15),
        style = list(width = first_width),
        children = first_col  # First column content
      ), 
      # Second Column
      div(
        tokens = list(childrenGap = 15),
        style = list(width = second_width),
        children = second_col  # Second column content
      )
    )
  )
}


##############################################################################
# makeCard function
# --------------------------
#
# This function imports the makes a card following the Fluent UI format. 
#
# PARAMETERS: 
#   - title: String with the large title that will be printed in the card
#   - id: Identifying tag for use to link 
#   - content: A list of content to be rendered
#   - size: A value from 1 to 12 of the width of the screen (default = 12)
#   - style: Value for any css styling (reactive)
#
# OUTPUT:
#   - A "card" in the Fluent UI format with content. 
##############################################################################

makeCard <- function(title, id, content, size = 12, style = "") {
  div(
    id = id,
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(padding = 20, childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

############################################################################################################################################################
# Data processing and classification functions
# Author: Dionne Argyropoulos, Shazia Ruybal-Pesantez, Lauren Smith, Eamon Conway, Connie Li Wai Suen
############################################################################################################################################################

##############################################################################
# euro_csv_read: Custom Read European CSV 
# --------------------------
#
# Description: 
# This function reads European CSV format where the deliminter is ";" and 
# decimal points are ",". This is a helper function inside `readSeroData`. 
# 
# Usage: euro_csv_read(raw_data, filter_start, platform) 
# 
# Arguments: 
#   - raw_data: String with the raw data path (reactive).
#   - filter_start: String to filter the df as the start row.
#   - filter_stop: String to filter the df as the final row. 
#
# Output:
#   - df: Rows that correspond to the MFI, counts or raw data. 
# 
# Author: Dionne Argyropoulos
##############################################################################

euro_csv_read <- function(raw_data, filter_start, filter_stop) {
  
  # Read lines from the raw input
  lines <- readLines(raw_data, encoding = "UTF-8")
  
  # Find start and end positions
  start_line <- grep(filter_start, lines)
  end_line <- grep(filter_stop, lines)
  if (length(end_line) == 0) end_line <- length(lines) + 1
  
  # Extract relevant lines between start and stop
  if(filter_start == "Program"){
    data_lines <- lines[(start_line):(end_line - 1)]
  } else {
    data_lines <- lines[(start_line + 1):(end_line - 1)]
  }
  data_lines <- data_lines[nzchar(data_lines)]  # remove empty lines
  
  # Split each line by semicolon, clean quotes, convert comma decimals
  clean_lines <- lapply(
    str_split(data_lines, ";"),
    function(row) {
      row <- str_replace_all(row, '"', "")
      row <- str_replace_all(row, "^(\\d+),(\\d+)$", "\\1.\\2")
      row
    }
  )
  
  # Extract headers and pad data rows
  headers <- clean_lines[[1]]
  data_rows <- clean_lines[-1]
  max_cols <- length(headers)
  data_rows <- lapply(data_rows, function(row) {
    length(row) <- max_cols
    row
  })
  
  # Build the data frame
  df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  colnames(df) <- headers
  
  # Replace junk string with NA and drop fully NA rows
  df[df == ",,,,,,,,,,"] <- NA
  df <- df[rowSums(is.na(df)) < ncol(df), ]
  colnames(df) <- sub(",+$", "", colnames(df)) # Remove trailing commas from column names
  df[] <- lapply(df, function(col) {
    col <- str_remove(col, ",+$")  # Removes trailing commas using stringr
    col
  })
  return(df)
}

>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
##############################################################################
# check_platform function: Check Platform 
# --------------------------
#
# Description: 
# This function checks the platform the user has input and whether it aligns
# with the correct format as expected. Will report error if NOT aligned. 
# 
# Usage: check_platform(raw_data, raw_data_filenames, platform) 
# 
# Arguments: 
#   - raw_data: String with the raw data path (reactive).
<<<<<<< HEAD
#   - raw_data_filenames: String with the raw data filenames (reactive).
=======
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
#   - platform: "magpix" or "bioplex" (reactive).
#
# Output:
#   - TRUE: if platform == file format
#   - ERROR message when platform != file format 
# 
# Author: Dionne Argyropoulos
##############################################################################

<<<<<<< HEAD
=======
check_platform <- function(raw_data, platform) {
  
  if (length(raw_data) == 0) {
    stop("No raw data files were provided.")
  }
  
  file_extension <- tools::file_ext(raw_data)  # Identify the file extension and read the file accordingly
  
  if (file_extension == "xlsx") {
    df <- suppressMessages(readxl::read_excel(raw_data, n_max = 5))
  } else if (file_extension == "csv") {
    df <- suppressMessages(readr::read_csv(raw_data, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE))
  }
  
  # Extract the first two column names
  col_names <- colnames(df)
  if (all(grepl("^X\\d+$", col_names))) {
    df <- suppressWarnings(df %>% row_to_names(row_number = 1))
  }
  first_two_cols <- colnames(df)[1:2]
  
  # Detect if the file is Magpix based on column names
  is_magpix <- any(grepl("Program", first_two_cols, ignore.case = TRUE)) || 
    any(grepl("xPonent", first_two_cols, ignore.case = TRUE))
  
  # User selected "magpix" but the file does not have "Program" or "xPonent"
  if (platform == "magpix" && !is_magpix) {
    stop(paste("Error: The file", file_name, "does not appear to be a 'magpix' file, but the platform was set to 'magpix'. Please check your selection."))
  }
  
  # User selected "bioplex" but the file contains "Program" or "xPonent"
  if (platform == "bioplex" && is_magpix) {          
    stop(paste("Error: The file", file_name, "appears to be a 'magpix' file, but the platform was set to 'bioplex'. Please check your selection."))
  }
  
  return(TRUE)
  
}
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b

##############################################################################
# readSeroData function: Read Serological Data
# --------------------------
#
# Description: 
# This function imports the raw data from the Magpix or Bioplex machine
# and matches the sample names from the plate layout based on their plate/well 
# location.
# 
# Usage: readSeroData(raw_data, raw_data_filenames, platform) 
# 
# Arguments: 
#   - raw_data: String with the raw data path (reactive).
#   - raw_data_filenames: String with the raw data filenames (reactive).
#   - platform: "magpix" or "bioplex" (reactive).
#
# Output:
#   - List of data frames: (i) raw data output, (ii) cleaned all results 
#   (iii) count data, (iv) blanks only, (v) standards only, (vi) run 
#   information. 
# 
# Authors: Shazia Ruybal-Pesántez, Dionne Argyropoulos
##############################################################################

readSeroData <- function(raw_data, raw_data_filenames, platform){
  
  platemap <- read.csv(here::here("data/platemap.csv"))
  
  # Initialise master list to store files 
  master_list <- list(
    data_raw  = NULL,  # Placeholder for raw data combined across files
    results   = NULL,  # Placeholder for processed results combined
    counts    = NULL,  # Placeholder for any count data combined
    blanks    = NULL,  # Placeholder for any blanks data combined
    stds      = NULL,  # Placeholder for any stds data combined
    run       = NULL   # Placeholder for any run data combined
  )
  
<<<<<<< HEAD
  check_platform <- function(raw_data, raw_data_filenames, platform) {
    
    if (length(raw_data) == 0) {
      stop("No raw data files were provided.")
    }
    
    file_extension <- tools::file_ext(file)  # Identify the file extension and read the file accordingly
    
    if (file_extension == "xlsx") {
      df <- suppressMessages(readxl::read_excel(file, n_max = 5))
    } else if (file_extension == "csv") {
      df <- suppressMessages(readr::read_csv(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE))
    }
    
    # Extract the first two column names
    col_names <- colnames(df)
    if (all(grepl("^X\\d+$", col_names))) {
      df <- suppressWarnings(df %>% row_to_names(row_number = 1))
    }
    first_two_cols <- colnames(df)[1:2]
    
    # Detect if the file is Magpix based on column names
    is_magpix <- any(grepl("Program", first_two_cols, ignore.case = TRUE)) || 
      any(grepl("xPonent", first_two_cols, ignore.case = TRUE))
    
    # User selected "magpix" but the file does not have "Program" or "xPonent"
    if (platform == "magpix" && !is_magpix) {
      stop(paste("Error: The file", file_name, "does not appear to be a 'magpix' file, but the platform was set to 'magpix'. Please check your selection."))
    }
    
    # User selected "bioplex" but the file contains "Program" or "xPonent"
    if (platform == "bioplex" && is_magpix) {          
      stop(paste("Error: The file", file_name, "appears to be a 'magpix' file, but the platform was set to 'bioplex'. Please check your selection."))
    }
    
    return(TRUE)
    
  }
  
=======
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  # Loop through each file and process accordingly
  for (i in seq_along(raw_data)) {
    file <- raw_data[i]
    file_name <- raw_data_filenames[i]
    
<<<<<<< HEAD
    if (check_platform(file, file_name, platform) == TRUE) {
=======
    if (check_platform(file, platform) == TRUE) {
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
      message("PASS: File ", file_name, " successfully validated.")
    }
    
    if (platform == "magpix") { 
      
      file_extension <- tools::file_ext(file)  # Identify the file extension and read the file accordingly
      
      if (file_extension == "xlsx") {
        full <- suppressMessages(readxl::read_excel(file))
        df <- as.data.frame(full)
        
        data_raw <- df
        
        median_row_number     <- which(df$xPONENT == "Median")
        count_row_number      <- which(df$xPONENT == "Count")
        endcount_row_number   <- which(df$xPONENT == "Avg Net MFI")
        
<<<<<<< HEAD
        results <- readxl::read_excel(file, skip = median_row_number + 1)
        counts <- readxl::read_excel(file, skip = count_row_number + 1, n_max = endcount_row_number - count_row_number - 2, col_names = TRUE)
        run <- readxl::read_excel(file, n_max = median_row_number)
=======
        results <- suppressMessages(readxl::read_excel(file, skip = median_row_number + 1))
        counts <- suppressMessages(readxl::read_excel(file, skip = count_row_number + 1, n_max = endcount_row_number - count_row_number - 2, col_names = TRUE))
        run <- suppressMessages(readxl::read_excel(file, n_max = median_row_number))
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
        
      } else if (file_extension == "csv") {
        
        first_lines <- readLines(file, n = 5)           # Read the first few lines of the file
        
<<<<<<< HEAD
        if (all(grepl(";", first_lines))) {
          # If semicolons are consistently found, use read.csv2
          full <- suppressMessages(readr::read_csv2(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE)) # Read in the data
        } else {
          full <- suppressMessages(readr::read_csv(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE)) # Read in the data
        }
        
        df <- suppressWarnings(as.data.frame(full) %>% janitor::row_to_names(row_number = 1))
        data_raw <- df
        
        median_row_number     <- which(df$xPONENT == "Median")
        endmedian_row_number  <- which(df$xPONENT == "Net MFI")
        count_row_number      <- which(df$xPONENT == "Count")
        endcount_row_number   <- which(df$xPONENT == "Avg Net MFI")
        
        results <- df[(median_row_number + 1):(endmedian_row_number - 1), ]
        colnames(results) <- results[1, ]
        results <- results[-1, ]
        results <- results[, colSums(!is.na(results)) > 0] # remove NA columns
        results <- results[rowSums(!is.na(results)) > 0, ] # remove NA rows
        rownames(results) <- NULL
        
        counts <- df[(count_row_number + 1):(endcount_row_number - 1), ]
        counts <- counts[, colSums(!is.na(counts)) > 0] # remove NA columns
        counts <- counts[rowSums(!is.na(counts)) > 0, ] # remove NA rows
        colnames(counts) <- counts[1, ]
        counts <- counts[-1, ]
        rownames(counts) <- NULL
        
        run <- df[1:median_row_number, ]
        run <- run[, colSums(!is.na(run)) > 0] # remove NA columns
        run <- run[rowSums(!is.na(run)) > 0, ] # remove NA rows
        rownames(run) <- NULL
        
=======
        if (any(grepl(";", first_lines))) { # If EUROPEAN CSV FORMAT 
          
          results <- suppressWarnings(euro_csv_read(file, "Median", "Net MFI"))
          counts <- suppressWarnings(euro_csv_read(file, 'DataType:;\"\"Count', "Avg Net MFI"))
          run <- suppressWarnings(euro_csv_read(file, "Program", "Results"))
          data_raw <- run 
          
        } else { # IF CSV FORMAT 
          full <- suppressMessages(readr::read_csv(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE)) # Read in the data
          df <- suppressWarnings(as.data.frame(full) %>% janitor::row_to_names(row_number = 1))
          data_raw <- df
          
          median_row_number     <- which(df$xPONENT == "Median")
          endmedian_row_number  <- which(df$xPONENT == "Net MFI")
          count_row_number      <- which(df$xPONENT == "Count")
          endcount_row_number   <- which(df$xPONENT == "Avg Net MFI")
          
          results <- df[(median_row_number + 1):(endmedian_row_number - 1), ]
          colnames(results) <- results[1, ]
          results <- results[-1, ]
          results <- results[, colSums(!is.na(results)) > 0] # remove NA columns
          results <- results[rowSums(!is.na(results)) > 0, ] # remove NA rows
          rownames(results) <- NULL
          
          counts <- df[(count_row_number + 1):(endcount_row_number - 1), ]
          counts <- counts[, colSums(!is.na(counts)) > 0] # remove NA columns
          counts <- counts[rowSums(!is.na(counts)) > 0, ] # remove NA rows
          colnames(counts) <- counts[1, ]
          counts <- counts[-1, ]
          rownames(counts) <- NULL
          
          run <- df[1:median_row_number, ]
          run <- run[, colSums(!is.na(run)) > 0] # remove NA columns
          run <- run[rowSums(!is.na(run)) > 0, ] # remove NA rows
          rownames(run) <- NULL
        }
        
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
      } else {
        stop("Unsupported file format! Please use .csv or .xlsx")
      }
      
      # Remove blank rows and preprocess results        
      blank_row_number <- which(rowSums(is.na(results)) == length(names(results)))[1] # Handle blank rows
      if (!is.na(blank_row_number)) {
        results <- results[1:(blank_row_number - 1), ]
      }
      
      # 2. Create results
      results <- results %>%
        dplyr::select(-dplyr::any_of("Total Events")) %>%
        dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN", 0, .))) %>% # Change "NaN" to 0s
        dplyr::mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), 
                                      ifelse(Sample == "B", paste0("Blank", row_number()), Sample))) %>% # Sequentially relabel Blank rows and keep other Sample values unchanged
        dplyr::mutate(Sample = ifelse(Sample == "S", paste0("S", cumsum(Sample == "S")), Sample)) # Sequentially relabel Sample rows and keep other Sample values 
      
      # 3. Load counts for QC 
      counts <- counts %>%
        dplyr::mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), 
                                      ifelse(Sample == "B", paste0("Blank", row_number()), Sample))) %>% # Sequentially relabel Blank rows and keep other Sample values unchanged
        dplyr::select(-any_of("Total Events"))
      counts <- tidyr::as_tibble(counts)
      
      # 4. Save blanks
      blanks <- results %>% dplyr::filter(grepl("Blank|^B$", Sample, ignore.case = TRUE))
      
      # 5. Save standards
      stds <- results %>% dplyr::filter(grepl("^S", Sample, ignore.case = TRUE))
      
      # 6. Save run info 
      run_info <- as.data.frame(run) %>% dplyr::select(Program:xPONENT)
      
      # Ensure blanks exist
      if (nrow(blanks) == 0) {
        stop("No blanks were found in the dataset. Ensure blanks are properly labeled.")
      }
      
      # Ensure standards exist
      if (nrow(stds) == 0) {
        stop("No standards were found in the dataset. Ensure standards are properly labeled.")
      }
      
      # Save the plate number for this file 
      plate_numbers <- file_name %>% stringr::str_extract("(?i)(repeat)?plate\\d+(?=[._-]|$)")
      
      # Add 'plate' column to each dataframe
      data_raw$Plate <- plate_numbers
      results$Plate <- plate_numbers
      counts$Plate <- plate_numbers
      blanks$Plate <- plate_numbers
      stds$Plate <- plate_numbers        
      run_info$Plate <- plate_numbers
      
      # Add processed file's tables to the master list
      master_list$data_raw <- suppressMessages(dplyr::bind_rows(master_list$data_raw, data_raw))   # Combine raw data
      master_list$results  <- dplyr::bind_rows(master_list$results, results)     # Combine processed results
      master_list$counts   <- dplyr::bind_rows(master_list$counts, counts)       # Combine counts
      master_list$blanks   <- dplyr::bind_rows(master_list$blanks, blanks)       # Combine blanks
      master_list$stds     <- dplyr::bind_rows(master_list$stds, stds)           # Combine stds
      master_list$run      <- dplyr::bind_rows(master_list$run, run_info)        # Combine run
      
      
    } else if (platform == "bioplex") { 
      
      file_extension <- tools::file_ext(file)     # Identify the file extension and read the file accordingly
      
      if (file_extension == "xlsx") {
        
        full <- suppressMessages(readxl::read_excel(file))
        df <- as.data.frame(full) 
        
      } else if (file_extension == "csv") {
        
        # Check for the delimiter
        first_lines <- readLines(file, n = 5)
        
        if (all(grepl(";", first_lines))) {
          full <- suppressMessages(utils::read.csv2(file))
          df <- as.data.frame(full)
        } else {
          full <- suppressMessages(utils::read.csv(file))
          df <- as.data.frame(full)
        }
        
      } else {
        stop("Unsupported file format! Please use .csv or .xlsx")
      }
      
      colnames(df)[1] <- "Run" # Renames first column name as it is named by the local computer
      data_raw <- df # Save Raw File 
      
      # 2. Create results 
      df_cleaned <- which(df[,2] == "Type")
      df <- df[df_cleaned:nrow(df), ] # Remove first few rows 
      colnames(df) <- as.character(df[1,])
      rownames(df_cleaned) <- NULL 
      df <- df[-1,]
      
      colnames(df) <- gsub("\\s*\\(.*\\)", "", colnames(df)) # Clean up column names
      df <- df %>% 
        dplyr::mutate(Type = ifelse(Type == "B", "Blank", Type), # Re-label 
                      suffix = as.numeric(gsub("\\D", "", Type)), # Order so that standards and blanks are at the top
                      prefix = substr(Type, 1, 1)) %>% # Order so that standards and blanks are at the top
        dplyr::arrange(prefix, suffix) %>% # Order so that standards and blanks are at the top 
        dplyr::left_join(platemap, by = "Well") %>%  # Join on the Well column
        dplyr::select(-c(prefix, suffix, Region, Gate, Total, `% Agg Beads`, `Sampling Errors`, Well, dplyr::any_of("Description"))) %>% # Remove unnecessary columns, including Description if it exists
        dplyr::select(Location, Sample = Type, everything()) %>% # Rename columns to be same as magpix 
        dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN", 0, .)),  # Change "NaN" to 0s
                      dplyr::across(everything(), ~ gsub("\\*\\*\\*", "0", .)), #Change "***" to 0s
                      Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), Sample)) # Sequentially relabel Blank rows and keep other Sample values unchanged
      
      results <- df %>% dplyr::mutate(dplyr::across(-c(Location, Sample), ~ gsub("\\s*\\(.*\\)", "", .)))
      
      # 3. Load counts for QC 
      counts <- df %>% dplyr::mutate(dplyr::across(-c(Location, Sample), ~ gsub(".*\\((.*)\\).*", "\\1", .)))
      
      # 4. Save blanks
      blanks <- results %>% dplyr::filter(grepl("Blank", Sample, ignore.case = TRUE))
      
      # 5. Save standards
      stds <- results %>% dplyr::filter(grepl("^S", Sample, ignore.case = TRUE))
      
      # 6. Save run info
      well_row <- which(data_raw[,1] == "Well")[1] # [1] ensures only the first occurrence
      run_info <- data_raw[1:(well_row-2), 1, drop = FALSE] # Save run info
      
      # Ensure blanks exist
      if (nrow(blanks) == 0) {
        stop("No blanks were found in the dataset. Ensure blanks are properly labeled.")
      }
      
      # Ensure standards exist
      if (nrow(stds) == 0) {
        stop("No standards were found in the dataset. Ensure standards are properly labeled.")
      }
      
      # Save the plate number for this file 
      plate_numbers <- file_name %>% stringr::str_extract("(?i)(repeat)?plate\\d+(?=[._-]|$)")
      
      # Add 'plate' column to each dataframe
      data_raw$Plate <- plate_numbers
      results$Plate <- plate_numbers
      counts$Plate <- plate_numbers
      blanks$Plate <- plate_numbers
      stds$Plate <- plate_numbers
      run_info$Plate <- plate_numbers
      
      # Stitch together for master file
      master_list$data_raw <- suppressMessages(bind_rows(master_list$data_raw, data_raw))   # Combine raw data
      master_list$results  <- bind_rows(master_list$results, results)     # Combine processed results
      master_list$counts   <- bind_rows(master_list$counts, counts)       # Combine counts
      master_list$blanks   <- bind_rows(master_list$blanks, blanks)       # Combine blanks
      master_list$stds     <- bind_rows(master_list$stds, stds)           # Combine stds
      master_list$run      <- bind_rows(master_list$run, run_info)        # Combine run
      
    } else {
      stop("Unsupported file type. Please use either Magpix or Bioplex!")
    }
  }
  
  return(master_list)
  
}

##############################################################################
# readAntigens function: Standardise Antigen Names
# --------------------------
#
# Description: 
# This function ensures that the antigens in the raw data adheres to our
# nomenclature format in the data processing and model steps.
# This function calls `readSeroData` first to read the serological raw data and
# then use our nomenclature for the eight antigens of interest in PvSeroApp. 
# 
# Useage: readAntigens(raw_data, raw_data_filenames, platform)
# 
# Arguments: 
#   - raw_data: String with the raw data path (reactive).
#   - raw_data_filenames: String with the raw data filenames (reactive).
#   - platform: "magpix" or "bioplex" (reactive).
#
# Output:
#   - List of data frames with relabelled column names for our antigen names.
# 
# Author: Dionne Argyropoulos
##############################################################################

readAntigens <- function(serodata_output){
  
  # Function to relabel column names
  relabel_columns <- function(df) {
    colnames(df) <- dplyr::case_when(
      stringr::str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
      stringr::str_detect(colnames(df), regex("LF005", ignore_case = TRUE)) ~ "LF005",
      stringr::str_detect(colnames(df), regex("LF010", ignore_case = TRUE)) ~ "LF010",
      stringr::str_detect(colnames(df), regex("LF016", ignore_case = TRUE)) ~ "LF016",
      stringr::str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
      stringr::str_detect(colnames(df), regex("(P87|RBP2b-P87)", ignore_case = TRUE)) ~ "RBP2b.P87",
      stringr::str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150",
      stringr::str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
      TRUE ~ colnames(df) # Keep unmatched names as-is
    )
    return(df)
  }
  
  # Step 1: Read and process `master_file`
  master_file <- serodata_output
  
  # Step 2: Process `master_file$results`
  results_df <- master_file$results %>%
    as.data.frame() %>%
    relabel_columns() %>%
    dplyr::select(dplyr::any_of(c("Location", "Sample",  "Plate", "EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")))
  master_file$results <- results_df
  
  # Step 3: Loop through and process specific data frames in `master_file`
  dataframes_to_process <- c("results", "counts", "blanks", "stds")
  
  master_file <- lapply(names(master_file), function(df_name) {
    if (df_name %in% dataframes_to_process) {
      master_file[[df_name]] <- relabel_columns(master_file[[df_name]])
    }
    return(master_file[[df_name]])
  }) %>% setNames(names(master_file)) # Preserve list names
  
}

##############################################################################
# readPlateLayout function: Read Plate Layout/s
# --------------------------
#
# Description: 
# This function imports the plate layout. Each sheet of the plate layout 
# ".xlsx" file must contain 13 columns (labelled
# Plate, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) (columns A-M) and 9 rows 
# (Plate, A, B, C, D, E, F, G, H) (rows 1-9). *Note that the first row/column 
# i.e., the A1 cell in excel is called "Plate". This function also checks that
# the plate sheet labels are consistent with the MAGPIX file input names, as a
# check prior to merging downstream. 
# 
# Usage: readPlateLayout(plate_layout)
#
# Arguments: 
#   - plate_layout_file: An ".xlsx" file with sheets labelled plate1, plate2... 
#     etc. (reactive).
#   - antigen_output: Output from `readAntigens` (reactive).
#
# Output:
#   - A list of data frames, with each one representing an individual plate.
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

readPlateLayout <- function(plate_layout, antigen_output) {
  
  if (is.null(plate_layout) || !file.exists(plate_layout)) {
    stop("ERROR: Invalid plate layout file provided.")
  }
  
  sheet_names <- tryCatch({
    openxlsx::getSheetNames(plate_layout)
  }, error = function(e) {
    stop("ERROR: Failed to read sheet names. Ensure the file is a valid Excel file.")
  })
  
  # Step 1: Get the sheet names to confirm
  sheet_names <- openxlsx::getSheetNames(plate_layout)
  
  # Step 2: Read all sheets into plate_layout_list using indices
  plate_layout_list <- lapply(1:length(sheet_names), function(i) {
    openxlsx::read.xlsx(plate_layout, sheet = i)
  })
  
  # Step 3: Name each element in the list after the corresponding sheet name
  names(plate_layout_list) <- sheet_names
  
  # Step 4: Check if 'Plate' column exists in antigen_output$results
  antigen_output_results <- antigen_output$results
  
  if (!"Plate" %in% colnames(antigen_output_results)) {
    stop("ERROR: 'Plate' column is missing from antigen_output$results.")
  }
  
  # Step 5: Extract levels from 'Plate' column
  antigen_output_levels <- unique(as.character(antigen_output$results$Plate))  # Convert factor to character
  
  # Step 6: Compare plate names
  if (all(antigen_output_levels %in% sheet_names)) {
    message("Plate layouts correctly identified!")
  } else {
    stop("Plate layout sheets and plates labeled in raw data file names do not match. Ensure plate sheets are correctly labeled.")
  }
  
  return(plate_layout_list)
}

##############################################################################
<<<<<<< HEAD
#' process_counts(antigen_output)
#' @description
#' A helper function to process counts data. 
#'
#' @param antigen_output Output from `readAntigens` (reactive). 
#' 
#' @return Returns a long table of counts with "Warning" category (<15 == 1 and 
#' ≥ 15 == 0) for downstream wrangling.
#' @export
#' Author: Dionne Argyropoulos
=======
# process_counts function: Process Counts from Luminex file 
# --------------------------
#
# Description: 
# A helper function to process counts data. 
#  
# Usage: process_counts(antigen_output)
# 
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#
# Output: Returns a long table of counts with "Warning" category (<15 == 1 and 
# ≥ 15 == 0) for downstream wrangling.
# 
# Authors: Dionne Argyropoulos
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
##############################################################################

process_counts <- function(antigen_output){
  
  # 1. Store Counts Data 
  counts_data <- antigen_output$counts 
  
  # 2. Data Wrangling 
  counts_data <- counts_data %>%
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>% 
    tidyr::pivot_longer(-c(Sample, Location, Plate), names_to = "Antigen", values_to = "Count") %>% 
    dplyr::mutate(Warning = case_when(
      as.numeric(Count)<15~1,
      as.numeric(Count)>=15~0
    )) 
  
  return(counts_data)
}
<<<<<<< HEAD
##############################################################################
# getCounts function: Get Count Data from Raw Median Fluescent Itensity
# --------------------------
#
# Description: 
# This function obtains the count data from the raw Median Fluescent Itensity
=======

##############################################################################
# getCounts function: Get Count Data from Raw Median Fluorescent Intensity
# --------------------------
#
# Description: 
# This function obtains the count data from the raw Median Fluorescent Intensity
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
# (MFI). This is an interim function used for the plotCounts function.
# This function relies on the `readAntigens` and `readSeroData` data processing
# functions.
#  
# Usage: getCounts(raw_data, raw_data_filenames, platform)
# 
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#
# Output:
#   - Data frame providing bead counts per well per plate.
#   - Designates whether wells should be repeated if there are ≤ 15 beads 
#     (repeat) or if they are sufficient with > 15 beads (sufficient beads).
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

getCounts <- function(process_counts){
  
  counts <- process_counts %>%
    dplyr::select(Location, Warning, Plate) %>%
    dplyr::group_by(Location, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
      QC_total = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )
  
  return(counts)
}

<<<<<<< HEAD
##############################################################################
#' getSampleID(counts_data, plate_list)
#' @description
#' A helper function to extract Sample ID based on plate name and row/col
#'
#' @param antigen_output Output from `readAntigens` (reactive). 
#' @param plate_name Plate name inside of the plate layout file. 
#' 
#' @return Returns the corresponding Sample ID for the correct row/column in 
#' the plate layout file. Henceforth "Sample ID" refers to the code in the 
#' plate layout file, while "Sample" is the code in the Luminex file. 
#' @export
#' Author: Dionne Argyropoulos
##############################################################################

getSampleID <- function(counts_data, plate_list) {
  table <- counts_data
  layout <- plate_list
  
  # Extract row and column info
  table$Row <- substr(table$Location, 1, 1)
  table$Col <- substr(table$Location, 2, 2)
  
  # Apply logic to extract Sample directly
  table$Sample <- mapply(function(Plate, Row, Col) {
    platelayout_df <- layout[[Plate]]
    row_index <- which(platelayout_df$Plate == Row)
    col_index <- as.integer(Col) + 1  # Adjust for 1-based indexing
    platelayout_df[row_index, col_index]
  }, table$Plate, table$Row, table$Col)
  
  return(table)
}

##############################################################################
#' getAntigenCounts(antigen_output, plate_list)
#' @description
#' xxxx
#'
#' @param antigen_output Output from `readAntigens` (reactive). 
#' @param plate_name Plate name inside of the plate layout file. 
#' 
#' @return x
#' @export
#' Author: Dionne Argyropoulos
##############################################################################

getAntigenCounts <- function(process_counts, plate_list){ 
=======

##############################################################################
# getSampleID function: Get SampleID from Plate Layout
# --------------------------
#
# Description: 
# A helper function to extract Sample ID based on plate name and row/col
#  
# Usage: getSampleID(process_counts, plate_list)
# 
# Arguments: 
#   - process_counts: Output from `process_counts` (reactive).
#   - plate_name: Plate name inside of the plate layout file. 
#
# Output:Returns the corresponding Sample ID for the correct row/column in 
# the plate layout file. Henceforth "Sample ID" refers to the code in the 
# plate layout file, while "Sample" is the code in the Luminex file. 
# 
# Authors: Dionne Argyropoulos
##############################################################################

getSampleID <- function(process_counts, plate_list) {
  plate_layout_longer <- list()  
  
  for (plate_level in seq_along(plate_list)) {
    
    # Get plate name (or fallback to index)
    plate_name <- names(plate_list)[plate_level]
    if (is.null(plate_name) || plate_name == "") {
      plate_name <- as.character(plate_level)
    }
    
    # Read and wrangle Plate i
    plate_layout <- plate_list[[plate_level]]
    names(plate_layout)[1] <- "Row"
    
    plate_layout_level <- plate_layout %>% 
      pivot_longer(cols = `1`:`12`, names_to = "Col", values_to = "SampleID") %>%
      mutate(
        Location = paste0(Row, Col),
        Plate = plate_name  # Add Plate info here
      )
    
    # Save to list
    plate_layout_longer[[plate_level]] <- plate_layout_level
  }
  
  # Combine all into a single data frame
  plate_layout_longer_df <- bind_rows(plate_layout_longer) %>%
    mutate(Plate = factor(Plate))  # Make Plate a factor
  
  # Join to antigen_specific_df 
  final_table <- plate_layout_longer_df %>% 
    dplyr::left_join(process_counts, by = c("Location", "Plate")) %>% 
    dplyr::select(-c(Row, Col))
  
  return(final_table)
  
}

##############################################################################
# getAntigenCounts function: Get Count Data for each Antigen
# from the Raw Median Fluorescent Intensity
# --------------------------
#
# Description: 
# This function obtains the count data from the raw Median Fluorescent Intensity
# (MFI). This function relies on the `readAntigens` and `readSeroData` data
# processing functions.
#  
# Usage: getAntigenCounts(process_counts, plate_list)
# 
# Arguments: 
#   - process_counts: Output from `process_counts` (reactive).
#   - plate_name: Plate name inside of the plate layout file. 
#
# Output: 
#   - Data frame providing bead counts per antigen per well per plate.
#   - Designates whether wells should be repeated if there are ≤ 15 beads 
#     (repeat) or if they are sufficient with > 15 beads (sufficient beads).
#
# Authors: Dionne Argyropoulos
##############################################################################

getAntigenCounts <- function(process_counts, plate_list){
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  #############################################################################
  # Data Wrangling 
  #############################################################################
  
  antigen_specific_df <- process_counts %>%
    dplyr::select(Location, Antigen, Warning, Count, Plate) %>%
    dplyr::group_by(Location, Antigen, Count, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Repeat = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(
<<<<<<< HEAD
      Row = as.factor(substr(Location, 1, nchar(Location)-1)),
      Row = gsub("1", "", Row),
      Col = as.numeric(substr(Location, 2, nchar(Location))),
=======
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
      Count = as.numeric(Count), 
      Repeat = factor(Repeat, levels = c("sufficient beads", "repeat")), 
      QC_antigen = ifelse(Repeat == "sufficient beads", "pass", "fail")
    )
  
  #############################################################################
  # Create Table Output 
  #############################################################################
  
<<<<<<< HEAD
  table <- getSampleID(antigen_specific_df, plate_list) %>% 
    ungroup() %>% 
    dplyr::select(SampleID = Sample, Location, Antigen, Plate, Repeat, Count)
  antigen_specific_df_final <- antigen_specific_df %>% 
    dplyr::left_join(table, by = c("Plate", "Count", "Repeat", "Antigen", "Location")) %>% 
    dplyr::select(-c(Row, Col, Sum)) %>% 
=======
  table <- getSampleID(process_counts, plate_list) %>% 
    ungroup() %>% 
    dplyr::select(SampleID, Location, Antigen, Plate, Count) %>% 
    mutate(Count = as.numeric(Count))
  antigen_specific_df_final <- antigen_specific_df %>% 
    dplyr::left_join(table, by = c("Plate", "Count", "Antigen", "Location")) %>% 
    dplyr::select(-Sum) %>% 
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
    arrange(Location, Antigen, Plate)
  
  return(antigen_specific_df_final)
  
}
<<<<<<< HEAD
##############################################################################
#' getCountsQC(antigen_counts_output, counts_output)
#' @description
#' xxxx
#'
#' @param antigen_counts_output xxx
#' @param counts_output xxx
#' 
#' @return x
#' @export
#' Author: Dionne Argyropoulos
=======

##############################################################################
# getCountsQC function: Get All Counts Data 
# --------------------------
#
# Description: 
# This function obtains the count data from the raw Median Fluorescent Intensity
# (MFI). This function relies on the output of the Antigen-specific counts 
# (getAntigenCounts) and the Well or Sample-specific counts(getCounts). 
#  
# Usage: getCountsQC(antigen_counts_output, counts_output)
# 
# Arguments: 
#   - antigen_counts_output: Output from `getAntigenCounts` (reactive).
#   - counts_output: Output from `getCounts` (reactive).
#
# Output: Joined data frame for all count data.
#
# Authors: Dionne Argyropoulos
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
##############################################################################

getCountsQC <- function(antigen_counts_output, counts_output){
  
  #############################################################################
  # Data Wrangling 
  #############################################################################
  
  # 1. Data Wrangling to store counts per antigen output
  antigen_counts_only <- antigen_counts_output %>% 
<<<<<<< HEAD
    pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "Count") %>%
=======
    tidyr::pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "Count") %>% ungroup() %>%
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
    dplyr::select(Location, SampleID, Plate, everything()) %>% 
    dplyr::rename_with(~ paste0(., "_Count"), .cols = where(is.numeric))
  
  # 2. Data Wrangling to store QC pass/fail per antigen output 
  antigen_QC_only <- antigen_counts_output %>% 
    pivot_wider(id_cols = c(SampleID, Location, Plate), names_from = "Antigen", values_from = "QC_antigen") %>% 
    dplyr::rename_with(~ paste0(., "_QC"), .cols = -c(SampleID, Location, Plate))
  
  # 3. Join both antigen-specific data frames together 
  joined_antigen_counts <- antigen_counts_only %>% 
    left_join(antigen_QC_only, by = c("SampleID", "Location", "Plate"))
  
  #############################################################################
  # Re-arrange data 
  #############################################################################
  
  # Get all base marker names by stripping _Count
  marker_bases <- names(joined_antigen_counts) %>%
    grep("_Count$", ., value = TRUE) %>%
    sub("_Count$", "", .)
  
  # Create the desired column order
  new_order <- c(
    "Location", "SampleID", "Plate",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_Count"), paste0(x, "_QC"))))
  )
  
  # Reordered data frame
  joined_antigen_counts <- joined_antigen_counts %>% 
    dplyr::select(all_of(new_order))
  
  #############################################################################
  # Add total counts 
  #############################################################################
  
  total_counts_only <- counts_output %>% 
    dplyr::select(Location, Plate, QC_total)
  
  total_counts_final_output <- joined_antigen_counts %>% 
    left_join(total_counts_only, by = c("Location", "Plate")) 
  
  return(total_counts_final_output)
  
}

##############################################################################
# plotCounts function: Plot Bead Count Data 
# --------------------------
#
# Description: 
# This function gets the count data and plots the plate image, creating a new
# facet (i.e., panel) for each antigen and each line represents the
# different plates so that they can be visualised.
# 
# Usage: plotCounts(antigen_output, experiment_name)
#
# Arguments: 
#   - counts_output: Output from `getCounts` (reactive).
#   - experiment_name: User-input experiment name (reactive).
#
# Output:
#   - Tile Plot showing binary result of "sufficient beads" with cut-off >15
#   beads and "repeat" ≤15 beads (ggplot).
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

plotCounts <- function(counts_output, experiment_name){
  bead_counts <- counts_output
  bead_counts$Plate <- factor(bead_counts$Plate, levels = unique(bead_counts$Plate[order(as.numeric(str_extract(bead_counts$Plate, "\\d+")))])) # reorder by plate number 
  bead_counts %>% 
    ggplot2::ggplot(mapping = aes(x = Col, y = fct_rev(Row), fill = Repeat), fill = summary) +
    ggplot2::geom_tile(aes(height = 0.90, width = 0.90)) +
    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), position = "top") +
    ggplot2::scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", title = experiment_name , fill = "") +
    ggplot2::facet_wrap( ~ Plate, ncol = 3, scales = "free_y")  # This will create separate facets for each level of 'Plate'
}

##############################################################################
<<<<<<< HEAD
# check_repeats: Check Beads to Repeat
=======
# getRepeats: Check Beads to Repeat
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
# --------------------------
#
# Description: 
# This function gets the count data and outputs a table of the isolates to 
# repeat or a statement to confirm that none need to be repeated.
# 
<<<<<<< HEAD
# Usage: check_repeats(counts_output)
#
# Arguments: 
#   - counts_output: Output from `getCounts` (reactive).
#
# Output:
#   - Data frame with wells to "repeat", OR
#   - If no "repeats" found will return text "No repeats necessary".
=======
# Usage: getRepeats(counts_output, process_counts, plate_list)
#
# Arguments: 
#   - counts_output: Output from `getCounts` (reactive).
#   - process_counts Output from `process_counts`.
#   - plate_list: 
#
# Output:
#   - Data frame with wells to "fail", OR
#   - If no "fail" found will return text "No repeats necessary".
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
# 
# Author: Dionne Argyropoulos
##############################################################################

<<<<<<< HEAD
getRepeats <- function(counts_output, plate_list) {
  
  # 1. Filter "Repeats" in Counts Output 
  repeats <- counts_output %>% dplyr::filter(Repeat == "repeat")
=======
getRepeats <- function(counts_output, process_counts, plate_list) {
  
  # 1. Filter "Repeats" in Counts Output 
  repeats <- counts_output %>% dplyr::filter(QC_total == "fail")
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  # 2. If zero "Repeats" found, then write text. If "Repeats" found, then output table. 
  if (nrow(repeats) == 0) {
    return("No repeats necessary.")
  } else {
<<<<<<< HEAD
    table <- getSampleID(counts_output, plate_list)
    table <- table %>% 
      dplyr::select(Sample, Location, Plate, Repeat) %>% 
      dplyr::filter(Repeat == "repeat")
=======
    table <- getSampleID(process_counts, plate_list) %>% dplyr::distinct(SampleID, Location, Plate)
    table <- table %>% 
      dplyr::left_join(repeats, by = c("Location", "Plate")) %>% 
      drop_na() %>% 
      dplyr::select(Location, SampleID, Plate, QC = QC_total)
    
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
    return(table)
  }
}

##############################################################################
# plotBlanks function: Plot Raw Median Fluorescent Intensity Blanks Data
# --------------------------
#
# Description: 
# This function gets the blank sample data and plots the blank sample Median
# Fluorescent Intensity (MFI) values.
# 
# Usage: plotBlanks(antigen_output, experiment_name)
#
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#   - experiment_name: User-input experiment name (reactive).
<<<<<<< HEAD
=======
#   - plate_list: Output from `readPlateLayout` (reactive).
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
#
# Output:
#   - Bar plot showing whether MFI values for the blanks for each antigen per 
#   plate is above or below the threshold MFI = 50 (ggplot).
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

plotBlanks <- function(antigen_output, experiment_name){
  master_file <- antigen_output
  blanks <- master_file$blanks
  blanks %>% 
    dplyr::select(-Location) %>% 
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))]))) %>% # Reorder by plate number 
    ggplot2::ggplot(aes(x = factor(Antigen), y = as.numeric(MFI), fill = Sample)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    ggplot2::labs(x = "Antigen", 
                  y = "MFI",
                  title = experiment_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggplot2::facet_wrap(~ Plate)  # Create separate facets for each 'plate'
}

############################################################################## 
# plotStds function: Plot Raw Median Fluorescent Intensity of Standard Curve 
# Data
# --------------------------
#
# Description: 
# This function gets the standards data and plots the standard curves.
# 
# Usage: plotStds(antigen_output, experiment_name)
#
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#   - experiment_name: User-input experiment name (reactive).
#   - location: "PNG" or "ETH" to filter WEHI standard curve data (reactive).
#
# Output:
#   - Dot and line plot of standard curves (S1-S10) with PNG or Ethiopia stds 
#     underneath (ggplot).
#   - WEHI-acceptable standard curve data on background of plot with user data.
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

plotStds <- function(antigen_output, location, experiment_name){
  
  master_file <- antigen_output
  stds <- master_file$stds
  
  stds_1 <- stds %>% 
    dplyr::select(-Location) %>% 
    tidyr::pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number 
                  Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")), 
                  MFI = as.numeric(MFI)) 
  
  location_1 <- ifelse(location == "ETH", "ETH", "PNG")
  
  wehi_stds <- read.csv("data/wehi_compare_data/all_stds_MFI.csv")
  wehi_stds <- wehi_stds %>% dplyr::filter(Location==location_1)
  
  gg <- 
    ggplot2::ggplot() + 
    ggplot2::geom_point(data = wehi_stds, aes(x = Sample, y = MFI), colour = "grey", alpha = 0.25) + 
    ggplot2::geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate, 
                                           text = paste("Sample:", Sample, "<br>MFI:", MFI, "<br>Plate:", Plate))) + 
    ggplot2::geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) + 
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::labs(x = "Standard Curve", 
                  y = "log(MFI)",
                  title = experiment_name) +
    ggplot2:: facet_wrap(~Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

##############################################################################
# MFItoRAU_PNG function: Median Fluorescent Intensity (MFI) to Relative  
# Antibody Units (RAU) conversion
# --------------------------
#
# This function fits a 5-parameter logistic standard curve to the dilutions
# of the positive controls for each protein and converts the MFI values 
# into relative antibody units (RAU) written by Connie Li Wai Suen. 
#
# Usage: MFItoRAU(antigen_output, plate_layout)
# 
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
<<<<<<< HEAD
#   - plate_layout_file: An ".xlsx" file with sheets labelled plate1, plate2... 
#     etc. (reactive).
=======
#   - plate_list: Output from `readPlateLayout` (reactive).
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
#
# Output: A list of three data frames:
#   1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#   2. Plot information for `plotModel` function 
#   3. Data frame of RAU data for random forest classification use. 
# 
# Authors: Connie Li Wai Suen, Dionne Argyropoulos
##############################################################################

MFItoRAU_PNG <- function(antigen_output, plate_list, counts_QC_output){
  
  master_file <- antigen_output
  L <- master_file$results
  layout <- plate_list
  
  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]
  
  L$type.letter <- substr(L$Sample, start=1, stop=1)
  dilution <- c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
  dilution.scaled <- dilution*25600; dilution.scaled
  dilution.plot <- c("1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600")
  
  ##########################################################################################################
  #### LOG-LOG MODEL 
  ##########################################################################################################
  
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates
  
  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]
    
    # Fetch the corresponding layout data frame
    current_layout <- layout[[plate_level]] ######## when the plate tab name == the plate level defined in the plate column from the file name 
    
    # Initialize storage for results
    results.df.wide <- NULL
    model_list <- list()
    
    # Iterate over antigens
    for (i in antigens){
      results.df <- NULL
      ## Taking the mean of duplicates for each standard and storing in object std in the following order: S1, S2, S3, ..., S9, S10.
      std <- NULL
      b <- c <- d <- e <- NULL
      # Process standards
      for (r in 1:nrow(subset_data)){
        if (subset_data$type.letter[r]=="S"){
          std <- c(std, as.numeric(subset_data[r,i])) 
          std <- ifelse(is.na(std) | std == 0, 1, std)
        }
      }
      
      log.std <- log(as.numeric(std))
      model1 <- drm(log.std ~ dilution, fct = LL.5(names = c("b", "c", "d", "e", "f")))
      summary(model1)
      model_list[[i]] <- model1
      
      b <- coef(summary(model1))[1]; b  ## slope
      c <- coef(summary(model1))[2]; c  ## lower asymptote
      d <- coef(summary(model1))[3]; d  ## upper asymptote
      e <- coef(summary(model1))[4]; e  ## ED50
      f <- coef(summary(model1))[5]; f  ## asymmetry parameter (f=1 for 4PL curves)
      
      ##########################################################################################################
      #### MFI TO RAU CONVERSION
      ##########################################################################################################
      
      # Process unknowns
      for (r in 1:nrow(subset_data)) {
        results <- NULL
        if (subset_data$type.letter[r] == "U" | subset_data$type.letter[r] == "X") { ##### Unknown works for MAGPIX and X works for BioPlex
          mfi.X <- as.numeric(subset_data[r, i])
          y <- log(mfi.X)
          
          if (y > max(log.std)) {
            dil.X <- max(dilution)
          } else {
            dil.X <- e*(( ((d-c)/(y-c))^(1/f) - 1 )^(1/b) )
          }
          dil.X <- ifelse(dil.X > 0.02, 0.02, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y>log.std[2]), 0.02, dil.X)       ## Setting observations with very high MFI to 1/50.
          dil.X <- ifelse(dil.X < 1/51200, 1/51200, dil.X)
          dil.X <- ifelse((is.na(dil.X) & y<max(log.std)), 1/51200, dil.X)  ## Setting observations with very low MFI to 1/51200.
          
          location.X <- subset_data[r, "Location"]
          sample.X <- subset_data[r, "Sample"]
          Plate.X <- subset_data[r, "Plate"]
          results <- cbind(Location = location.X, Sample = sample.X, Plate = Plate.X, 
                           MFI = mfi.X, Dilution = dil.X, DilutionReciprocal = 1 / dil.X, 
                           MinStd = min(std), MaxDilution = min(dilution), 
                           MaxStd = max(std), MinDilution = max(dilution))
          
          results.colnames <- c("Location", "Sample", "Plate", 
                                paste0(i, "_", c("MFI", "Dilution", "DilutionReciprocal", 
                                                 "MinStd", "MaxDilution", "MaxStd", 
                                                 "MinDilution")))
          colnames(results) <- results.colnames
        }
        results.df <- rbind(results.df, results)
      }
      
      # Merge results into wide format
      if (is.null(results.df.wide)) {
        results.df.wide <- results.df
      } else {
        results.df.wide <- merge(results.df.wide, results.df, by = c("Location", "Sample", "Plate"))
      }
    }
    
    ##########################################################################################################
    #### MODEL RESULTS AND PLOTS
    ##########################################################################################################
    
    # Plot models with plate in the title
    model_results <- list()
    for (i in names(model_list)) {
      title <- paste("Plate:", plate_level, "- Protein:", i)  # Combine plate and protein name
      model_results[[i]] <- plot(model_list[[i]], main = title)
    }
    
    ##########################################################################################################
    #### MERGE DATA
    ##########################################################################################################
    
    # Bind to location
    results.df.wide <- as.data.frame(results.df.wide)
    results.location <- matrix(unlist(strsplit(as.character(results.df.wide$Location), ",")), ncol = 2, byrow = TRUE)[, 2]
    results.location <- substr(results.location, 1, nchar(results.location) - 1)
    results.df.wide <- cbind(Location.2 = results.location, results.df.wide)
    
    ## Matching SampleID from plate layout to corresponding sample.
    location.1 <- matrix(unlist(strsplit(L$Location, ",")), ncol=2, byrow=T)[,2]
    location.1 <- substr(location.1, 1, nchar(location.1)-1)
    location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
    for (i in location.2[, "Location.2"]){
      plate_layout_current <- layout[[plate_level]]
      names(plate_layout_current)[1] <- "Plate" # Relabel first column to be "Plate"
      location.2[location.2$Location.2==i, "SampleID"] <- plate_layout_current[
        plate_layout_current$Plate == unique(location.2[location.2$Location.2 == i, "alpha"]), 
        colnames(plate_layout_current) == unique(location.2[location.2$Location.2 == i, "numeric"])
      ]
    }
    row_to_match <- location.2[,c("Location.2", "SampleID")]
    row_to_match <- row_to_match %>% distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()
    
    ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
    results.df.wide <- plyr::join(results.df.wide, row_to_match, by="Location.2", type="left")
    
    ## Move SampleID to first column
    results.df.wide <- results.df.wide[, c("SampleID", colnames(results.df.wide)[!(colnames(results.df.wide) %in% "SampleID")])]
    
    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "Plate")
    
    # Convert specified columns to character
    results.df.wide[character_columns] <- lapply(results.df.wide[character_columns], as.character)
    
    # Convert all other columns (not in the specified list) to numeric
    numeric_columns <- setdiff(names(results.df.wide), character_columns)
    results.df.wide[numeric_columns] <- lapply(results.df.wide[numeric_columns], as.numeric)
    
    ##########################################################################################################
    #### Output
    ##########################################################################################################
    
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(results.df.wide))
    MFI_RAU_results <- results.df.wide[, col_selection]
    
    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- results.df.wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results
  }
  
  #############################################################################
  # Return the final results tables with QC pass/fail
  #############################################################################
  
  counts_data <- counts_QC_output %>%
    ungroup() %>% 
<<<<<<< HEAD
    dplyr::select(Location, Plate, QC_total)
  
  final_results <- dplyr::bind_rows(results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Plate"))
=======
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)
  
  final_results <- dplyr::bind_rows(results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Plate", "Location.2"))
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Plate"))
  
  # Output
  return(list(final_results, final_MFI_RAU_results, model_results_all))
  
}
<<<<<<< HEAD
=======

>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
##############################################################################
# MFItoRAU_ETH function: 
# --------------------------
#
# This function fits a 5-parameter logistic standard curve to the dilutions
# of the positive controls for each protein and converts the MFI values 
# into relative antibody units (RAU) written by Eamon Conway.
#
# Usage: MFItoRAU(antigen_output, plate_layout)
# 
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#   - plate_layout_file: An ".xlsx" file with sheets labelled plate1, plate2... 
#     etc. (reactive).
#
# Output: A list of three data frames:
#   1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#   2. Plot information for `plotModel` function.
#   3. Data frame of RAU data for random forest classification use. 
# 
# Authors: Eamon Conway, Dionne Argyropoulos
##############################################################################

MFItoRAU_ETH <- function(antigen_output, plate_list, counts_QC_output){
  
  master_file <- antigen_output$results
  L <- master_file %>% mutate(across(-c(Location, Sample, Plate), as.numeric))
  layout <- plate_list
  
  ##########################################################################################################
  #### Reference Fit 
  ##########################################################################################################
  
  refs <- read.csv(here::here("data/png_eth_stds.csv"))
  # MAGIC PARAMETERS FOR THIS SECTION
  s1_concentration <- 1/50
  s10_relative_dilution <- 2^-9
  current_min_relative_dilution <- s10_relative_dilution
  # END MAGIC PARAMETER DEFINITIONS
  
  control = list(maxit = 10000,
                 abstol = 1e-10,
                 reltol = 1e-8)
  
  initial_solution = c(-1.0, 0.0, 10, 0.0, 0.0)
  
  ref_fit <- refs %>% 
    dplyr::group_by(.data$std_plate, .data$antigen) %>% 
    tidyr::nest()  %>% 
    dplyr::mutate(
      .keep = "none",
      eth_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$eth_mfi, .x$dilution, control)
      }),
      png_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$png_mfi, .x$dilution, control)
      })
    )
  
  reference_antigens = unique(ref_fit$antigen)
  
  excluded_cols <- c("Location", "Sample", "Plate")
  remaining_cols <- setdiff(colnames(L), excluded_cols)
  antigens <- remaining_cols[remaining_cols != ""]
  
  L$type.letter <- substr(L$Sample, start=1, stop=1) # Categorises into "B" = "Blank", "S" = "Standards", "U" or "X" = "Samples"
  
  ##########################################################################################################
  #### Initialise outputs and prepare function by plate 
  ##########################################################################################################
  
  # Iterate over each level in L$Plate and corresponding layout data frame
  results_all <- list()  # To store results for all plates
  model_results_all <- list()  # To store model results for all plates
  MFI_RAU_results_all <- list() # To store MFI to RAU conversion results for all plates
  
  for (plate_idx in seq_along(unique(L$Plate))) {
    plate_level <- unique(L$Plate)[plate_idx]
    subset_data <- L[L$Plate == plate_level, ]
    
    ##########################################################################################################
    #### Apply conversion  
    ##########################################################################################################
    
    eth_qa_sc <- subset_data %>% 
<<<<<<< HEAD
      dplyr::filter(type.letter == "S") %>% 
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
=======
      filter(type.letter == "S") %>% 
      pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
      dplyr::mutate(dilution = 2 ^ (-as.numeric(gsub( # 2 = dilution factor 
        "\\D", "", .data$`Sample`
      )) + 1))  %>% 
      dplyr::group_by(.data$antigen) %>% 
      tidyr::nest()
    
    eth_qa_mfi <- subset_data %>% 
<<<<<<< HEAD
      dplyr::filter(type.letter == "U") %>% 
      tidyr::pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
=======
      filter(type.letter == "U") %>% 
      pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
      dplyr::group_by(.data$antigen) %>% 
      tidyr::nest()
    
    qa_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))
    
    # We have the fit for each antigen.
    eth_converted = dplyr::inner_join(ref_fit, qa_fit) %>%
      dplyr::inner_join(eth_qa_mfi) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          mfi = .data$mfi,
          Sample = .data$Sample,
          # dilution = convert_between_curves(.data$mfi, new_fit, eth_fit, png_fit)
          dilution = convert_mfi_to_dilution_no_lower_bound(mfi,new_fit, current_min_relative_dilution*2^-1), #This makes it s11 not s10 - Eamon 
          ref_mfi = convert_dilution_to_mfi(dilution,eth_fit),
          dilution = convert_mfi_to_dilution(ref_mfi,png_fit, s10_relative_dilution)
        )
      )) %>%
      tidyr::unnest(cols = data)
    
    # Take MEAN of these 10 repeats
    estimate_eth <- eth_converted %>%
      dplyr::group_by(antigen, Sample) %>%
      dplyr::summarise(dilution = mean(dilution) * s1_concentration,
                       mfi = mean(mfi))
    
    ##########################################################################################################
    #### MODEL RESULTS AND PLOTS
    ##########################################################################################################
    
    sc_fit <- eth_qa_sc %>%
      dplyr::mutate(.keep = "none", new_fit = purrr::map(data, ~ {
        fit_standard_curve(.x$mfi, .x$dilution, control)
      }))
    
    qa_converted <- dplyr::inner_join(sc_fit, eth_qa_sc) |>
      dplyr::rowwise() |>
      dplyr::mutate(.keep = "none", data = list(
        data |> dplyr::mutate(
          .keep = "none",
          Sample = .data$Sample,
          dilution = .data$dilution,
          mfi = .data$mfi,
          mfi_pred = convert_dilution_to_mfi(.data$dilution, new_fit)
        )
      )) |>
      tidyr::unnest(cols = data)
    
    model_results <- qa_converted
    
    ##########################################################################################################
    #### MERGE DATA: Relabel Sample Names with Plate Layout
    ##########################################################################################################
    
    # Bind to location
    eth_converted_locations <- subset_data %>% 
      dplyr::select(Location, Sample, Plate) %>%
      dplyr::right_join(estimate_eth, by = "Sample")
    
    results.location <- matrix(unlist(strsplit(as.character(eth_converted_locations$Location), ",")), ncol = 2, byrow = TRUE)[, 2]
    results.location <- substr(results.location, 1, nchar(results.location) - 1)
    eth_converted_locations <- cbind(Location.2 = results.location, eth_converted_locations)
    
    ## Matching SampleID from plate layout to corresponding sample.
    location.1 <- matrix(unlist(strsplit(subset_data$Location, ",")), ncol=2, byrow=T)[,2]
    location.1 <- substr(location.1, 1, nchar(location.1)-1)
    location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
    for (i in location.2[, "Location.2"]){
      plate_layout_current <- layout[[plate_level]]
      names(plate_layout_current)[1] <- "Plate" # Relabel first column to be "Plate"
      location.2[location.2$Location.2==i, "SampleID"] <- plate_layout_current[
        plate_layout_current$Plate == unique(location.2[location.2$Location.2 == i, "alpha"]),
        colnames(plate_layout_current) == unique(location.2[location.2$Location.2 == i, "numeric"])
      ]
    }
    row_to_match <- location.2[,c("Location.2", "SampleID")]
    row_to_match <- row_to_match %>% dplyr::distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()
    
    ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
    eth_converted_locations <- plyr::join(eth_converted_locations, row_to_match, by="Location.2", type="left")
    
    ## Move SampleID to first column
    eth_converted_locations <- eth_converted_locations[, c("SampleID", colnames(eth_converted_locations)[!(colnames(eth_converted_locations) %in% "SampleID")])]
    
    # Define column names to remain as characters
    character_columns <- c("SampleID", "Location", "Location.2", "Sample", "antigen", "Plate")
    
    # Convert specified columns to character
    eth_converted_locations[character_columns] <- lapply(eth_converted_locations[character_columns], as.character)
    
    # Convert all other columns (not in the specified list) to numeric
    numeric_columns <- setdiff(names(eth_converted_locations), character_columns)
    eth_converted_locations[numeric_columns] <- lapply(eth_converted_locations[numeric_columns], as.numeric)
    
    # Make long data frame wide 
    eth_converted_locations_mfi <-eth_converted_locations %>%
      dplyr::select(-dilution) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "mfi") %>% 
      dplyr::rename_with(~paste0(.x, "_MFI"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_locations_dilutions <- eth_converted_locations %>%
      dplyr::select(-mfi) %>%
      tidyr::pivot_wider(names_from = "antigen", values_from = "dilution") %>% 
      dplyr::rename_with(~paste0(.x, "_Dilution"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_wide <- eth_converted_locations_mfi %>% 
      dplyr::left_join(eth_converted_locations_dilutions, by = c("SampleID", "Location.2", "Location", "Sample", "Plate"))
    
    ##########################################################################################################
    #### Create output dataframes
    ##########################################################################################################
    # Save just MFI and RAU for downstream analyses
<<<<<<< HEAD
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
=======
    col_selection <- grepl("SampleID|Location.2|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
    MFI_RAU_results <- eth_converted_wide[, col_selection]
    
    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- eth_converted_wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results
    
  }
  
  ##########################################################################################################
  #### Joining all plate data 
  ##########################################################################################################
  
  counts_data <- counts_QC_output %>%
    ungroup() %>% 
<<<<<<< HEAD
    dplyr::select(SampleID, Plate, QC_total)
  
  final_results <- dplyr::bind_rows(results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Plate"))
  
  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Plate"))
  
=======
    dplyr::select(SampleID, Location.2 = Location, Plate, QC_total)
  
  final_results <- dplyr::bind_rows(results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))
  
  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all) %>% 
    inner_join(counts_data, by = c("SampleID", "Location.2", "Plate"))
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  #############################################################################
  # Re-arrange data for final outputs
  #############################################################################
  
  # Get all base marker names by stripping _Count
  marker_bases <- names(final_results) %>%
    grep("_MFI$", ., value = TRUE) %>%
    sub("_MFI$", "", .)
  
  # Create the desired column order
  final_results_order <- c(
    "SampleID", "Location.2", "Location", "Sample", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )
  final_MFI_RAU_order <- c(
    "SampleID", "Plate", "QC_total",
    unlist(lapply(marker_bases, function(x) c(paste0(x, "_MFI"), paste0(x, "_Dilution"))))
  )
  
  # Reordered data frame
  final_results <- final_results %>% 
    dplyr::select(all_of(final_results_order))
  
  final_MFI_RAU_results <- final_MFI_RAU_results %>% 
    dplyr::select(all_of(final_MFI_RAU_order))
  
  return(list(final_results, final_MFI_RAU_results, model_results_all))
}
<<<<<<< HEAD
=======

>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
##############################################################################
# plotModel_PNG function: Plot the Median Fluorescent Intensity (MFI) to  
# Relative Antibody Units (RAU) Results Data
# --------------------------
#
# Description: 
# This function gets the Median Fluorescent Intensity (MFI) to Relative 
# Antibody Units (RAU) model results data and plots the model fits based on
# MFItoRAU_PNG.
# 
# Usage: plotModel(mfi_to_rau_output, antigen_output)
#
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#   - mfi_to_rau_output: Output from `MFItoRAU_PNG` (reactive).
#
# Output:
#   - List of dot and line plots of MFI to RAU model standard curve, with each 
#     one representing an individual plate (ggplots).
# 
# Authors: Shazia Ruybal-Pesantez, Dionne Argyropoulos
##############################################################################

plotModel_PNG <- function(mfi_to_rau_output, antigen_output){
  
  model_results <- mfi_to_rau_output[[3]]
  
  # Create a combined data frame with plate and protein
  combined_data <- do.call(rbind, lapply(names(model_results), function(file_name) {
    # Get the list of antigens for this file
    antigens <- model_results[[file_name]]
    # For each antigen in the file
    lapply(names(antigens), function(antigen_name) {
      data <- antigens[[antigen_name]]  # Get the antigen's data frame
      data$Plate <- file_name  # Add plate column will be the file name
      data$Antigen <- antigen_name  # Add antigen column will be the antigen name
      return(data)
    })
  }))
  
  # Convert the list of data frames into a single data frame
  combined_data <- do.call(rbind, combined_data)
  
  ### Get Standards for points
  stds_file <- antigen_output$stds
  stds_log <- 
    stds_file %>%
    dplyr::mutate(across(-c(Location, Sample, Plate), ~ as.numeric(.))) %>% 
    tidyr::pivot_longer(-c(Location, Sample, Plate), names_to = "Antigen", values_to = "stdcurve") %>%
    dplyr::mutate(dilution = ifelse(
      Sample == "S1", 1/50, 
      ifelse(Sample == "S2", 1/100, 
             ifelse(Sample == "S3", 1/200, 
                    ifelse(Sample == "S4", 1/400, 
                           ifelse(Sample == "S5", 1/800, 
                                  ifelse(Sample == "S6", 1/1600, 
                                         ifelse(Sample == "S7", 1/3200, 
                                                ifelse(Sample == "S8", 1/6400, 
                                                       ifelse(Sample == "S9", 1/12800, 
                                                              ifelse(Sample == "S10", 1/25600, NA)))))))))))
  
  # Generate plots for each plate, grouping proteins together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot() +  # Use 'protein' to differentiate lines
      ggplot2::geom_line(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = exp(`1`), color = Antigen)) +
      ggplot2::geom_point(data = subset(stds_log, Plate == plate_name), aes(x = dilution, y = stdcurve, color = Antigen)) +
      ggplot2::scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                             labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::labs(x = "Antibody Dilution",
                    y = "Standard Curve (log(MFI))",
                    title = paste("Standard Curves for Plate:", plate_name)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ Antigen, scales = "free")  # Create a separate plot for each antigen
  })
  
  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)
  
  # Arrange all plots using grid.arrange
  # arranged_plots <- do.call(grid.arrange, c(plots_model, ncol = 1))  # Stack plots vertically
  
  return(plots_model)
}

##############################################################################
# plotModel_ETH function: Plot the Median Fluorescent Intensity (MFI) to  
# Relative Antibody Units (RAU) Results Data based on MFItoRAU_ETH. 
# --------------------------
#
# Description: 
# This function gets the Median Fluorescent Intensity (MFI) to Relative Antibody
# Units (RAU) model results data and plots the model fits.
# 
# Usage: plotModel(mfi_to_rau_output, antigen_output)
#
# Arguments: 
#   - antigen_output: Output from `readAntigens` (reactive).
#   - mfi_to_rau_output: Output from `MFItoRAU_ETH` (reactive).
#
# Output:
#   - List of dot and line plots of MFI to RAU model standard curve, with each 
#     one representing an individual plate (ggplots).
# 
# Authors: Dionne Argyropoulos
##############################################################################

plotModel_ETH <- function(mfi_to_rau_output, antigen_output){
  
  # Load model results 
  model_results <- mfi_to_rau_output[[3]]
  
  # Convert the list of data frames into a single data frame
  combined_data <- model_results %>% 
    dplyr::bind_rows(.id = "Plate")
  
  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot2::ggplot(data = subset(combined_data, Plate == plate_name), 
                    aes(x = dilution, y = mfi_pred, color = antigen)) +  # Use 'Antigen' to differentiate lines
<<<<<<< HEAD
      ggplot2::geom_line() +
      ggplot2::scale_x_log10() +    
      ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
      ggplot2::geom_point(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = mfi, color = antigen)) +
      ggplot2::labs(x = "Antibody Dilution",
                    y = "Standard Curve (log(MFI))",
                    fill = "Antigen",
                    title = paste("Standard Curves for Plate:", plate_name)) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ antigen, scales = "free_y")  # Create a separate plot for each Antigen
=======
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +    
    ggplot2::scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    ggplot2::geom_point(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = mfi, color = antigen)) +
    ggplot2::labs(x = "Antibody Dilution",
                  y = "Standard Curve (log(MFI))",
                  fill = "Antigen",
                  title = paste("Standard Curves for Plate:", plate_name)) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ antigen, scales = "free_y")  # Create a separate plot for each Antigen
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  })
  
  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)
  
  return(plots_model)
}

##############################################################################
# classify_final_results function: Random Forest Classification
# --------------------------
#
# Description: 
# This function classifies unknown samples as recently exposed or not 
# (Note: MFItoRAU() needs to be run first to convert to RAU).
#  
# Usage: classify_final_results(mfi_to_rau_output, algorithm_type, Sens_Spec)
#
# Arguments: 
#   - mfi_to_rau_output: Output from `MFItoRAU` (reactive)
#   - algorithm_type: User-selected algorithm choice: 
#       * "antibody_model" (PvSeroTaT model; default), or 
#       * "antibody_model_excLF016" (PvSeroTat excluding LF016).
#   - Sens_Spec: User-selected Sensitivity/Specificity threshold: 
#       * "maximised" (default), 
#       * "85\% sensitivity",
#       * "90\% sensitivity",
#       * "95\% sensitivity", 
#       * "85\% specificity",
#       * "90\% specificity".
#       * "95\% specificity".
#
# Output:
#   - Data frame with exposure status for every sample.
#   - Summary table with positive/negative results for each threshold.
# 
# Authors: Lauren Smith, Dionne Argyropoulos
##############################################################################

classify_final_results <- function(mfi_to_rau_output, algorithm_type, Sens_Spec, counts_QC_output) {
  
  #############################################################################
  # Data wrangling
  #############################################################################
  
<<<<<<< HEAD
  rau_data <- mfi_to_rau_output[[2]]
  rau_data <- rau_data %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
    rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix
=======
  rau_data <- mfi_to_rau_output[[1]]
  rau_data <- rau_data %>%
    dplyr::select(SampleID, Plate, Location.2, ends_with("_Dilution")) %>%
    dplyr::mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
    dplyr::rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  #############################################################################
  # Model-specific functions
  #############################################################################
  
  # Step 1. Reads in serostatus using the trained random forest
  antibody_model <- readRDS(here::here("model/PvSeroTaTmodel.rds")) # Model 1: All top 8
  antibody_model_excLF016 <- readRDS(here::here("model/random_forest_excludingLF016.rds")) # Model 2: w/o LF016 
  
  # Step 2: Read in the random forest votes threshold values
  threshold_table <- if(algorithm_type == "antibody_model"){
    read.csv(here::here("model/threshold_values.csv"))
  } else if (algorithm_type == "antibody_model_excLF016"){
    read.csv(here::here("model/excluding_LF016_threshold_values.csv"))
  } else {
    stop("Invalid model provided")
  }
  
  # Step 3: Determine random forest votes threshold based on the algorithm_type string
  threshold <- if (Sens_Spec == "maximised") {
    threshold_table %>% filter(sens_spec == "max_sens_spec") %>% pull(threshold)
  } else if (Sens_Spec == "85% sensitivity") {
    threshold_table %>% filter(sens_spec == "85_sens") %>% pull(threshold)
  } else if (Sens_Spec == "90% sensitivity") {
    threshold_table %>% filter(sens_spec == "90_sens") %>% pull(threshold)
  } else if (Sens_Spec == "95% sensitivity") {
    threshold_table %>% filter(sens_spec == "95_sens") %>% pull(threshold)
  } else if (Sens_Spec == "85% specificity") {
    threshold_table %>% filter(sens_spec == "85_spec") %>% pull(threshold)
  } else if (Sens_Spec == "90% specificity") {
    threshold_table %>% filter(sens_spec == "90_spec") %>% pull(threshold)
  } else if (Sens_Spec == "95% specificity") {
    threshold_table %>% filter(sens_spec == "95_spec") %>% pull(threshold)
  } else {
    stop("Invalid sensitivity/specificity type provided.")
  }
  
  # Step 4: Run the model
  # Retrieve the model based on the algorithm_type string
  model <- get(algorithm_type)
  
  #############################################################################
  # Model outputs
  #############################################################################
  
  # Classify rau_data using the specified model
  class_preds <- predict(model, new_data = rau_data)
  prob_preds <- predict(model, new_data = rau_data, type = "prob")
  # Binds predictions to rau_data
<<<<<<< HEAD
  results <- rau_data %>% bind_cols(class_preds, prob_preds)
  # Classify new (seropositive) / old (seronegative) based on selected threshold
  results <- results %>%
    mutate(pred_class_max = ifelse(.pred_new > threshold, "new", "old"),
           pred_class_max = as.factor(pred_class_max))
  # Final processing and renaming
  final_results <- results %>%
    dplyr::select(-c(.pred_class, .pred_new, .pred_old)) %>%
    mutate(pred_class_max = recode(pred_class_max, "new" = "seropositive", "old" = "seronegative")) 
=======
  results <- rau_data %>% dplyr::bind_cols(class_preds, prob_preds)
  # Classify new (seropositive) / old (seronegative) based on selected threshold
  results <- results %>%
    dplyr::mutate(pred_class_max = ifelse(.pred_new > threshold, "new", "old"),
                  pred_class_max = as.factor(pred_class_max))
  # Final processing and renaming
  final_results <- results %>%
    dplyr::select(-c(.pred_class, .pred_new, .pred_old)) %>%
    dplyr::mutate(pred_class_max = recode(pred_class_max, "new" = "seropositive", "old" = "seronegative")) 
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  #############################################################################
  # Return the table of prediction classes and QC pass/fail
  #############################################################################
  
  final_classification_qc <- counts_QC_output %>% 
<<<<<<< HEAD
    ungroup() %>% 
    dplyr::select(SampleID, Plate, QC_total) %>% 
    inner_join(final_results, by = c("SampleID", "Plate"))
=======
    dplyr::ungroup() %>% 
    dplyr::select(SampleID, Plate, Location.2 = Location, QC_total) %>% 
    dplyr::inner_join(final_results, by = c("SampleID", "Plate", "Location.2")) %>% 
    dplyr::select(-Location.2)
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  return(final_classification_qc)
}

##############################################################################
# plotBoxPlotClassification function: Plot Classification
# --------------------------
#
# Description: 
# One example of data visualisation to detect the median and interquartile range
# of the RAU values per antigen for seropositive and seronegative individuals.
# Please note that the `classify_final_results` function must be run first.
#
# Usage: plotBoxPlotClassification(all_classifications, selected_threshold)
# 
# Arguments: 
#   - all_classifications: Data frame of `classify_final_results()` for all 
#     Sens_Spec thresholds. 
#   - selected_threshold: String with the threshold (reactive).
#
# Output:
#   - Box plots with RAU values for each protein stratified by classification 
#     (ggplot).
# 
# Author: Dionne Argyropoulos
##############################################################################

plotBoxPlotClassification <- function(all_classifications, selected_threshold){
  
  all_classifications %>% 
    dplyr::filter(Sens_Spec == selected_threshold) %>% 
<<<<<<< HEAD
    tidyr::pivot_longer(-c(SampleID, Plate, pred_class_max, Sens_Spec), names_to = "Antigen", values_to = "RAU") %>%
=======
    tidyr::pivot_longer(-c(SampleID, Plate, QC_total, pred_class_max, Sens_Spec), names_to = "Antigen", values_to = "RAU") %>%
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
    dplyr::mutate(pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))) %>%
    ggplot2::ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
    ggplot2::labs(title = paste0("Threshold Chosen: "), selected_threshold, 
                  x = "Classification", y = "RAU", fill = "Classification") +
    ggplot2::facet_grid(~Antigen) +
    ggplot2:: theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

##############################################################################
# plotMFI function: Median Fluorescent Intensity (MFI) Box Plots
# --------------------------
#
# Description: 
# Boxplot of the MFI values.
# 
# Usage: plotMFI(mfi_to_rau_output)
# 
# Arguments: 
#   - mfi_to_rau_output: Output from `MFItoRAU` (reactive).
#   - location: PNG or ETH (reactive).
#
# Output:
#   - Box plots with MFI values for each protein (ggplot).
# 
# Author: Dionne Argyropoulos
##############################################################################

plotMFI <- function(mfi_to_rau_output, location){
  
  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_MFI")) %>%
    dplyr::rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number 
                  MFI = as.numeric(MFI)) 
  
<<<<<<< HEAD
  if (location == "PNG"){
    
    df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_MFI.csv"))
    
    plot <- df_results %>% 
      ggplot2::ggplot(aes(x= Antigen, y = MFI)) +
      ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = MFI), fill = "grey", colour = "darkgrey") + 
      ggplot2::geom_boxplot(aes(fill = Antigen)) +
      ggplot2::scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, 10000), labels = c("10", "100", "1,000", "10,000")) +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody log(MFI)") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
    
  } else if (location == "ETH") {
    
    plot <- df_results %>% 
      ggplot2::ggplot(aes(x= Antigen, y = MFI, fill = Antigen)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10() +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody log(MFI)") +
      ggplot2::facet_wrap( ~ Plate) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
    
  }
=======
  df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_MFI.csv"))
  
  plot <- df_results %>% 
    ggplot2::ggplot(aes(x= Antigen, y = MFI)) +
    ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = MFI), fill = "grey", colour = "darkgrey") + 
    ggplot2::geom_boxplot(aes(fill = Antigen)) +
    ggplot2::scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, 10000), labels = c("10", "100", "1,000", "10,000")) +
    ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
    ggplot2::labs(x = "Antigen", y = "Antibody log(MFI)") +
    ggplot2::facet_wrap( ~ Plate) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  return(plot)
  
}

##############################################################################
# plotRAU function: Relative Antibody Unit (RAU) Box Plots
# --------------------------
#
# Description: 
# Boxplot of the RAU values. 
# 
# Usage: plotRAU(mfi_to_rau_output)
# 
# Arguments: 
#   - mfi_to_rau_output: Output from `MFItoRAU` (reactive).
#   - location: PNG or ETH (reactive).
#
# Output:
#   - Box plots with RAU values for each protein (ggplot).
# 
# Author: Dionne Argyropoulos
##############################################################################

plotRAU <- function(mfi_to_rau_output, location){
  
  df_results <- mfi_to_rau_output[[2]]
  df_results <- df_results %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    dplyr::rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
    tidyr::pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "RAU") %>% 
    dplyr::mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number 
                  RAU = as.numeric(RAU)) 
  
<<<<<<< HEAD
  if (location == "PNG"){
    
    df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_RAU.csv"))
    
    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = RAU), fill = "grey", colour = "darkgrey") +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                             labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
      ggplot2::facet_wrap( ~ Plate) + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if (location == "ETH") {
    
    plot <- df_results %>%
      ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10() +
      ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
      ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
      ggplot2::facet_wrap( ~ Plate) + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  }
=======
  df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_RAU.csv"))
  
  plot <- df_results %>%
    ggplot2::ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
    ggplot2::geom_boxplot(data = df_wehi, aes(x = Antigen, y = RAU), fill = "grey", colour = "darkgrey") +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                           labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
    ggplot2::scale_fill_brewer(palette = "Paired", type = "qual") +
    ggplot2::labs(x = "Antigen", y = "Antibody RAU") +
    ggplot2::facet_wrap( ~ Plate) + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
  
  return(plot)
  
}

##############################################################################
# plotBeadCounts function
# --------------------------
# 
# Description: 
# Enhances the `plotCounts` output by providing greater resolution, displaying 
# antigens per plate, and enabling SampleID name visibility via hover 
# (transformed to Plotly in server.R)
# 
# Usage: plotBeadCounts(antigen_output, plate_layout)
#
# Arguments: 
#   - antigen_counts_output: Output from `getAntigenCounts` (reactive).
#
# Output:
#   - Dot plot with values > 15 threshold coloured in blue (sufficient beads) 
#     and ≤15 beads coloured in red (repeat) faceted by each antigen (ggplot).
# 
# Author: Dionne Argyropoulos
##############################################################################
<<<<<<< HEAD
=======

>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
plotBeadCounts <- function(antigen_counts_output){
  
  antigen_counts_output$Plate <- factor(antigen_counts_output$Plate, levels = unique(antigen_counts_output$Plate[order(as.numeric(str_extract(antigen_counts_output$Plate, "\\d+")))])) # reorder by plate number 
  antigen_counts_output %>% 
<<<<<<< HEAD
    ggplot(aes(Plate, Count, colour = Repeat, alpha = Repeat, size = Repeat, 
               text = paste("Sample:", SampleID, "<br>Bead Count:", Count, "<br>Location:", Location,"<br>Plate:", Plate))) + 
    geom_hline(yintercept = 15, linetype = "dashed", colour = "#861e18") +
    geom_point() +
    scale_y_continuous(breaks = c(0, 15, 50, 100, 150, 200)) +
    scale_colour_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    scale_alpha_manual(values = c("sufficient beads" = 0.5, "repeat" = 1)) +
    scale_size_manual(values = c("sufficient beads" = 1, "repeat" = 3)) + 
    labs(x = "Plate", y = "Bead Counts", alpha = "", colour = "", size = "") +  # Add legend title
    facet_grid(~ Antigen) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") + # Show legend
    guides(alpha = "none") + 
    guides(size = "none") 
  
=======
    ggplot2::ggplot(
      aes(Plate, Count, colour = Repeat, alpha = Repeat, size = Repeat,
          text = paste("Sample:", SampleID, "<br>Bead Count:", Count, "<br>Location:", Location,"<br>Plate:", Plate))) + 
    ggplot2::geom_hline(yintercept = 15, linetype = "dashed", colour = "#861e18") +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(breaks = c(0, 15, 50, 100, 150, 200)) +
    ggplot2::scale_colour_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    ggplot2::scale_alpha_manual(values = c("sufficient beads" = 0.5, "repeat" = 1)) +
    ggplot2::scale_size_manual(values = c("sufficient beads" = 1, "repeat" = 3)) + 
    ggplot2::labs(x = "Plate", y = "Bead Counts", alpha = "", colour = "", size = "") +  # Add legend title
    ggplot2::facet_grid(~ Antigen) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") + # Show legend
    ggplot2::guides(alpha = "none") + 
    ggplot2::guides(size = "none") 
  
}

############################################################################################################################################################
# Ethiopian standard curve functions
# Author: Eamon Conway
############################################################################################################################################################

#' Convert between curves.
#' @description
#' Convert mfi on new plate to the dilution on the reference plate.
#'
#' @param mfi Known mfi of samples to be converted
#' @param params_new Known parameters for five parameter logistic fit.
#' @param params_ref_new Known parameters for five parameter logistic fit on reference plate
#' @param params_ref_old Known parameters for five parameter logistic fit of the old beads on the sample plate.
#' @return Returns the predicted dilution in comparison to the reference plate
#' @export
convert_between_curves <- function(mfi, params_new, params_ref_new, params_ref_old) {
  dilution <- convert_mfi_to_dilution(mfi,params_new)
  ref_mfi <- convert_dilution_to_mfi(dilution,params_ref_new)
  convert_mfi_to_dilution(ref_mfi,params_ref_old)
}

#' Convert known dilution to mfi from fitted standard curve
#' @description
#' Convert dilution to predicted mfi using known standard curve fit.
#'
#' @param dilution Known dilution of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the predicted mfi of a sample with known dilution.
#' @export
convert_dilution_to_mfi <- function(dilution, params) {
  if (is.null(dilution) || is.null(params)) {
    error("Require both mfi and params to run.")
  }
  exp(log_logistic_5p(dilution, params[1], params[2], params[3], params[4], exp(params[5])))
}

#' Convert mfi to dilution using known standard curve fit.
#' @description
#' Convert mfi to dilution using known standard curve fit.
#'
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1. 
#' @return Returns the dilution of each sample in mfi.
#' @export
convert_mfi_to_dilution <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- min_relative_dilution
  result[y < params[6]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}


#' Convert mfi to dilution using known standard curve fit and no lower bound
#' @description
#' Convert mfi to dilution using known standard curve fit and no lower bound unless you are below the asymptote of the standard curve. 
#' In this situation we set your value to min_relative_dilution. I dunno argue? 
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @param min_relative_dilution Known minimum value of dilution in the standard curve. Relative means setting S1 to a dilution/RAU/concentration of 1. 
#' @return Returns the dilution of each sample in mfi.
#' @export
convert_mfi_to_dilution_no_lower_bound <- function(mfi, params, min_relative_dilution) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- min_relative_dilution
  result[y > params[7]] <- 1.0
  # I dont think this will happen - Eamon (ask if needed)
  result[result > 1.0] <- 1.0
  return(result)
}

#' Fit a standard curve to known mfi and dilution values.
#' @description
#' We wish to convert the standard curve samples to a five parameter logistic curve.
#' This function takes those values and calls optim to determine the fit.
#'
#' @param mfi Known mfi of samples
#' @param dilution Known dilution of samples
#' @param init Initial guess for solution of fit.
#' @param control Optional list of control parameters for the underlying call to optim.
#' @export
fit_standard_curve <- function(mfi, dilution, control = NULL) {
  if (is.null(mfi) | is.null(dilution)) {
    error("Require both mfi and dilution to run.")
  }
  
  y1 <- log(mfi)
  initial_solution <- c(-1.0, 0.0, max(y1), 0.0, 0.0)
  
  error_func <- function(x) {
    f1 <- log_logistic_5p(dilution, x[1], x[2], x[3], x[4], exp(x[5]))
    sum((y1 - f1)^2.0)
  }
  
  solution <- optim(par = initial_solution, fn = error_func, control = control)
  if (solution$convergence != 0) {
    stop("Standard curve failed to converge. Look at data and possibly change control parameters from default.")
  }
  c(solution$par, min(y1), max(y1))
}

inverse_log_logistic_5p <- function(y,b,c,d,e,f){
  A <- (d/(y-c))^(1/f)-1
  return(exp(-e) *A^(1/b))
}

log_logistic_5p <- function(x, b, c, d, e, f) {
  return(c + d / (1.0 + exp(b * (log(x) + e)))^f)
>>>>>>> d15b2562b45868748206fef79cd14f008c06580b
}