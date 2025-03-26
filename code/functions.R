################################# FUNCTIONS ##################################
# This script stores the functions used in the app 
##############################################################################

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
  # Load platemap
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
  
  # Loop through each file and process accordingly
  for (i in seq_along(raw_data)) {
    file <- raw_data[i]
    file_name <- raw_data_filenames[i]
    
    check_platform <- function(raw_data, raw_data_filenames, platform) {
      
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
      
      # Initialize result variable
      result_msg <- "PASS"  # Default to "PASS"
      
      # User selected "magpix" but the file does not have "Program" or "xPonent"
      if (platform == "magpix" && !is_magpix) {
        stop(paste("Error: The file", file_name, "does not appear to be a 'magpix' file, but the platform was set to 'magpix'. Please check your selection."))
      }
      
      # User selected "bioplex" but the file contains "Program" or "xPonent"
      if (platform == "bioplex" && is_magpix) {          
        stop(paste("Error: The file", file_name, "appears to be a 'magpix' file, but the platform was set to 'bioplex'. Please check your selection."))
      }
      
      return(result_msg)
      
    }
    
    # Store the check result
    check_result <- check_platform(raw_data, raw_data_filenames, platform)
    
    # Print message based on check result
    if (check_result == "PASS") {
      message("PASS: File ", file_name, " successfully validated.")
    } else {
      warning(check_result)  # Print warning message
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
        
        results <- read_excel(file, skip = median_row_number + 1)
        counts <- read_excel(file, skip = count_row_number + 1, n_max = endcount_row_number - count_row_number - 2, col_names = TRUE)
        run <- read_excel(file, n_max = median_row_number)
        
      } else if (file_extension == "csv") {
        
        first_lines <- readLines(file, n = 5)           # Read the first few lines of the file
        
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
        dplyr::select(-`Total Events`) %>%
        mutate(across(everything(), ~ gsub("NaN", 0, .))) %>% # Change "NaN" to 0s
        mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), 
                               ifelse(Sample == "B", paste0("Blank", row_number()), Sample))) %>% # Sequentially relabel Blank rows and keep other Sample values unchanged
        mutate(Sample = ifelse(Sample == "S", paste0("S", cumsum(Sample == "S")), Sample)) # Sequentially relabel Sample rows and keep other Sample values 
      
      # 3. Load counts for QC 
      counts <- counts %>%
        mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), 
                               ifelse(Sample == "B", paste0("Blank", row_number()), Sample))) %>% # Sequentially relabel Blank rows and keep other Sample values unchanged
        dplyr::select(-`Total Events`)
      counts <- as_tibble(counts)
      
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
      plate_numbers <- file_name %>% str_extract("(?i)plate\\d+(?=[._-]|$)")
      
      # Add 'plate' column to each dataframe
      data_raw$Plate <- plate_numbers
      results$Plate <- plate_numbers
      counts$Plate <- plate_numbers
      blanks$Plate <- plate_numbers
      stds$Plate <- plate_numbers        
      run_info$Plate <- plate_numbers
      
      # Add processed file's tables to the master list
      master_list$data_raw <- bind_rows(master_list$data_raw, data_raw)   # Combine raw data
      master_list$results  <- bind_rows(master_list$results, results)     # Combine processed results
      master_list$counts   <- bind_rows(master_list$counts, counts)       # Combine counts
      master_list$blanks   <- bind_rows(master_list$blanks, blanks)       # Combine blanks
      master_list$stds     <- bind_rows(master_list$stds, stds)           # Combine stds
      master_list$run      <- bind_rows(master_list$run, run_info)        # Combine run
      
      
    } else if (platform == "bioplex") { 
      
      file_extension <- tools::file_ext(file)     # Identify the file extension and read the file accordingly
      
      if (file_extension == "xlsx") {
        
        full <- suppressMessages(readxl::read_excel(file))
        df <- as.data.frame(full) 
        
      } else if (file_extension == "csv") {
        
        # Check for the delimiter
        first_lines <- readLines(file, n = 5)
        
        if (all(grepl(";", first_lines))) {
          full <- suppressMessages(read.csv2(file))
          df <- as.data.frame(full)
        } else {
          full <- suppressMessages(read.csv(file))
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
        mutate(Type = ifelse(Type == "B", "Blank", Type), # Re-label 
               suffix = as.numeric(gsub("\\D", "", Type)), # Order so that standards and blanks are at the top
               prefix = substr(Type, 1, 1)) %>% # Order so that standards and blanks are at the top
        arrange(prefix, suffix) %>% # Order so that standards and blanks are at the top 
        left_join(platemap, by = "Well") %>%  # Join on the Well column
        dplyr::select(-c(prefix, suffix, Region, Gate, Total, `% Agg Beads`, `Sampling Errors`, Well, any_of("Description"))) %>% # Remove unnecessary columns, including Description if it exists
        dplyr::select(Location, Sample = Type, everything()) %>% # Rename columns to be same as magpix 
        mutate(across(everything(), ~ gsub("NaN", 0, .)),  # Change "NaN" to 0s
               across(everything(), ~ gsub("\\*\\*\\*", "0", .)), #Change "***" to 0s
               Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), Sample)) # Sequentially relabel Blank rows and keep other Sample values unchanged
      
      results <- df %>% mutate(across(-c(Location, Sample), ~ gsub("\\s*\\(.*\\)", "", .)))
      
      # 3. Load counts for QC 
      counts <- df %>% mutate(across(-c(Location, Sample), ~ gsub(".*\\((.*)\\).*", "\\1", .)))
      
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
      plate_numbers <- file_name %>% str_extract("(?i)plate\\d+(?=[._-]|$)")
      
      # Add 'plate' column to each dataframe
      data_raw$Plate <- plate_numbers
      results$Plate <- plate_numbers
      counts$Plate <- plate_numbers
      blanks$Plate <- plate_numbers
      stds$Plate <- plate_numbers
      run_info$Plate <- plate_numbers
      
      # Stitch together for master file
      master_list$data_raw <- bind_rows(master_list$data_raw, data_raw)   # Combine raw data
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
    colnames(df) <- case_when(
      str_detect(colnames(df), regex("EBP", ignore_case = TRUE)) ~ "EBP",
      str_detect(colnames(df), regex("LF005", ignore_case = TRUE)) ~ "LF005",
      str_detect(colnames(df), regex("LF010", ignore_case = TRUE)) ~ "LF010",
      str_detect(colnames(df), regex("LF016", ignore_case = TRUE)) ~ "LF016",
      str_detect(colnames(df), regex("(MSP8|L34)", ignore_case = TRUE)) ~ "MSP8",
      str_detect(colnames(df), regex("(P87|RBP2b-P87)", ignore_case = TRUE)) ~ "RBP2b.P87",
      str_detect(colnames(df), regex("(PTEX|PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150",
      str_detect(colnames(df), regex("CSS", ignore_case = TRUE)) ~ "PvCSS",
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
    dplyr::select(any_of(c("Location", "Sample",  "Plate", "EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")))
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
  
  # Step 1: Get the sheet names to confirm
  sheet_names <- getSheetNames(plate_layout)
  
  # Step 2: Read all sheets into plate_layout_list using indices
  plate_layout_list <- lapply(1:length(sheet_names), function(i) {
    read.xlsx(plate_layout, sheet = i)
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
# getCounts function: Get Count Data from Raw Median Fluescent Itensity
# --------------------------
#
# Description: 
# This function obtains the count data from the raw Median Fluescent Itensity
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

getCounts <- function(antigen_output){
  
  master_file <- antigen_output
  counts <- master_file$counts
  
  counts <- counts %>%
    dplyr::select(-Sample) %>% # can maybe "keep" relevant columns + protein names?
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>% 
    tidyr::pivot_longer(-c(Location, Plate), names_to = "Antigen", values_to = "Count") %>% 
    dplyr::mutate(Warning = case_when(
      as.numeric(Count)<15~1,
      as.numeric(Count)>=15~0
    )) %>%
    dplyr::select(Location, Warning, Plate) %>%
    dplyr::group_by(Location, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Colour = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(Row = substr(Location, 1, nchar(Location)-1)) %>%
    dplyr::mutate(Col = substr(Location, 2, nchar(Location))) %>%
    dplyr::mutate(Row = gsub("1", "", Row)) %>%
    dplyr::mutate(Row = as.factor(Row)) %>%
    dplyr::mutate(Col = as.numeric(Col))
  
  return(counts)
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
    ggplot(mapping = aes(x = Col, y = fct_rev(Row), fill = Colour), fill = summary) +
    geom_tile(aes(height = 0.90, width = 0.90)) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       position = "top") +
    scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    theme_bw() +
    labs(x = "", y = "", title = experiment_name , fill = "") +
    facet_wrap( ~ Plate, ncol = 3, scales = "free_y")  # This will create separate facets for each level of 'Plate'
}

##############################################################################
# check_repeats function: Check Beads to Repeat
# --------------------------
#
# Description: 
# This function gets the count data and outputs a table of the isolates to 
# repeat or a statement to confirm that none need to be repeated.
# 
# Usage: check_repeats(counts_output)
#
# Arguments: 
#   - counts_output: Output from `getCounts` (reactive).
#
# Output:
#   - Data frame with wells to "repeat", OR
#   - If no "repeats" found will return text "No repeats necessary".
# 
# Author: Dionne Argyropoulos
##############################################################################

check_repeats <- function(counts_output) {
  bead_counts <- counts_output
  repeat_rows <- bead_counts %>% filter(Colour == "repeat")
  if (nrow(repeat_rows) == 0) {
    return("No repeats necessary.")
  } else {
    return(repeat_rows)
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
    pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))]))) %>% # Reorder by plate number 
    ggplot(aes(x = factor(Antigen), y = as.numeric(MFI), fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    labs(x = "Antigen", 
         y = "MFI",
         title = experiment_name) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Plate)  # Create separate facets for each 'plate'
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
    pivot_longer(-c(Sample, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number 
           Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")), 
           MFI = as.numeric(MFI)) 
  
  location_1 <- ifelse(location == "ETH", "ETH", "PNG")
  
  wehi_stds <- read.csv("data/wehi_compare_data/all_stds_MFI.csv")
  wehi_stds <- wehi_stds %>% filter(Location==location_1)
  
  gg <- 
    ggplot() + 
    geom_point(data = wehi_stds, aes(x = Sample, y = MFI), colour = "grey", alpha = 0.25) + 
    geom_point(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate, 
                                  text = paste("Sample:", Sample, "<br>MFI:", MFI, "<br>Plate:", Plate))) + 
    geom_line(data = stds_1, aes(x = Sample, y = MFI, color = Plate, group = Plate)) + 
    scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    labs(x = "Standard Curve", 
         y = "log(MFI)",
         title = experiment_name) +
    facet_wrap(~Antigen) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
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
#   - plate_layout_file: An ".xlsx" file with sheets labelled plate1, plate2... 
#     etc. (reactive).
#
# Output: A list of three data frames:
#   1. Data frame with  MFI data, converted RAU data and matched SampleID's.
#   2. Plot information for `plotModel` function 
#   3. Data frame of RAU data for random forest classification use. 
# 
# Authors: Connie Li Wai Suen, Dionne Argyropoulos
##############################################################################

MFItoRAU_PNG <- function(antigen_output, plate_layout){
  
  master_file <- antigen_output
  L <- master_file$results
  layout <- readPlateLayout(plate_layout, antigen_output)
  
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
  
  final_results <- dplyr::bind_rows(results_all)
  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all)
  
  # Output
  return(list(final_results, final_MFI_RAU_results, model_results_all))
  
}

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

MFItoRAU_ETH <- function(antigen_output, plate_layout){
  
  master_file <- antigen_output$results
  L <- master_file %>% mutate(across(-c(Location, Sample, Plate), as.numeric))
  layout <- readPlateLayout(plate_layout, antigen_output)
  
  refs <- read.csv(here::here("data/png_eth_stds.csv"))
  s1_concentration <- 1/50
  
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
    #### Reference Fit 
    ##########################################################################################################
    
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
    
    ##########################################################################################################
    #### Apply conversion  
    ##########################################################################################################
    
    eth_qa_sc <- subset_data %>% 
      filter(type.letter == "S") %>% 
      pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
      dplyr::mutate(dilution = 2 ^ (-as.numeric(gsub( # 2 = dilution factor 
        "\\D", "", .data$`Sample`
      )) + 1))  %>% 
      dplyr::group_by(.data$antigen) %>% 
      tidyr::nest()
    
    eth_qa_mfi <- subset_data %>% 
      filter(type.letter == "U") %>% 
      pivot_longer(-c(Sample, Location, Plate, type.letter), names_to = "antigen", values_to = "mfi") %>% 
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
          dilution = convert_between_curves(.data$mfi, new_fit, eth_fit, png_fit)
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
      right_join(estimate_eth, by = "Sample")
    
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
    row_to_match <- row_to_match %>% distinct(SampleID, Location.2, .keep_all = T) %>% na.omit()
    
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
      dplyr::select(-mfi) %>%
      pivot_wider(names_from = "antigen", values_from = "dilution") %>% 
      rename_with(~paste0(.x, "_MFI"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_locations_dilutions <- eth_converted_locations %>%
      dplyr::select(-mfi) %>%
      pivot_wider(names_from = "antigen", values_from = "dilution") %>% 
      rename_with(~paste0(.x, "_Dilution"), -c(SampleID, Location.2, Location, Sample, Plate))
    eth_converted_wide <- eth_converted_locations_mfi %>% left_join(eth_converted_locations_dilutions, by = c("SampleID", "Location.2", "Location", "Sample", "Plate"))
    
    ##########################################################################################################
    #### Create output dataframes
    ##########################################################################################################
    # Save just MFI and RAU for downstream analyses
    col_selection <- grepl("SampleID|Plate|_MFI|\\_Dilution$", colnames(eth_converted_wide))
    MFI_RAU_results <- eth_converted_wide[, col_selection]
    
    # Store results and models for current plate: `results_all` and `model_results_all` store all results and model plots for each plate.
    results_all[[plate_level]] <- eth_converted_wide
    model_results_all[[plate_level]] <- model_results
    MFI_RAU_results_all[[plate_level]] <- MFI_RAU_results
    
  }
  
  ##########################################################################################################
  #### Final output joining all plate data 
  ##########################################################################################################
  final_results <- dplyr::bind_rows(results_all)
  final_MFI_RAU_results <- dplyr::bind_rows(MFI_RAU_results_all)
  
  return(list(final_results, final_MFI_RAU_results, model_results_all))
}

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
# Usage: plotModel(mfi_to_rau_output, antigens_output)
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

plotModel_PNG <- function(mfi_to_rau_output, antigens_output){
  
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
  stds_file <- antigens_output$stds
  stds_log <- 
    stds_file %>%
    mutate(across(-c(Location, Sample, Plate), ~ as.numeric(.))) %>% 
    pivot_longer(-c(Location, Sample, Plate), names_to = "Antigen", values_to = "StdCurve") %>%
    mutate(dilution = ifelse(
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
  
  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot(data = subset(combined_data, Plate == plate_name), 
           aes(x = dilution, y = `1`, color = Antigen)) +  # Use 'Antigen' to differentiate lines
      geom_line() +
      scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      scale_y_log10() +
      geom_point(data = subset(stds_log, Plate == plate_name), aes(x = dilution, y = StdCurve, color = Antigen)) +
      labs(x = "Antibody Dilution",
           y = "Standard Curve (log(MFI))",
           title = paste("Standard Curves for Plate:", plate_name)) +
      theme_bw() +
      facet_wrap(~ Antigen, scales = "free")  # Create a separate plot for each Antigen
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
# Usage: plotModel(mfi_to_rau_output, antigens_output)
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

plotModel_ETH <- function(mfi_to_rau_output, antigens_output){
  
  # Load model results 
  model_results <- mfi_to_rau_output[[3]]
  
  # Convert the list of data frames into a single data frame
  combined_data <- model_results %>% 
    bind_rows(.id = "Plate")
  
  # Generate plots for each plate, grouping antigens together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot(data = subset(combined_data, Plate == plate_name), 
           aes(x = dilution, y = mfi, color = antigen)) +  # Use 'Antigen' to differentiate lines
      geom_line() +
      scale_x_log10() +
      scale_y_log10() +
      geom_point(data = subset(combined_data, Plate == plate_name), aes(x = dilution, y = mfi_pred, color = antigen)) +
      labs(x = "Antibody Dilution",
           y = "Standard Curve (log(MFI))",
           fill = "Antigen",
           title = paste("Standard Curves for Plate:", plate_name)) +
      theme_bw() +
      facet_wrap(~ antigen, scales = "free_y")  # Create a separate plot for each Antigen
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

classify_final_results <- function(mfi_to_rau_output, algorithm_type, Sens_Spec) {
  
  rau_data <- mfi_to_rau_output[[2]]
  rau_data <- rau_data %>%
    dplyr::select(SampleID, Plate, ends_with("_Dilution")) %>%
    mutate(across(ends_with("_Dilution"), as.numeric)) %>%    # Convert only "_Dilution" columns to numeric
    rename_with(~ str_replace(., "_Dilution$", ""), ends_with("_Dilution")) # Remove the "_Dilution" suffix
  
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
  # Classify rau_data using the specified model
  class_preds <- predict(model, new_data = rau_data)
  prob_preds <- predict(model, new_data = rau_data, type = "prob")
  # Binds predictions to rau_data
  results <- rau_data %>% bind_cols(class_preds, prob_preds)
  # Classify new (seropositive) / old (seronegative) based on selected threshold
  results <- results %>%
    mutate(pred_class_max = ifelse(.pred_new > threshold, "new", "old"),
           pred_class_max = as.factor(pred_class_max))
  # Final processing and renaming
  final_results <- results %>%
    dplyr::select(-c(.pred_class, .pred_new, .pred_old)) %>%
    mutate(pred_class_max = recode(pred_class_max, "new" = "seropositive", "old" = "seronegative")) 
  
  # Step 5: Return the table of prediction classes
  return(final_results)
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
    filter(Sens_Spec == selected_threshold) %>% 
    pivot_longer(-c(SampleID, Plate, pred_class_max, Sens_Spec), names_to = "Antigen", values_to = "RAU") %>%
    mutate(pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))) %>%
    ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
    geom_boxplot() +
    scale_y_log10() +
    scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
    labs(title = paste0("Threshold Chosen: "), selected_threshold, 
         x = "Classification", y = "RAU", fill = "Classification") +
    facet_grid(~Antigen) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
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
    rename_with(~str_replace(., "_MFI", ""), ends_with("_MFI")) %>%
    pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "MFI") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number 
           MFI = as.numeric(MFI)) 
  
  if (location == "PNG"){
    
    df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_MFI.csv"))
    
    plot <- df_results %>% 
      ggplot(aes(x= Antigen, y = MFI)) +
      geom_boxplot(data = df_wehi, aes(x = Antigen, y = MFI), fill = "grey", colour = "darkgrey") + 
      geom_boxplot(aes(fill = Antigen)) +
      scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, 10000), labels = c("10", "100", "1,000", "10,000")) +
      scale_fill_brewer(palette = "Paired", type = "qual") +
      labs(x = "Antigen", y = "Antibody log(MFI)") +
      facet_wrap( ~ Plate) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "none") 
    
  } else if (location == "ETH") {
    
    plot <- df_results %>% 
      ggplot(aes(x= Antigen, y = MFI, fill = Antigen)) +
      geom_boxplot() +
      scale_y_log10() +
      scale_fill_brewer(palette = "Paired", type = "qual") +
      labs(x = "Antigen", y = "Antibody log(MFI)") +
      facet_wrap( ~ Plate) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "none") 
    
  }
  
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
    rename_with(~str_replace(., "_Dilution", ""), ends_with("_Dilution")) %>%
    pivot_longer(-c(SampleID, Plate), names_to = "Antigen", values_to = "RAU") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # Reorder by plate number 
           RAU = as.numeric(RAU)) 
  
  if (location == "PNG"){
    
    df_wehi <- read.csv(here::here("data/wehi_compare_data/longitudinal_RAU.csv"))
    
    plot <- df_results %>%
      ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      geom_boxplot(data = df_wehi, aes(x = Antigen, y = RAU), fill = "grey", colour = "darkgrey") +
      geom_boxplot() +
      scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      scale_fill_brewer(palette = "Paired", type = "qual") +
      labs(x = "Antigen", y = "Antibody RAU") +
      facet_wrap( ~ Plate) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if (location == "ETH") {
    
    plot <- df_results %>%
      ggplot(aes(x= Antigen, y = RAU, fill = Antigen)) +
      geom_boxplot() +
      scale_y_log10() +
      scale_fill_brewer(palette = "Paired", type = "qual") +
      labs(x = "Antigen", y = "Antibody RAU") +
      facet_wrap( ~ Plate) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  }
  
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
#   - antigen_output: Output from `readAntigens` (reactive).
#   - plate_layout_file: An ".xlsx" file with sheets labelled plate1, plate2... 
#     etc. (reactive).
#
# Output:
#   - Dot plot with values > 15 threshold coloured in blue (sufficient beads) 
#     and ≤15 beads coloured in red (repeat) faceted by each antigen (ggplot).
# 
# Author: Dionne Argyropoulos
##############################################################################

plotBeadCounts <- function(antigen_output, plate_layout){
  
  master_file <- antigen_output
  counts <- master_file$counts
  counts <- counts %>%
    dplyr::select(-Sample) %>% # can maybe "keep" relevant columns + protein names?
    dplyr::mutate(Location=gsub(".*,", "", Location)) %>%
    dplyr::mutate(Location=substr(Location, 1, nchar(Location)-1))  %>% 
    tidyr::pivot_longer(-c(Location, Plate), names_to = "Antigen", values_to = "Count") %>% 
    dplyr::mutate(Warning = case_when(
      as.numeric(Count)<15~1,
      as.numeric(Count)>=15~0
    )) %>%
    dplyr::select(Location, Antigen, Warning, Count, Plate) %>%
    dplyr::group_by(Location, Antigen, Count, Plate) %>%
    dplyr::summarise(Sum = sum(Warning)) %>%
    dplyr::mutate(Colour = case_when(
      Sum>=1 ~ "repeat",
      Sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(Row = substr(Location, 1, nchar(Location)-1)) %>%
    dplyr::mutate(Col = substr(Location, 2, nchar(Location))) %>%
    dplyr::mutate(Row = gsub("1", "", Row)) %>%
    dplyr::mutate(Row = as.factor(Row)) %>%
    dplyr::mutate(Col = as.numeric(Col))
  
  bead_counts <- counts %>% 
    mutate(Count = as.numeric(Count), 
           Repeat = factor(Colour, levels = c("sufficient beads", "repeat"))) 
  bead_counts$Plate <- factor(bead_counts$Plate, levels = unique(bead_counts$Plate[order(as.numeric(str_extract(bead_counts$Plate, "\\d+")))])) # reorder by plate number 
  
  check_repeats_output <- check_repeats(bead_counts)
  table <- bead_counts
  layout <- readPlateLayout(plate_layout, antigen_output)
  
  # Extract the row and column information from the 'location' column in table
  table$Row <- substr(table$Location, 1, 1)  # Extract row (e.g., 'A')
  table$Col <- substr(table$Location, 2, 2)  # Extract column (e.g., '1')
  
  # Function to extract Sample based on plate name and row/col
  get_sample_id <- function(plate_name, Row, Col) {
    # Get the platelayout data frame based on the plate name
    platelayout_df <- layout[[plate_name]]
    # Find the correct row and column in platelayout
    row_index <- which(platelayout_df$Plate == Row)
    col_index <- as.integer(Col) + 1  # Adding 1 because platelayout has column names as strings
    # Extract the corresponding Sample
    return(platelayout_df[row_index, col_index])
  }
  
  # Apply the function to extract Sample for each row in table
  table$Sample <- mapply(function(Plate, Row, Col) {
    get_sample_id(Plate, Row, Col)
  }, table$Plate, table$Row, table$Col)
  
  table <- table %>% ungroup() %>% dplyr::select(Sample, Location, Antigen, Plate, Colour, Count)
  bead_counts_1 <- bead_counts %>% left_join(table, by = c("Plate", "Count", "Antigen", "Location"))
  
  bead_counts_1 %>% 
    ggplot(aes(Plate, Count, colour = Repeat, alpha = Repeat, size = Repeat, 
               text = paste("Sample:", Sample, "<br>Bead Count:", Count, "<br>Location:", Location,"<br>Plate:", Plate))) + 
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
  
}
