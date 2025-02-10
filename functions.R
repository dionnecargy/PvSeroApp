######### FUNCTIONS ##########################################################
# This script stores the functions used in the app 
# Author: Dionne C. Argyropoulos 
##############################################################################

##############################################################################
# readSeroData function
# --------------------------
#
# This function imports the raw data from the Lightcycler 480 machine (eg WEHI)
# and matches the sample names from the plate layout based on their plate/well
# location. This function has been amended by Dionne to include MAGPIX input 
# as well as BIOPLEX input files. 
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .txt filename 
#   - platform: "magpix" or "bioplex" (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

readSeroData <- function(raw_data, raw_data_filenames, platform){
  
  # Load platemap
  platemap <- read.csv(here::here("data/platemap.csv"))
  
  # Store processed files
  master_list <- list(
    data_raw  = NULL,  # Placeholder for raw data combined across files
    results   = NULL,  # Placeholder for processed results combined
    counts    = NULL,  # Placeholder for any count data combined
    blanks    = NULL,  # Placeholder for any blanks data combined
    stds      = NULL,  # Placeholder for any stds data combined
    run       = NULL   # Placeholder for any run data combined
  )
  
  all_raw_data_list <- list(
    file_paths = raw_data,
    file_names = raw_data_filenames
  )
  
  # Loop through each file and process accordingly
  for (i in seq_along(raw_data)) {
    file <- raw_data[i]
    file_name <- raw_data_filenames[i]
    
    if (platform == "magpix") { 
      
      file_extension <- tools::file_ext(file)  # Identify the file extension and read the file accordingly
      
      if (file_extension == "xlsx") {
        full <- readxl::read_excel(file)
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
          full <- readr::read_csv2(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE) # Read in the data
        } else {
          full <- readr::read_csv(file, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE) # Read in the data
        }
        
        df <- as.data.frame(full) %>% janitor::row_to_names(row_number = 1)
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
      
      # Remove blank rows and preprocess results
      blank_row_number <- which(rowSums(is.na(results)) == length(names(results)))[1] # Handle blank rows
      if (!is.na(blank_row_number)) {
        results <- results[1:(blank_row_number - 1), ]
      }
      
      # 2. Create results
      results <- results %>%
        dplyr::select(-`Total Events`) %>%
        mutate(across(everything(), ~ gsub("NaN", 0, .))) %>% # Change "NaN" to 0s
        mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), Sample)) # Sequentially relabel Blank rows and keep other Sample values unchanged
      
      # 3. Load counts for QC 
      counts <- counts %>%
        mutate(Sample = ifelse(Sample == "Blank", paste0("Blank", row_number()), Sample)) %>% # Sequentially relabel Blank rows and keep other Sample values unchanged
        dplyr::select(-`Total Events`)
      counts <- as_tibble(counts)
      
      # 4. Save blanks
      blanks <- results %>% dplyr::filter(grepl("Blank", Sample, ignore.case = TRUE))
      
      # 5. Save standards
      stds <- results %>% dplyr::filter(grepl("^S", Sample, ignore.case = TRUE))
      
      # 6. Save run info 
      run_info <- as.data.frame(run) %>% dplyr::select(Program:xPONENT)
      
      # Save the plate number for this file 
      plate_numbers <- file_name %>% str_extract("plate\\d+")
      
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
        
        full <- readxl::read_excel(file)
        df <- as.data.frame(full) 
        
      } else if (file_extension == "csv") {
        
        # Check for the delimiter
        first_lines <- readLines(file, n = 5)
        
        if (all(grepl(";", first_lines))) {
          full <- read.csv2(file)
          df <- as.data.frame(full)
        } else {
          full <- read.csv(file)
          df <- as.data.frame(full)
        }
        
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
      
      # Save the plate number for this file 
      plate_numbers <- file_name %>% str_extract("plate\\d+")
      
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
      stop("Unsupported file type. Please use either Magpix or Bioplex")
    }
    
    
  }
  
  return(master_list)
}

##############################################################################
# Read Antigen Names 
# --------------------------
#
# User forced to choose antigens from a set list and ensures that raw data 
# adheres to our format. 
# 
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#
# OUTPUT:
#   - Data frame with relabelled column names for our antigen names 
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
      str_detect(colnames(df), regex("(P87|RBP2b-P87|RBP2b)", ignore_case = TRUE)) ~ "RBP2b.P87",
      str_detect(colnames(df), regex("(PTEX150|L18)", ignore_case = TRUE)) ~ "PTEX150",
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
# readPlateLayout function
# --------------------------
#
# This function imports the plate layout 
#  
#
# PARAMETERS: 
#   - plate_layout_file: string with the plate layout .xlsx or .csv filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

readPlateLayout <- function(plate_layout) {
  
  # Step 1: Get the sheet names to confirm
  sheet_names <- getSheetNames(plate_layout)
  
  # Step 2: Read all sheets into plate_layout_list using indices
  plate_layout_list <- lapply(1:length(sheet_names), function(i) {
    read.xlsx(plate_layout, sheet = i)
  })
  
  # Step 3: Name each element in the list after the corresponding sheet name
  names(plate_layout_list) <- sheet_names
  
  return(plate_layout_list)
}

##############################################################################
# getCounts function
# --------------------------
#
# This function gets the count data 
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

getCounts <- function(antigen_output){
  
  master_file <- antigen_output
  counts <- master_file$counts
  
  counts <- counts %>%
    clean_names() %>% 
    dplyr::select(-sample) %>% # can maybe "keep" relevant columns + protein names?
    dplyr::mutate(location=gsub(".*,", "", location)) %>%
    dplyr::mutate(location=substr(location, 1, nchar(location)-1))  %>% 
    tidyr::pivot_longer(-c(location, plate), names_to = "protein", values_to = "count") %>% 
    dplyr::mutate(warning = case_when(
      as.numeric(count)<15~1,
      as.numeric(count)>=15~0
    )) %>%
    dplyr::select(location, warning, plate) %>%
    dplyr::group_by(location, plate) %>%
    dplyr::summarise(sum = sum(warning)) %>%
    dplyr::mutate(colour = case_when(
      sum>=1 ~ "repeat",
      sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(row = substr(location, 1, nchar(location)-1)) %>%
    dplyr::mutate(col = substr(location, 2, nchar(location))) %>%
    dplyr::mutate(row = gsub("1", "", row)) %>%
    dplyr::mutate(row = as.factor(row)) %>%
    dplyr::mutate(col = as.numeric(col))
  
  return(counts)
}

##############################################################################
# plotCounts function
# --------------------------
#
# This function gets the count data and plots the plate image - DA added multiple
# facets for each plot so that different plates can be visualised. 
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotCounts <- function(counts_output, experiment_name){
  bead_counts <- counts_output
  bead_counts$plate <- factor(bead_counts$plate, levels = unique(bead_counts$plate[order(as.numeric(str_extract(bead_counts$plate, "\\d+")))])) # reorder by plate number 
  bead_counts %>% 
    ggplot(mapping = aes(x = col, y = fct_rev(row), fill = colour), fill = summary) +
    geom_tile(aes(height = 0.90, width = 0.90)) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       position = "top") +
    scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"), drop=FALSE) +
    theme_bw() +
    labs(x = "", y = "", title = experiment_name , fill = "") +
    facet_wrap(~ plate, scales = "free_y")  # This will create separate facets for each level of 'plate'
}

##############################################################################
# check_repeats function
# --------------------------
#
# This function gets the count data and outputs a table of the isolates to 
# repeat or a statement to confirm that none need to be repeated.
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#
# OUTPUT:
#   - Data frame with list of isolates to repeat
##############################################################################

check_repeats <- function(counts_output) {
  bead_counts <- counts_output
  repeat_rows <- bead_counts %>% filter(colour == "repeat")
  if (nrow(repeat_rows) == 0) {
    return("No repeats necessary.")
  } else {
    return(repeat_rows)
  }
}

##############################################################################
# plotBlanks function
# --------------------------
#
# This function gets the blank sample data and plots the blank sample MFI values
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#   - experiment_name: string with the experimet name (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotBlanks <- function(antigen_output, experiment_name){
  master_file <- antigen_output
  blanks <- master_file$blanks
  blanks %>% 
    dplyr::select(-Location) %>% 
    pivot_longer(-c(Sample, Plate), names_to = "protein", values_to = "MFI") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))]))) %>% # reorder by plate number 
    ggplot(aes(x = factor(protein), y = as.numeric(MFI), fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    labs(x = "Protein", 
         y = "MFI",
         title = experiment_name) +
    # theme_linedraw() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ Plate)  # This will create separate facets for each level of 'plate'
}

############################################################################## 
# plotStds function
# --------------------------
#
# This function gets the standards data and plots the standard curves
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#   - experiment_name: string with the experimet name (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotStds <- function(antigen_output, experiment_name){
  master_file <- antigen_output
  stds <- master_file$stds
  gg <- stds %>% 
    dplyr::select(-Location) %>% 
    pivot_longer(-c(Sample, Plate), names_to = "protein", values_to = "MFI") %>% 
    mutate(Plate = factor(Plate, levels = unique(Plate[order(as.numeric(str_extract(Plate, "\\d+")))])), # reorder by plate number 
           Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")), 
           MFI = as.numeric(MFI)) %>% 
    ggplot(aes(x = Sample, y = MFI, color = Plate, group = Plate)) + 
    geom_point() + 
    geom_line() +
    scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    labs(x = "Standard Curve", 
         y = "log(MFI)",
         title = experiment_name) +
    facet_wrap(~protein) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

##############################################################################
# MFItoRAU function
# --------------------------
#
# This function fits a 5-parameter logistic standard curve to the dilutions
# of the positive controls for each protein and converts the MFI values 
# into relative antibody units (RAU)
# Written by Connie Li Wai Suen
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#   - plate_layout_file: string with the plate layout .xlsx filename 
#
# OUTPUT:
#   - Data frame with converted RAU data
##############################################################################

MFItoRAU <- function(antigen_output, plate_layout){
  
  master_file <- antigen_output
  L <- master_file$results
  layout <- readPlateLayout(plate_layout)
  proteins <- c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS")
  
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
    
    # Iterate over proteins
    for (i in proteins){
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
    row_to_match <- row_to_match %>% distinct(SampleID, .keep_all = T)
    
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
    #### OUTPUT
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
# plotModel function
# --------------------------
#
# This function gets the model results data and plots the model fits
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#   - plate_layout_file: string with the plate layout .xlsx filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotModel <- function(mfi_to_rau_output, antigens_output){
  
  model_results <- mfi_to_rau_output[[3]]
  
  # Create a combined data frame with plate and protein
  combined_data <- do.call(rbind, lapply(names(model_results), function(file_name) {
    # Get the list of proteins for this file
    proteins <- model_results[[file_name]]
    # For each protein in the file
    lapply(names(proteins), function(protein_name) {
      data <- proteins[[protein_name]]  # Get the protein's data frame
      data$Plate <- file_name  # Add plate column will be the file name
      data$protein <- protein_name  # Add protein column will be the protein name
      return(data)
    })
  }))
  
  # Convert the list of data frames into a single data frame
  combined_data <- do.call(rbind, combined_data)
  
  ### Get Standards for points
  stds_file <- antigens_output$stds
  stds_log <- 
    stds_file %>%
    mutate(across(-c(Location, Sample, Plate), ~ log(as.numeric(.)))) %>% 
    pivot_longer(-c(Location, Sample, Plate), names_to = "protein", values_to = "stdcurve") %>%
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
  
  # Generate plots for each plate, grouping proteins together
  plots_model <- lapply(unique(combined_data$Plate), function(plate_name) {
    ggplot(data = subset(combined_data, Plate == plate_name), 
           aes(x = dilution, y = `1`, color = protein)) +  # Use 'protein' to differentiate lines
      geom_line() +
      scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      geom_point(data = subset(stds_log, Plate == plate_name), aes(x = dilution, y = stdcurve, color = protein)) +
      labs(x = "Antibody Dilution",
           y = "Standard Curve",
           title = paste("Standard Curves for Plate:", plate_name)) +
      theme_bw() +
      facet_wrap(~ protein, scales = "free")  # Create a separate plot for each protein
  })
  
  # Assign names to the list of plots for clarity
  names(plots_model) <- unique(combined_data$Plate)
  
  # Arrange all plots using grid.arrange
  # arranged_plots <- do.call(grid.arrange, c(plots_model, ncol = 1))  # Stack plots vertically
  
  return(plots_model)
}

##############################################################################
# classify_final_results
# --------------------------
#
# This function classifies unknown samples as recently exposed or not 
# (Note: MFItoRAU() needs to be run first to convert to RAU)
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx or .csv filename 
#   - platform: "magpix" or "bioplex" (reactive)
#   - algorithm: user-selected algorithm choice
#
# OUTPUT:
#   - Data frame with exposure status for every sample
#   - Summary table with positive/negative results for each classifier
##############################################################################

classify_final_results <- function(mfi_to_rau_output, algorithm_type, Sens_Spec) {
  
  rau_data <- mfi_to_rau_output[[2]]
  rau_data <- rau_data %>%
    mutate(rowname = base::paste(SampleID, Plate, sep = "_")) %>% # Combine SampleID and plate into a single rowname
    column_to_rownames(var = "rowname") %>%  # Convert to rownames
    dplyr::select(contains("_Dilution")) %>% # Select relevant columns
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
    mutate(pred_class_max = recode(pred_class_max, "new" = "seropositive", "old" = "seronegative")) %>%
    rownames_to_column(var = "rowname") %>%
    separate(rowname, into = c("SampleID", "plate"), sep = "_") # Split back into two columns
  
  # Step 5: Return the table of prediction classes
  return(final_results)
}

##############################################################################
# plotBoxPlotClassification function
# --------------------------
#
# This function plots the RAU values for each protein based on a selected  
# threshold based on the row chosen: "maximised", "85% sensitivity", 
# "90% sensitivity", 
# "95% sensitivity","85% specificity", "90% specificity", "95% specificity". 
#
# PARAMETERS: 
#   - all_classifications: df of classify_final_results() for all thresholds
#   - selected_threshold: string with the threshold (reactive)
#
# OUTPUT:
#   - Box plots with MFI values for each protein
##############################################################################

plotBoxPlotClassification <- function(all_classifications, selected_threshold){
  
  all_classifications %>% 
      filter(Sens_Spec == selected_threshold) %>% 
      pivot_longer(-c(SampleID, plate, pred_class_max, Sens_Spec), names_to = "protein", values_to = "RAU") %>%
      mutate(pred_class_max = factor(pred_class_max, levels = c("seronegative", "seropositive"))) %>%
      ggplot(aes(x = pred_class_max, y = RAU, fill = pred_class_max)) +
      geom_boxplot() +
      scale_y_log10() +
      scale_fill_manual(values = c(seronegative = "#878787", seropositive = "#d6604d")) +
      labs(title = paste0("Threshold Chosen: "), selected_threshold, 
           x = "Classification", y = "RAU", fill = "Classification") +
      facet_grid(~protein) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
