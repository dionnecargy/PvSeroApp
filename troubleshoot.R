file_paths <- c(
  "data/example_MAGPIX_plate1.csv",
  "data/example_MAGPIX_plate2.csv",
  "data/example_MAGPIX_plate3.csv"
)

file_names <- c(
  "example_MAGPIX_plate1.csv",
  "example_MAGPIX_plate2.csv",
  "example_MAGPIX_plate3.csv"
)

plate_layout <- c("data/example_platelayout_1.xlsx")

serodata <- readSeroData(file_paths, file_names, "magpix")
antigens <- readAntigens(serodata)

rau <- MFItoRAU(antigens, plate_layout)

classification <- classify_final_results(rau, "antibody_model", "maximised")


#### MFI to RAU 
antigen_output <- antigens
plate_layout <- plate_layout

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
