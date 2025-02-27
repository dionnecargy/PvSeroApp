###############################################################################
###### Load Packages and Functions
###############################################################################

require(shiny)
require(shiny.fluent)
require(shiny.react)
require(shinyjs)
require(htmltools)
require(workflowsets)
require(plotly)
require(tidyverse)
require(ggpubr)
require(janitor)
require(DT)
require(rmarkdown)
require(shinyWidgets)
require(here)
require(RColorBrewer)
require(readxl)
require(openxlsx)
require(glue)
require(drc)
require(gt)
require(rsconnect)
require(httr)
require(jsonlite)

source("functions.R")
source("content.R")

options(repos = c(CRAN = "https://cloud.r-project.org/"))
antibody_model <- readRDS(here::here("model/PvSeroTaTmodel.rds"))
antibody_model_excLF016 <- readRDS(here::here("model/random_forest_excludingLF016.rds"))
platemap <- read.csv(here::here("data/platemap.csv"))

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

get_github_release <- function(repo_owner, repo_name) {
  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/releases/latest")
  response <- httr::GET(url)
  
  if (status_code(response) == 200) {
    release_info <- fromJSON(content(response, "text"))
    return(release_info$tag_name)  # Extracts the tag name (release version)
  } else {
    return(NULL)
  }
}


###############################################################################
###### Server
###############################################################################
shinyServer(function(input, output, session){
  
  dark_mode <- reactiveVal(FALSE)  # Track theme state
  
  observeEvent(input$toggle_theme, {
    dark_mode(!dark_mode())  # Toggle state
    session$sendCustomMessage("toggle-theme", dark_mode()) # Send a message to update the CSS theme
  })
  
  # Get release version dynamically
  release_version <- get_github_release("dionnecargy", "pvseroapp")
  
  # Define footer content
  version <- reactive({
    if (!is.null(release_version)) {
      release_version  # Display version
    } else {
      "Version info not available."
    }
  })
  
  # Define the footer with version info
  output$footer_version <- renderText({
    version_text <- if (!is.null(release_version)) {
      paste("© 2025 PvSeroApp", release_version)
    } else {
      "© 2025 PvSeroApp version info not available"
    }
    version_text
  })
  
  ###############################################################################
  # ------------ PAGES ------------
  ###############################################################################
  
  output$page_content <- renderUI({
    hash <- session$clientData$url_hash # Check the value of the URL hash (based on user navigation)
    
    if (is.null(hash) || hash == "") {hash <- "#home"}  # Default to the "home" page if no hash
    
    # Render content from content.R 
    if (hash == "#home") {
      home_page() 
    } else if (startsWith(hash, "#tutorial")) {
      tutorial_page()
    } else if (hash == "#algorithm") {
      algorithm_page()
    } else if (hash == "#input") {
      input_page()
    } else if (hash == "#check") {
      check_page()
    } else if (hash == "#model") {
      model_page()
    } else if (hash == "#datavis") {
      datavis_page()
    } else {
      return(home_page())  # Default page if no page is specified
    }
  })
  
  ###############################################################################
  # ------------ TUTORIAL ------------
  ###############################################################################
  
  # table example for how antigens should be included.
  output$antigens <- renderDT({
    example_data <- data.frame(
      `Antigen Names` = c("EBP", "LF005", "LF010", "LF016", "MSP8", "RBP2b.P87", "PTEX150", "PvCSS"), 
      `Other Accepted Options` = c("EBP-II, PvEBP-II", "", "", "", "PvMSP8, L34", "RBP2b-P87, RBP2b, P87", "PTEx150, L18", "CSS, css")
    )
    colnames(example_data) <- c("Antigen Names", "Other Accepted Options")
    datatable(example_data, 
              options = list(dom = 't',                    # 't' means only the table (no pagination, search, etc.)
                             searching = FALSE,            # Disable search box
                             paging  = FALSE,              # Disable pages box
                             lengthChange = FALSE          # Disable number of entries dropdown
              ), 
              rownames = FALSE)                            # Remove row numbers
  })
  
  # table example for how standards should be included. 
  output$standards <- renderDT({
    example_standards <- data.frame(
      `Standard Label` = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"), 
      `Dilution` =  c("1/50", "1/100", "1/200", "1/400", "1/800", "1/1600", "1/3200", "1/6400", "1/12800", "1/25600")
    )
    # Transpose and create the desired tibble
    example_standards_t <- t(example_standards)
    
    # Convert to a tibble with proper row names
    example_standards_final <- as_tibble(example_standards_t, .name_repair = "minimal")
    
    datatable(example_standards_final, 
              options = list(dom = 't',                    # 't' means only the table (no pagination, search, etc.)
                             searching = FALSE,            # Disable search box
                             paging  = FALSE,              # Disable pages box
                             lengthChange = FALSE,         # Disable number of entries dropdown
                             ordering = FALSE              # No column sorting
              ), 
              rownames = FALSE)                            # Remove row numbers
  })
  
  # APP RESPONSE: Read imported plate layout file and print template
  plate_image_list <- reactive({
    list(
      list(
        src = "www/2_tutorial/plate_layout_1.png",
        style = "max-width: 100%; height: auto; display: block; margin: 0 auto;",
        imageFit = "container"
      ),
      list(
        src = "www/2_tutorial/plate_layout_2.png",
        style = "max-width: 100%; height: auto; display: block; margin: 0 auto;",
        imageFit = "container"
      ),
      list(
        src = "www/2_tutorial/plate_layout_3.png",
        style = "max-width: 100%; height: auto; display: block; margin: 0 auto;",
        imageFit = "container"
      ),
      list(
        src = "www/2_tutorial/plate_layout_4.png",
        style = "max-width: 100%; height: auto; display: block; margin: 0 auto;",
        imageFit = "container"
      )
    )
  })
  # Track the current plate index
  current_plate_image <- reactiveVal(1)
  # Get the total number of plates
  total_plate_image <- reactive({
    length(plate_image_list())
  })
  # Next button
  observeEvent(input$inc_plate_image, {
    new_index <- current_plate_image() + 1
    if (new_index <= total_plate_image()) {
      current_plate_image(new_index)
    }
  })
  # Previous button
  observeEvent(input$dec_plate_image, {
    new_index <- current_plate_image() - 1
    if (new_index >= 1) {
      current_plate_image(new_index)
    }
  })
  # Render the selected plate
  output$individual_plate_image <- renderImage({
    req(plate_image_list(), current_plate_image())
    plate_image_list()[[current_plate_image()]]
  }, deleteFile=FALSE)
  
  ###############################################################################
  # ------------ ALGORITHM ------------
  ###############################################################################
  
  # how the model was trained and where the samples were from 
  output$methods1 <- renderDT({
    table1 <- data.frame(
      `Source/Country` = c("Brazil", "Thailand", "Solomon Islands", "Volunteer Blood Donor Registry", 
                           "Australian Red Cross", "Thai Red Cross", "Brazil Blood Donor Registry"), 
      Type = c("Year-long cohort study", "Year-long cohort study", "Year-long cohort study", 
               "Negative control", "Negative control", "Negative control", "Negative control"),
      `No. samples` = c("886", "680", "709", "98", "97", "69", "96")
    )
    colnames(table1) <- c("Source/Country", "Type", "No. samples")
    datatable(table1, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                'Table 1: Description of sample size for training and testing dataset, including plasma samples from year-long observational cohort studies in malaria-endemic regions of Brazil, 
                             Thailand, and the Solomon Islands, as well as negative controls from Australia, Thailand and Brazil.'
              ),
              options = list(dom = 't',                    # 't' means only the table (no pagination, search, etc.)
                             searching = FALSE,            # Disable search box
                             paging  = FALSE,              # Disable pages box
                             lengthChange = FALSE          # Disable number of entries dropdown
              ), 
              rownames = FALSE)                            # Remove row numbers
  })
  # the antigens included in the final model 
  output$methods2 <- renderDT({
    # Sys.setlocale(category = "LC_ALL", locale = "Greek")
    table2 <- data.frame(
      Antigen = c("RBP2b", "MSP1-19", "Pv-fam-a", "MSP5", "EBP", "PTEX150", "PvCSS", "MSP8"), 
      `Gene ID (PlasmoDB)` = c("PVX_094255", "PVX_099980", "PVX_096995", "PVX_003770", "KMZ83376.1", "PVX_084720", "PVX_086200", "PVX_097625")
    )
    colnames(table2) <- c("Antigen", "Gene ID (PlasmoDB)")
    datatable(table2, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                'Table 2. ', htmltools::em('P. vivax'), 'proteins considered in the selection of our serological exposure markers.'
              ),
              options = list(dom = 't',                    # 't' means only the table (no pagination, search, etc.)
                             searching = FALSE,            # Disable search box
                             paging  = FALSE,              # Disable pages box
                             lengthChange = FALSE          # Disable number of entries dropdown
              ), 
              rownames = FALSE)                            # Remove row numbers
  })
  
  output$methods3 <- render_gt({
    table3 <- data.frame(
      Prediction = c("new", "old"), 
      new = c("366", "414"), 
      old = c("86", "1,769")
    )
    table3 %>% 
      gt() %>% 
      tab_spanner(label = "True", columns = 2:3)
  })
  
  
  ###############################################################################
  # ------------ INPUT PAGE TUTORIAL ------------
  ###############################################################################
  
  showBubble <- reactiveVal(FALSE)
  current_step <- reactiveVal(1)  # Start with step 1
  
  steps <- list(
    list(
      headline = "Introduction",
      content = "Welcome to the tutorial. Let's get started!"
    ),
    list(
      headline = "Step 1: Write your experiment name",
      content = "Type in your experiment name!"
    ),
    list(
      headline = "Step 2: Add the date",
      content = "Add in the date of your experiment and/or date of running this application!"
    ),
    list(
      headline = "Step 3: Write any additional notes",
      content = "Feel free to write in any notes about your experiment!"
    ),
    list(
      headline = "Step 4: Choose the platform",
      content = "Did you run your experiment on a MAGPIX or BioPlex machine? Please choose the platform here!"
    ),
    list(
      headline = "Step 5: Uploading your files",
      content = "Click the input buttons to add your RAW DATA file/s and your PLATE LAYOUT file!"
    ),
    list(
      headline = "Step 6: Final step",
      content = "Press the button to save all of these data to use throughout the app. If you want to change your inputs, you will have to press this button again!"
    ),
    list(
      headline = "Reset Inputs",
      content = "If you need to add in new inputs and want to ensure that you are starting with a clean slate, press this button and go through Steps 1-6 again!"
    ),
    list(
      headline = "Congratulations 🎉",
      content = "You have completed this tutorial. Well done! Press the > button to exit the window. You can repeat the tutorial by pressing the Step-By-Step Tutorial button again!"
    ),
    list(
      headline = NULL,
      content = NULL
    )
  )
  
  observeEvent(input$toggleTeachingBubble, {
    showBubble(TRUE)
    current_step(1)
  })
  
  observeEvent(input$next_button, {
    new_index <- current_step() + 1
    if (new_index <= length(steps)) {
      current_step(new_index)
    }
    
    # Check if we're at the last step, and if so, close the teaching bubble
    if (current_step() == length(steps)) {
      showBubble(FALSE)  # Close the bubble on the last step
    }
  })
  
  observeEvent(input$previous_button, {
    new_index <- current_step() - 1
    if (new_index >= 1) {
      current_step(new_index)
    }
  })
  
  output$teaching_bubble_ui <- renderReact({
    if (showBubble()) {
      step <- steps[[current_step()]]
      
      # Map steps to actual Shiny input IDs
      target_id <- switch(current_step(),
                          "1" = "#target",  # Reference Shiny input ID directly
                          "2" = "#experiment_name",
                          "3" = "#date",
                          "4" = "#experiment_notes",
                          "5" = "#platform",
                          "6" = "#raw_data",
                          "7" = "#save_inputs",
                          "8" = "#resetAll",
                          "9" = "#target"
      )
      
      req(target_id)
      
      TeachingBubble(
        key = paste0("step_", current_step()),  # Forces re-render
        target = target_id,  # Corrected selector reference
        headline = step$headline,
        children = step$content,
        calloutProps = if (current_step() %in% c(1, 9)) list(directionalHint = 0) else NULL
      )
    }
  })
  
  ###############################################################################
  # ------------ CLASSIFY EXPOSURE PAGE TUTORIAL ------------
  ###############################################################################
  
  showBubble_CE <- reactiveVal(FALSE)
  current_step_CE <- reactiveVal(1)  # Start with step 1
  
  steps_CE <- list(
    list(
      headline = "Step 1: Select the Algorithm",
      content = "The 8-antigen combination includes: EBP, LF005, LF010, LF016, MSP8, RBP2b.P87, PTEX150, PvCSS. The 7-antigen combination includes all of these EXCEPT for LF016 which is known to be difficult to assay."
    ),
    list(
      headline = "Step 2: Select the Sensitivity/Specificity of interest.",
      content = HTML("For further information on each option, please see the <a href='#tutorial' class-`link`>Tutorial<a/>.")
    ),
    list(
      headline = "Step 3: Run Classification Algorithm",
      content = "Press the play button to run the classification based on these parameters 🎉"
    ),
    list(
      headline = "Classification Results",
      content = "The processed results of your data will be summarised in a table here!"
    ),
    list(
      headline = "Download your Files",
      content = "Click the download button to save your FINAL CLASSIFICATION data file!"
    ),
    list(
      headline = "Congratulations 🎉",
      content = "You have completed this tutorial. Well done! Press the > button to exit the window. You can repeat the tutorial by pressing the Step-By-Step Tutorial button again!"
    ),
    list(
      headline = NULL,
      content = NULL
    )
  )
  
  observeEvent(input$toggleTeachingBubble_CE, {
    showBubble_CE(TRUE)
    current_step_CE(1)
  })
  
  observeEvent(input$next_button_CE, {
    new_index <- current_step_CE() + 1
    if (new_index <= length(steps_CE)) {
      current_step_CE(new_index)
    }
    
    # Check if we're at the last step, and if so, close the teaching bubble
    if (current_step_CE() == length(steps_CE)) {
      showBubble_CE(FALSE)  # Close the bubble on the last step
    }
  })
  
  observeEvent(input$previous_button_CE, {
    new_index <- current_step() - 1
    if (new_index >= 1) {
      current_step_CE(new_index)
    }
  })
  
  output$teaching_bubble_CE <- renderReact({
    if (showBubble_CE()) {
      steps_CE <- steps_CE[[current_step_CE()]]
      
      # Map steps to actual Shiny input IDs
      target_id_CE <- switch(current_step_CE(),
                             "1" = "#target_CE",
                             "2" = "#algorithm",  # Reference Shiny input ID directly
                             "3" = "#sens_spec",
                             "4" = "#run_classification",
                             "5" = "#classification_summary",
                             "6" = "#download_classification",
                             "7" = "#target_CE"
      )
      
      req(target_id_CE)
      
      TeachingBubble(
        key = paste0("step_", current_step_CE()),  # Forces re-render
        target = target_id_CE,  # Corrected selector reference
        headline = steps_CE$headline,
        children = steps_CE$content,
        calloutProps = if (current_step_CE() %in% c(1, 7)) list(directionalHint = 0) else NULL
      )
    }
  })
  
  ###############################################################################
  # ------------ RAW DATA INPUT ------------
  ###############################################################################
  
  # Download links for the template/example data
  
  output$template_zip <- downloadHandler(
    filename = function() {
      "example_data.zip"
    },
    content = function(file) {
      file.copy("data/example_data.zip", file)
    }
  )
  
  output$template_excel <- downloadHandler(
    filename = function() {
      "template_platelayout.xlsx"
    },
    content = function(file) {
      file.copy("data/template_platelayout.xlsx", file)
    }
  )
  
  # When files are successfully uploaded, show a success message
  observeEvent(input$raw_data, {
    if (!is.null(input$raw_data)) {
      output$uploadMessage1 <- renderUI({
        MessageBar(
          messageBarType = 4,  # 4 = success
          "Files Imported!"
        )
      })
    }
  })
  observeEvent(input$plate_layout, {
    if (!is.null(input$plate_layout)) {
      output$uploadMessage2 <- renderUI({
        MessageBar(
          messageBarType = 4,  # 4 = success
          "Files Imported!"
        )
      })
    }
  })
  
  ## ----- Reactive User Inputs -----
  
  # USER INPUT 1: Reactive expression to get experiment name 
  experiment_name <- reactive({
    req(input$experiment_name)
    input$experiment_name
  })
  
  # USER INPUT 2: Reactive expression to get date
  date <- reactive({
    req(input$date)
    format(as.Date(input$date), "%d%m%y")  # Convert to ddmmyy format
  })
  
  # USER INPUT 3: Reactive expression to get experiment notes 
  
  # USER INPUT 3: Reactive expression to get experiment notes 
  experiment_notes <- reactive({
    
    text <- if (is.null(input$experiment_notes) || input$experiment_notes == "") {
      "no notes"
    } else {
      input$experiment_notes
    }
    text
    
    print(text)
  })
  
  # USER INPUT 4: Reactive expression to get uploaded files (raw_data)
  raw_data <- reactive({
    req(input$raw_data)
    input$raw_data
  })
  
  # USER INPUT 5: Reactive expression to get filenames of uploaded raw data files
  raw_data_filename <- reactive({
    req(input$raw_data)
    input$raw_data$name
  })
  
  # USER INPUT 6: Reactive expression for platform (bioplex or magpix)
  platform <- reactive({
    req(input$platform)  # Ensure platform is selected
    input$platform
  })
  
  # USER INPUT 7: Reactive expression for plate layout
  plate_layout <- reactive({
    req(input$plate_layout)
    input$plate_layout
  })
  
  # USER INPUT 8: Reactive expression for plate layout file name 
  plate_layout_filename <- reactive({
    req(input$plate_layout)
    input$plate_layout$name
  })
  
  # Reactive values to store user inputs
  app_data <- reactiveValues()
  
  observeEvent(input$save_inputs, {
    req(experiment_name(), date(), raw_data(), raw_data_filename(), platform(), plate_layout())
    
    # Store App Data 
    app_data$experiment_name <- experiment_name()
    app_data$date <- date()
    app_data$raw_data <- raw_data()
    app_data$raw_data_filename <- raw_data_filename()
    app_data$platform <- platform()
    app_data$plate_layout <- plate_layout()
    
    # Relay message that App Data is "Saved"
    output$notification <- renderUI({
      MessageBar(messageBarType = 4, "Inputs saved successfully!")
    })
  })
  
  observeEvent(input$resetAll, {
    runjs("history.go(0)")
  })
  
  # Create reactive expressions to access the data
  experiment_name_reactive <- reactive({ app_data$experiment_name })
  date_reactive <- reactive({ app_data$date })
  raw_data_reactive <- reactive({ app_data$raw_data })
  raw_data_filename_reactive <- reactive({ app_data$raw_data_filename })
  platform_reactive <- reactive({ app_data$platform })
  plate_layout_reactive <- reactive({ app_data$plate_layout })
  
  ## ----- Print Names for Cross-Checking -----
  
  # APP RESPONSE: Print raw file name
  output$raw_data_filename <- renderText({
    paste0("Raw data filename: ", paste(raw_data_filename(), collapse = ", "))
  })
  
  # APP RESPONSE: Print plate layout file name
  output$plate_layout_filename <- renderText({
    paste0("Plate layout filename: ", plate_layout_filename())
  })
  
  # RUN readSeroData only once! 
  serodata_output <- reactive({
    req(raw_data_reactive(), raw_data_filename_reactive()) # Wait for data to be ready
    file_paths <- raw_data_reactive()$datapath
    master_list <- readSeroData(file_paths, raw_data_filename_reactive(), platform_reactive())
  })
  
  # APP RESPONSE: Render the raw data table based on the imported file
  output$alldata <- renderUI({
    req(serodata_output())
    data_raw <- serodata_output()$data_raw
    
    # Convert the dataframe to a list of lists
    items <- apply(data_raw, 1, as.list)
    
    # Dynamically generate columns based on the file's column names
    columns <- lapply(names(data_raw), function(col) {
      list(fieldName = col, name = col, minWidth = 100, maxWidth = 200)
    })
    
    # Create the DetailsList component with height and width adjustments
    div(
      style = "height: auto; max-height: 80vh; overflow-y: auto; padding: 10px; width: 100%; max-width: 1000px;",
      DetailsList(
        items = items,
        columns = columns,
        compact = TRUE,
        checkboxVisibility = 2,
        styles = list(root = list(height = "auto", overflowY = "auto", overflowX = "auto"))
      )
    )
  })
  
  output$runinfo <- renderUI({
    req(serodata_output())
    run <- serodata_output()$run
    
    div(
      style = "max-height: 600px; overflow: auto; width: 100%;",
      DetailsList(
        items = run,
        columns = tibble(fieldName = names(run), name = names(run)),
        compact = TRUE,
        checkboxVisibility = 2,
        styles = list(root = list(height = "auto", overflowY = "auto", overflowX = "auto"))
      )
    )
    
  })
  
  # APP RESPONSE: Read imported plate layout file and print template
  plate_list <- reactive({
    plate_file_path <- plate_layout_reactive()$datapath
    readPlateLayout(plate_file_path)
  })
  # Track the current plate index
  current_plate <- reactiveVal(1)
  # Get the total number of plates
  total_plates <- reactive({
    length(plate_list())
  })
  # Next button
  observeEvent(input$inc_plate, {
    new_index <- current_plate() + 1
    if (new_index <= total_plates()) {
      current_plate(new_index)
    }
  })
  # Previous button
  observeEvent(input$dec_plate, {
    new_index <- current_plate() - 1
    if (new_index >= 1) {
      current_plate(new_index)
    }
  })
  # Render the selected plate
  output$individual_plate <- renderUI({
    req(plate_list())
    
    DetailsList(
      items = plate_list()[[current_plate()]],
      columns = lapply(c("Plate", 1:12), function(col) {
        list(
          name = as.character(col),    # Display name of the column
          fieldName = as.character(col), # Field name in the dataset
          minWidth = 50,               # Optional: Set minimum width for the column
          maxWidth = 50               # Optional: Set maximum width for the column
        )}),
      checkboxVisibility = 2, 
      selectionMode = 0 # Disables selection
    )
    
  })
  
  ###############################################################################
  # ------------ QUALITY CONTROL ------------
  ###############################################################################
  
  ## ----- Create All Outputs (Plots/Data Frames) -----
  # RUN readSeroData only once! 
  antigens_output <- reactive({
    req(serodata_output()) 
    readAntigens(serodata_output())
  })
  
  counts_output <- reactive({
    req(antigens_output())
    getCounts(antigens_output())
  })
  
  # APP RESPONSE: Creating standard curve plot
  location <- reactive({
    value <- ifelse(is.null(input$toggle_png_eth), FALSE, input$toggle_png_eth)
    ifelse(value, "ETH", "PNG")  # Map TRUE to "ETH", FALSE to "PNG"
  })
  
  stdcurve_plot <- reactive({
    req(antigens_output(), location(), experiment_name()) 
    plotStds(antigens_output(), location(), experiment_name())
  })
  
  # APP RESPONSE: Creating plate QC plot
  plateqc_plot <- reactive({
    req(counts_output(), experiment_name())
    plotCounts(counts_output(), experiment_name()) 
  })
  
  check_repeats_output <- reactive({
    req(counts_output())
    check_repeats(counts_output())
  })
  
  # APP RESPONSE: Creating blanks plot
  blanks_plot <- reactive({
    req(antigens_output(), experiment_name())
    plotBlanks(antigens_output(), experiment_name())
  })
  
  # RUN MFItoRAU only once! 
  mfi_to_rau_output <- reactive({
    req(antigens_output(), plate_layout_reactive())
    MFItoRAU(antigen_output = antigens_output(), 
             plate_layout = plate_layout_reactive()$datapath)
  })
  
  # APP RESPONSE: Creating QC model plot
  model_plot <- reactive({
    req(mfi_to_rau_output(), antigens_output())
    plotModel(mfi_to_rau_output(), antigens_output())
  })
  
  ## ----- Display All Outputs (Plots/Data Frames) in APP -----
  # APP RESPONSE: Render QC standard curve plot
  output$stdcurve <- renderPlotly({
    
    gg <- stdcurve_plot()
    
    # Convert to plotly
    plotly_gg <- ggplotly(gg) %>% 
      layout(
        showlegend = TRUE,  # Ensure legend is visible
        font = list(family = "Helvetica", size = 20, colour = "black"),
        legend = list(tracegroupgap = 0)  # Makes sure the proteins can be toggled individually in a group
      )
    plotly_gg
    
  })
  
  # APP RESPONSE: Render plate QC plot
  output$plateqc <- renderPlot({
    # gg <- 
    plateqc_plot()
    # ggplotly(gg) %>% layout(showlegend = TRUE) # Convert to plotly
  })
  
  output$check_repeats_text <- renderText({
    if (is.character(check_repeats_output())) {
      check_repeats_output()
    }
  })
  
  check_repeats_table_format <- reactive({
    req(check_repeats_output(), plate_layout_reactive())
    
    if (is.data.frame(check_repeats_output())) {
      table <- check_repeats_output()
      layout <- readPlateLayout(plate_layout_reactive()$datapath)
      
      # Extract the row and column information from the 'location' column in table
      table$Row <- substr(table$Location, 1, 1)  # Extract row (e.g., 'A')
      table$Col <- substr(table$Location, 2, 2)  # Extract column (e.g., '1')
      
      # Function to extract SampleID based on plate name and Row/Col
      get_sample_id <- function(plate_name, Row, Col) {
        # Get the platelayout data frame based on the plate name
        platelayout_df <- layout[[plate_name]]
        # Find the correct Row and column in platelayout
        row_index <- which(platelayout_df$Plate == Row)
        col_index <- as.integer(Col) + 1  # Adding 1 because platelayout has column names as strings
        # Extract the corresponding SampleID
        return(platelayout_df[row_index, col_index])
      }
      
      # Apply the function to extract SampleID for each row in table
      table$SampleID <- mapply(function(Plate, Row, Col) {
        get_sample_id(Plate, Row, Col)
      }, table$Plate, table$Row, table$Col)
      
      table <- table %>% dplyr::select(SampleID, Location, Plate, Repeat = Colour)
      table
    }
  })
  
  output$check_repeats_table <- DT::renderDataTable({
    req(check_repeats_output(), check_repeats_table_format())
    if (is.data.frame(check_repeats_output())) {
      datatable(check_repeats_table_format(), 
                options = list(dom = 't',                    # 't' means only the table (no pagination, search, etc.)
                               searching = FALSE,            # Disable search box
                               paging  = FALSE,              # Disable pages box
                               lengthChange = FALSE,         # Disable number of entries dropdown
                               scrollX = TRUE
                ), 
                rownames = FALSE)                            # Remove row numbers
      
    }
  })
  
  # APP RESPONSE: Render blanks plot
  output$blanks <- renderPlot({
    # gg <- 
    blanks_plot()
    # ggplotly(gg) %>% layout(showlegend = TRUE) # Convert to plotly
  })
  
  # APP RESPONSE: Render QC model data frame
  output$results <- DT::renderDataTable({
    
    req(mfi_to_rau_output())
    
    df <- mfi_to_rau_output()[[1]]
    df <- df %>% dplyr::select(SampleID, Plate, ends_with("_Dilution"))
    df <- as.data.frame(lapply(df, function(x) {
      if (is.numeric(x)) round(x, 5) else x
    }))
    
    datatable(
      df,
      options = list(
        paging = FALSE,           # Disable pagination
        scrollX = TRUE,           # Enable horizontal scrolling
        scrollY = "1200px"       # Set vertical scrolling height
      ),
      rownames = FALSE           # Remove row numbers
    )
  })
  
  # APP RESPONSE: Render QC model plot 
  # 1. Track the current plot index
  current_plot <- reactiveVal(1)  # Start with the first plot
  # 2. Get the total number of plots
  total_plots <- reactive({
    length(model_plot())
  })
  # 3. Next button
  observeEvent(input$inc, {
    new_index <- current_plot() + 1
    if (new_index <= total_plots()) {
      current_plot(new_index)
    }
  })
  # 4. Previous button
  observeEvent(input$dec, {
    new_index <- current_plot() - 1
    if (new_index >= 1) {
      current_plot(new_index)
    }
  })
  # 5. Render the selected plot
  output$individual_plot <- renderPlot({
    model_plot()[[current_plot()]]
  })
  
  ## ----- Save Outputs of QC Model -----
  # Trigger hidden download buttons
  observeEvent(input$downloadButtonData, {
    click("downloadData")
  })
  observeEvent(input$downloadButtonStds, {
    click("downloadStds") 
  })
  observeEvent(input$downloadButtonReport, {
    click("report")
  })
  observeEvent(input$downloadButtonZip, {
    click("download_zip")
  })
  
  ## ----- Download Handlers -----
  
  # 1. Downloadable csv of MFI/RAU results file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_MFI_RAU.csv")
    },
    content = function(file) {
      write.csv(mfi_to_rau_output()[[1]], file, row.names = FALSE)
    }
  )
  # 2. Downloadable csv of standards
  output$downloadStds <- downloadHandler(
    filename = function() {
      paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_stdcurve.csv")
    },
    content = function(file) {
      write.csv(antigens_output()$stds, file, row.names = FALSE)
    }
  )
  
  output$report <- downloadHandler(
    filename = paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_QCreport.pdf"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "template.Rmd")
      file.copy("template.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        raw_data_filename_reactive = raw_data_filename_reactive(),
        experiment_name_reactive = experiment_name_reactive(),
        date_reactive = date_reactive(),
        experiment_notes = experiment_notes(),
        platform_reactive = platform_reactive(),
        stdcurve_plot = stdcurve_plot(),
        plateqc_plot = plateqc_plot(),
        blanks_plot = blanks_plot(),
        check_repeats_output = check_repeats_output(),
        check_repeats_table_format = check_repeats_table_format(),
        model_plot = model_plot()
      )
      
      callr::r(
        render_report,
        list(input = tempReport, output = file, params = params)
      )
      
    }
  )
  
  # 4. Download zip file
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0(experiment_name_reactive(), "_", date_reactive(),  "_all_files.zip")
    },
    content = function(file) {
      temp_dir <- file.path(tempdir(), "export_files")
      if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
      dir.create(temp_dir, showWarnings = FALSE)
      
      # Define file paths inside temp_dir
      data_file <- file.path(temp_dir, paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_MFI_RAU.csv"))
      stds_file <- file.path(temp_dir, paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_stdcurve.csv"))
      report_file <- file.path(temp_dir, paste0(experiment_name_reactive(), "_", date_reactive(), "_", version(), "_QCreport.pdf"))
      
      # Generate files
      write.csv(mfi_to_rau_output()[[1]], data_file, row.names = FALSE)
      write.csv(antigens_output()$stds, stds_file, row.names = FALSE)
      
      # Render the report
      tempReport <- file.path(tempdir(), "template.Rmd")
      file.copy("template.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        raw_data_filename_reactive = raw_data_filename_reactive(),
        experiment_name_reactive = experiment_name_reactive(),
        date_reactive = date_reactive(),
        experiment_notes = experiment_notes(),
        platform_reactive = platform_reactive(),
        stdcurve_plot = stdcurve_plot(),
        plateqc_plot = plateqc_plot(),
        blanks_plot = blanks_plot(),
        check_repeats_output = check_repeats_output(),
        check_repeats_table_format = check_repeats_table_format(),
        model_plot = model_plot()
      )
      callr::r(
        render_report,
        list(input = tempReport, output = report_file, params = params)
      )
      
      # Create ZIP with a clean structure
      old_wd <- setwd(temp_dir)  # Switch to temp_dir to avoid extra folders
      zip::zip(file, files = list.files(temp_dir, full.names = FALSE))  # Zip only file names
      setwd(old_wd)  # Restore original working directory
    }
  )
  
  ###############################################################################
  # ------------ CLASSIFY EXPOSURE   ------------
  ###############################################################################
  
  ## ----- Reactive User Inputs -----
  # USER INPUT 1: Reactive expression to get algorithm choice 
  algorithm <- reactive({
    req(input$algorithm)
    input$algorithm
  })
  
  # USER INPUT 2: Reactive expression to get sensitivity/specificity choice 
  sens_spec <- reactive({
    req(input$sens_spec)
    input$sens_spec
  })
  
  ## ----- Print Names for Cross-Checking -----
  output$algorithm_choice <- renderText(paste0("Algorithm choice: ", algorithm()))
  output$algorithm_choice_remind <- renderText(paste0("Based on your algorithm choice (", algorithm(), ") download your data below:"))
  
  ## ----- Run Classification -----
  classified_data <- reactive({
    req(mfi_to_rau_output(), algorithm(), sens_spec())
    
    results_of_classification <- classify_final_results(
      
      # Step 1: specify data to classify
      mfi_to_rau_output = mfi_to_rau_output(),
      
      # Step 2: Select which model you want to run. This needs to be a character string.
      algorithm_type = algorithm(),
      
      # Step 3: Select sensitivity/specificity of interest. 
      Sens_Spec = sens_spec())
    
    results_of_classification
  })
  
  ## ----- Display Results of Classification -----
  # APP RESPONSE: 
  classification_results_summary <- reactive({
    summary_table <- as.data.frame(table(classified_data()$pred_class_max))
    colnames(summary_table) <- c("Status", "Count")
    summary_table
  })
  
  # APP RESPONSE: Render the classification summary table and cross-check choices
  observeEvent(input$run_classification, {
    req(sens_spec(), algorithm(), classification_results_summary())
    
    sens_spec_val <- sens_spec()
    algorithm_val <- algorithm()
    
    result <- paste0("Classification run with the ", algorithm_val,
                     " Algorithm and Sensitivity/Specificity: ", sens_spec_val)
    
    # 1. Render the result text
    output$result <- renderText({result})
    
    # 2. Render the classification summary table
    output$classification_summary <- renderTable({
      classification_results_summary()})
  })
  
  # Trigger hidden download buttons
  observeEvent(input$downloadButtonClassify, {
    click("download_classification")
  })
  
  ## ----- Downloadable csv of classification results file -----
  output$download_classification <- downloadHandler(
    filename = function() {
      paste0(experiment_name(), "_", date(), "_", sens_spec(), "_", algorithm(), "_", version(), "_classification.csv", sep = "")
    },
    content = function(file) {
      write.csv(classified_data(), file, row.names = FALSE)
    })
  
  # ---------- DATA VISUALISATION ------------
  
  
  # Run classification for each specificity/sensitivity
  classified_data_all <- reactive({
    req(mfi_to_rau_output(), algorithm())
    sens_spec_all <- c("maximised", "85% sensitivity", "90% sensitivity", "95% sensitivity", 
                       "85% specificity", "90% specificity", "95% specificity")
    
    all_classifications <- purrr::map_dfr(sens_spec_all, ~{
      classify_final_results(
        mfi_to_rau_output = mfi_to_rau_output(),
        algorithm_type = algorithm(),
        Sens_Spec = .x
      ) %>%
        as.data.frame() %>%  # Ensure it's a data frame
        mutate(Sens_Spec = .x)  # Add the Sens_Spec column
    })
    
    # Return the combined data frame
    all_classifications
  })
  
  # Reactive table creation
  allclassify_df <- reactive({
    req(classified_data_all())
    
    classified_data_all() %>% 
      group_by(Sens_Spec, pred_class_max) %>% 
      summarise(n = n()) %>% 
      pivot_wider(names_from = pred_class_max, values_from = n) %>% 
      dplyr::select(`Sensitivity/Specificity` = Sens_Spec, 
                    Seropositive = seropositive, 
                    Seronegative = seronegative)
    
  })
  
  # Render the table in the UI
  output$allclassifytable <- renderDataTable({
    df <- allclassify_df() %>% 
      mutate(`Sensitivity/Specificity` = factor(`Sensitivity/Specificity`, 
                                                levels = c("maximised",
                                                           "85% specificity", "90% specificity", "95% specificity",
                                                           "85% sensitivity", "90% sensitivity", "95% sensitivity"), 
                                                labels = c("Maximised: 81% Sensitivity / 81% Specificity",
                                                           "85% Sensitivity / 75% Specificity",
                                                           "90% Sensitivity / 61.6% Specificity",
                                                           "95% Sensitivity / 43.4% Specificity",
                                                           "75% Sensitivity / 85% Specificity", 
                                                           "67.5% Sensitivity / 90% Specificity", 
                                                           "52.4% Sensitivity / 95% Specificity"))) %>% 
      arrange(`Sensitivity/Specificity`)
    
    df$Order <- as.numeric(factor(df$`Sensitivity/Specificity`, 
                                  levels = c("Maximised: 81% Sensitivity / 81% Specificity",
                                             "85% Sensitivity / 75% Specificity",
                                             "90% Sensitivity / 61.6% Specificity",
                                             "95% Sensitivity / 43.4% Specificity",
                                             "75% Sensitivity / 85% Specificity",
                                             "67.5% Sensitivity / 90% Specificity",
                                             "52.4% Sensitivity / 95% Specificity")))
    
    df <- df[order(df$Order), ]  # <-- Enforce order
    
    datatable(df %>% dplyr::select(-Order), selection = "single",
              options = list(
                pageLength = 7, 
                dom = 't',
                searching = FALSE,
                lengthChange = FALSE
              ), 
              rownames = FALSE
    )
  })
  
  observeEvent(input$allclassifytable_rows_selected, {
    print(input$allclassifytable_rows_selected)
  })
  
  output$classify_plots <- renderPlotly({
    
    selected_row <- input$allclassifytable_rows_selected
    req(selected_row)
    
    selected_value <- allclassify_df()[selected_row, "Sensitivity/Specificity", drop = TRUE]
    # print(paste("Generating plot for:", selected_value))
    
    gg <- plotBoxPlotClassification(classified_data_all(), selected_value)
    ggplotly(gg) %>% 
      layout(
        showlegend = TRUE, 
        font = list(family = "Helvetica", size = 20, colour = "black")
      )
    
  })
  
  output$mfi_plotly <- renderPlotly({
    req(mfi_to_rau_output())
    
    mfi_plot <- plotMFI(mfi_to_rau_output())
    
    plotly_mfi_plot <- ggplotly(mfi_plot) %>%
      layout(
        showlegend = TRUE, 
        legend = list(tracegroupgap = 0), 
        font = list(family = "Helvetica", size = 20, colour = "black"),
        xaxis = list(title_standoff = 40)
      )
    plotly_mfi_plot
  })
  
  output$rau_plotly <- renderPlotly({
    req(mfi_to_rau_output())
    
    rau_plot <- plotRAU(mfi_to_rau_output())
    
    plotly_rau_plot <- ggplotly(rau_plot) %>%
      layout(
        showlegend = TRUE, 
        legend = list(tracegroupgap = 0), 
        font = list(family = "Helvetica", size = 20, colour = "black"),
        xaxis = list(title_standoff = 40)
      )
    plotly_rau_plot
  })
  
  
  output$bead_count_plotly <- renderPlotly({
    req(antigens_output(), plate_layout_reactive())
    
    plotly_bead_count <- plotBeadCounts(antigen_output = antigens_output(),
                                        plate_layout = plate_layout_reactive()$datapath)
    plotly_bead_count_1 <- ggplotly(plotly_bead_count) %>%
      layout(
        showlegend = TRUE, 
        legend = list(tracegroupgap = 0), 
        font = list(family = "Helvetica", size = 20, colour = "black")
      )
    plotly_bead_count_1
    
  })
  
  
})
