setup <- function(){
  needed <- c("shiny", "shiny.fluent", "shiny.react", "shinyjs", "htmltools", 
              "workflowsets", "plotly", "tidyverse", "ggpubr", "janitor", "DT",
              "rmarkdown", "shinyWidgets", "here", "RColorBrewer", "readxl",
              "openxlsx", "glue", "drc", "gt", "callr", "httr", "jsonlite")
  for(package in needed){
    if(!sum(installed.packages() %in% package)){
      install.packages(package)
    }
    require(package, character.only = TRUE)
  }
}

setup()