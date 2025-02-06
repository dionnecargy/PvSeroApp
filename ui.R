###############################################################################
###### Load Packages and Functions
###############################################################################

source("packages.R")
source("functions.R")
source("content.R")

antibody_model <- readRDS(here::here("model/PvSeroTaTmodel.rds"))
antibody_model_excLF016 <- readRDS(here::here("model/random_forest_excludingLF016.rds"))
platemap <- read.csv(here::here("data/platemap.csv"))

###############################################################################
###### UI
###############################################################################

shinyUI(
  fluentPage(
    useShinyjs(),
    tags$head(
      if (file.exists("google_analytics.html")) {
        includeHTML("google_analytics.html")
      }
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "https://unpkg.com/react@17.0.2/umd/react.production.min.js"),
      tags$script(src = "https://unpkg.com/react-dom@17.0.2/umd/react-dom.production.min.js"),
      tags$script(src = "https://unpkg.com/@fluentui/react-charting"),
      tags$script(HTML("
      $(document).ready(function() {
        $('body').addClass('light-mode');
    
        function applyTheme(isDark) {
          console.log('Applying theme:', isDark); // Debugging
    
          if (isDark) {
            $('body').removeClass('light-mode').addClass('dark-mode');
            window.FluentUITheme = {
              palette: {
                themePrimary: '#9aafff',
                themeLighterAlt: '#06070a',
                themeLighter: '#181c29',
                themeLight: '#2e344d',
                themeTertiary: '#5c6899',
                themeSecondary: '#8799e0',
                themeDarkAlt: '#a3b6ff',
                themeDark: '#b1c1ff',
                themeDarker: '#c6d1ff',
                neutralLighterAlt: '#252423',
                neutralLighter: '#252423',
                neutralLight: '#232221',
                neutralQuaternaryAlt: '#21201f',
                neutralQuaternary: '#1f1e1e',
                neutralTertiaryAlt: '#1e1d1c',
                neutralTertiary: '#a2a1a1',
                neutralSecondary: '#888786',
                neutralPrimaryAlt: '#6d6c6c',
                neutralPrimary: '#f3f2f1',
                neutralDark: '#383737',
                black: '#1d1d1d',
                white: '#252423',
              }
            };
          } else {
            $('body').removeClass('dark-mode').addClass('light-mode');
            window.FluentUITheme = {
              palette: {
                themePrimary: '#0078d4',
                themeLighterAlt: '#eff6fc',
                themeLighter: '#deecf9',
                themeLight: '#c7e0f4',
                themeTertiary: '#71afe5',
                themeSecondary: '#2b88d8',
                themeDarkAlt: '#106ebe',
                themeDark: '#005a9e',
                themeDarker: '#004578',
                neutralLighterAlt: '#faf9f8',
                neutralLighter: '#f3f2f1',
                neutralLight: '#edebe9',
                neutralQuaternaryAlt: '#e1dfdd',
                neutralQuaternary: '#d0d0d0',
                neutralTertiaryAlt: '#c8c6c4',
                neutralTertiary: '#a19f9d',
                neutralSecondary: '#605e5c',
                neutralPrimaryAlt: '#3b3a39',
                neutralPrimary: '#323130', /* Text color */
                neutralDark: '#201f1e',
                black: '#000000',
                white: '#ffffff',
              }
            };
          }
    
          if (typeof FluentUI !== 'undefined' && typeof FluentUI.applyTheme === 'function') {
            FluentUI.applyTheme(window.FluentUITheme);
            console.log('Theme applied successfully');
          } else {
            console.error('FluentUI.applyTheme is undefined!');
          }
        }
    
        Shiny.addCustomMessageHandler('toggle-theme', function(isDark) {
          applyTheme(isDark);
        });
      });
    "))
    ),
    tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")),
    tags$head(tags$link(rel = "shortcut icon", href = "PvSeroApp.ico")),
    div(
      style = "display: flex; flex-direction: column; height: 100vh;",
      
      # ----------------- HEADER -----------------
      div(
        class = "header",
        style = "padding: 10px; display: flex; justify-content: space-between; align-items: center;",
        
        # Left Section (Icon + Text)
        div(
          style = "display: flex; align-items: center;",
          img(src = "logo.png", style = "height: 35px"),
          Separator(vertical = TRUE),
          Text(variant = "xLarge", "PvSeroApp: Sero-surveillance Tool", style = list(root = list(color = "var(--fluent-primary-text-color)"),
                                                                                      marginLeft = "15px"))
        ),
        
        # Right Section (Dark Mode Toggle + GitHub Icon)
        div(
          style = "display: flex; align-items: center;",
          # IconButton.shinyInput(inputId = "toggle_theme", iconProps = list(iconName = "Contrast")),
          tags$a(
            href = "https://github.com/dionnecargy", 
            target = "_blank",
            tags$i(class = "fab fa-github", style = "font-size: 24px; margin-left: 20px;")
          )
        )
      ),
      
      # ----------------- STACK LAYOUT FOR SIDEBAR AND MAIN CONTENT -----------------
      Stack(
        horizontal = TRUE, tokens = list(childrenGap = 20), horizontalAlign = "start",
            # Left navigation panel with Nav component
            div(
              class = "nav-panel",
              div(img(src = "PvSeroApp.png", style = "align-items: center; height: 200px; margin-left: 25px")),
                Nav(
                  ariaLabel = "Introduction",
                  groups = list(
                    list(
                      name = "Introduction", 
                      links = list(
                        list(name = "About", key = "home", url = "#home", iconProps = list(iconName = "Home", styles = list(root = list(fontSize = 20, color = "#106ebe")))),
                        list(name = "Tutorial", key = "tutorial", url = "#tutorial", iconProps = list(iconName = "Info", styles = list(root = list(fontSize = 20, color = "#106ebe")))), 
                        list(name = "Algorithm", key = "algorithm", url = "#algorithm", iconProps = list(iconName = "ConnectVirtualMachine", styles = list(root = list(fontSize = 20, color = "#106ebe"))))
                      )
                    ),
                    list(
                      name = "PvSeroApp Algorithm",
                      links = list(
                        list(name = "Step 1: Input Data", key = "input", url = "#input", iconProps = list(iconName = "BulkUpload", styles = list(root = list(fontSize = 20, color = "#106ebe")))),
                        list(name = "Step 2: Quality Control", key = "check", url = "#check", iconProps = list(iconName = "CheckList", styles = list(root = list(fontSize = 20, color = "#106ebe")))),
                        list(name = "Step 3: Classify Exposure", key = "model", url = "#model", iconProps = list(iconName = "Diagnostic", styles = list(root = list(fontSize = 20, color = "#106ebe")))),
                        list(name = "Step 4: Data Visualisation", key = "datavis", url = "#datavis", iconProps = list(iconName = "BIDashboard", styles = list(root = list(fontSize = 20, color = "#106ebe"))))
                      )
                    )
                  )
                )
            ),
            # Main content area where dynamic content will be rendered
            div(
              class = "main-content",
              style = "flex-grow: 1; padding: 20px;",
              uiOutput("page_content")  # Dynamic content area for rendering the page content
            )
      ),
      # ----------------- FOOTER -----------------
      div(
        class = "footer",
        style = "padding: 10px; text-align: center;",
        Stack(
          tokens = list(childrenGap = 5),
          textOutput("footer_version"),
          Text(variant = "small", HTML(r"(Developed by <a href='https://www.example.com' target='_blank'>Dionne Argyropoulos and Lauren Smith</a> and built using 
          <a href='https://shiny.posit.co/' target='_blank'>RShiny</a> by <a href='https://posit.co/download/rstudio-desktop/' target='_blank'>RStudio</a>. 
          <p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/dionnecargy/pvserotat">PvSeroTaT Classification App</a> 
          is licensed under <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC-SA 4.0
          <img style="height:12px;width:auto;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt="">
          <img style="height:12px;width:auto;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt="">
          <img style="height:12px;width:auto;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt="">
          <img style="height:12px;width:auto;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt=""></a></p>)"), styles = list(root = list(color = "var(--fluent-primary-text-color)")))
        )
      )
    )
  )
)
