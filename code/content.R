######### FUNCTIONS ###########################################################
# The first part of this script stores functions used in the UI
# Author: Dionne C. Argyropoulos 
###############################################################################

######### CONTENT #############################################################
# The second part of this script stores functions used as the pages in the web
# Author: Dionne C. Argyropoulos 
###############################################################################

###############################################################################
# --------------------------- Home Page --------------------------- 
###############################################################################
home_page <- function() {
  Stack(
    tokens = list(childrenGap = 10),
    Text(variant = "xxLarge",
         HTML("PvSeroApp: A fit-for-purpose tool to support <em>Plasmodium vivax</em> sero-surveillance via serological data processing and statistical analysis."),
         Separator()
    ),
    Text(variant = "medium",
         HTML("This analytical tool was developed to streamline the processing of serological data generated using a validated high-throughput multiplex serological 
              assay for measuring antibodies to <em>Plasmodium vivax</em> (<a href='https://doi.org/10.1038/s41591-020-0841-4' target='_blank'>Longley et al 2020</a>). 
              This tool also aims to make the downstream processing, quality control, and interpretation of the raw data generated from this serological assay 
              accessible to all researchers without the need for a specialist background in statistical methods and advanced programming.")
    ),
    Text(variant = "large", "Graphical overview of the web application:"),    # Link "here" to the #tutorial page using anchor
    Text(variant = "medium",
         HTML("For details on how to use this app, follow the step-by-step tutorial <a href='#tutorial' class='link'>here</a>.")
    ),
    Image(src = "1_main/pvseroapp_overview.png", width = "1000px", height = "auto"),
    Text(variant = "large", "Acknowledgements"),
    Text(variant = "medium", HTML("The code and scripts used to develop this R Shiny web application are available on <a href='https://github.com/dionnecargy/pvserotat' class-'link'>GitHub</a>. 
    The scripts and functions used in this application were developed by Lauren Smith and Dionne Argyropoulos, with contributions from the following researchers:
      <ul>
        <li>Lauren Smith, WEHI (classification algorithm development)</li>
        <li>Connie Li Wai Suen, WEHI (original 5-parameter logistic model development for conversion of MFI to RAU for PNG Pool Standard Curve)</li>
        <li>Eamon Conway for ongoing advice, WEHI (5-parameter logistic model development for conversion of MFI to RAU for Ethiopian Pool Standard Curve)</li>
        <li>Thomas Obadia, Institut Pasteur (development of serological classification algorithm, application scripts and functions for <em>P. vivax</em>)</li>
        <li>Shazia Ruybal-Pesántez, WEHI* (application scripts and functions for SARS-Cov-2)</li>
        <li>Narimane Nekkab** and Michael White, Institut Pasteur (original development of serological classification algorithm for <em>P. vivax</em>)</li>
      </ul>Current address: *Imperial College London; **Swiss Tropical and Public Health Institute.")),
    Text(variant = "medium", "Many thanks to Pailene Lim, Anju Abraham, Macie Lamont, Caitlin Bourke and Nick Walker for trailing and providing feedback on this tool."),
    Text(variant = "medium", "This work was conducted under supervision of Rhea Longley and Ivo Mueller."), 
    Text(variant = "large", "Funding"),
    Text(variant = "medium", "We acknowledge grant funding from the Bill and Melinda Gates Foundation (INV-051542), the WEHI Global Impact Fund, and a Ramaciotti Health Investment Grant (2022HIG52), to support development of this RShiny App.")
  )
}

###############################################################################
# --------------------------- Tutorial Page --------------------------- 
###############################################################################

tutorial_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "Tutorial: How to Use The PvSeroApp"),
    Separator(),
    Stack(
      horizontal = TRUE, tokens = list(childrenGap = 20), horizontalAlign = "start",
      div(
        class = "main-content",
        style = "flex-grow: 1; padding: 20px;", # Main content takes 80% width
        
        ######### ------------ Introduction ------------
        div(
          id = "tutorial/intro", 
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "1. Introduction"), alignContent = "start"),
            Text(variant = "medium", HTML("This web application was designed fit-for-purpose to process raw serology data exported from 
            <a href='https://int.diasorin.com/en/luminex-ltg/tools/magpix-system' class-'link'>MAGPIX</a> and 
            <a href='https://www.bio-rad.com/en-au/category/bio-plex-reader-tools?ID=317582a5-ac11-4e2c-8ee6-4cf0799352fd' class-'link'>Bioplex</a> machines.
            For an example of the raw data output from each machine, <a href='https://github.com/dionnecargy/pvserotat' class-'link'>download the example data</a>. 
            The built-in classification algorithms are applicable to perform quality control, analyse the data from the <em>Plasmodium vivax</em> PvSeroTaT Luminex 
            Assay as per <a href='https://doi.org/10.1038/s41591-020-0841-4' target='_blank'>Longley et al. (2020)</a>, and perform preliminary data visualisations."))
          )
        ),
        p(),
        ######### ------------ Raw Data Requirements  ------------
        div(
          id = "tutorial/rawdata",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "2. Raw Data Requirements"), alignContent = "start"),
            Text(variant = "medium", HTML(r"(The following files are required for this app. You might like to organise your folder as follows:)")),
            verbatimTextOutput("code_display"),
            makeCard(
              id = "tutorial/rawdata/antigens",
              title = "2.1. Antigen Label Requirements", 
              content = list(
                MessageBar(messageBarType = 3, "Make sure you label your antigens the same way in all plate runs!"),
                renderTwoCols(
                  first_col = Text(variant = "medium", HTML(r"(In the MAGPIX or Bioplex machines you can add the name for each antigen in the <b>PvSeroTaT Luminex Assay</b>.
                         For the purposes of data processing, it is important that the antigens are named with the convention listed here.<br><br>
                         The data processing in this app will be able to decipher any of these options, differing capitalisations, and any "Pv" prefixes.<br><br>
                         If you have run other antigens in your assay that are not included in the current PvSeroApp model, then the data processing with automatically remove these columns when you reach 
                         the <a href='#tutorial/classify' class-`link`>Classification</a> step.)")), 
                  second_col = fluentPage(DTOutput("antigens"))
                )
              )
            ), 
            makeCard(
              id = "tutorial/rawdata/plate", 
              title = "2.2. Plate Layout Requirements",
              content = list(
                Text(variant = "medium", 
                     HTML(r"(For each 96-well plate that you run on the Luminex machine, prepare a plate layout that includes the sample labels that will match your raw data.
                   The application will match the raw data to the corresponding sample based on the plate layout that you import.
                   <br><br> Make sure that your sample labels in the plate layout are as follows: <ul>
                        <li><b>Standards</b>: Labels start with <b>"S"</b> and then a number as required (e.g. S1, S2, S3 or Standard1, Standard2, Standard3).</li>
                        <li><b>Blanks</b>: Labels start with <b>"B"</b> and then a number as required if there is more than one blank sample (.e.g 'B1', 'B2', or 'Blank 1', 'Blank2' etc).</li>
                        <li><b>Unknown Samples</b>: Label your unknown samples according to your specific sample codes (e.g. ABC001, ABC002).</li>
                    </ul>The application expects standards to start with "S" and blanks to start with "B", but everything else with a label will be considered an unknown sample.
                    If you have other types of samples, for example a positive control, you can use a different sample label to the other unknown study samples
                    (i.e. "PositiveControl" in addition to the "ABC" study codes).)")),
                fluentPage(Text(variant = "medium", "If using the 10-point standard curve, the standards S1-10 correspond to the following dilution concentrations:"), DTOutput("standards")),
                fluentPage(Text(variant = "medium", "If using the 5-point standard curve, the standards S1-5 correspond to the following dilution concentrations:"), DTOutput("fivept_stds")),
                MessageBar(messageBarType = 3, "Make sure that there are no extra cells filled out other than the plate wells and sample names. You can use the example plate layout as a template to fill 
                    in with your sample details."),
                Text(variant = "medium", "If you downloaded the example plate layout, it should look like this, otherwise make sure your plate layout looks similar:"),
                div(
                  class = "plot-container",
                  actionButton("dec_plate_image", "←", class = "btn-primary-plate"),
                  div(
                    class = "plate-window",
                    imageOutput("individual_plate_image")
                  ),
                  actionButton("inc_plate_image", "→", class = "btn-primary-plate")
                ),
                Text(variant = "medium", HTML("If you have multiple plates that you are running through this application, all of the plate layouts can be added in one <b>excel (.xlsx)</b> file.")),
                MessageBar(HTML(r"(Please label each <b>tab or sheet</b> in the excel (.xlsx) file as "plate1", "plate2", "plate3" etc. corresponding to your <b>raw data</b> input file names 
                            discussed in the section below.)")),
                Text(variant = "medium", "Due to popular demand, you can now add multiple plate layouts to the PvSeroApp. However for security reasons the app does not allow you to add multiple files 
                    from different folders. For ease, please create a new folder with all of the files you would like to upload to the PvSeroApp so that you can add the files at once.")
              )
            ), 
            makeCard(
              id = "tutorial/rawdata/platform", 
              title = "2.3. Raw Data",
              content = list(
                Pivot(
                  PivotItem(
                    headerText = "2.3.1. Raw Data from MAGPIX Machines",
                    Text(variant = "medium", "You can pre-program the MAGPIX machine so that you can export all the raw data directly from the machine once the plate reading is completed. 
                         There is no need to edit the raw data file that comes from the MAGPIX."),
                    p(),
                    Text(variant = "medium", 
                         HTML(r"(Within your plate layout in the MAGPIX, you can use the "U" button for all unknown samples, "B" button for Background or Blank samples, and "S" for Standard Curve samples. 
                              For the control wells, please feel free to edit these labels so that the ID is just "B", "S1", "S2", "S3"...."S10".)")),
                    p(),
                    Text(variant = "medium", "On the MAGPIX machine, you can also write in the Operator i.e., who ran the assay! This is useful to track variation in plates between experiments."),
                    p(),
                    MessageBar("Ensure that the antigens are labelled the same way in all plate runs! You can do this by setting up the protocol directly on the MAGPIX and using it for all your plate runs."),
                    p(),
                    MessageBar(messageBarType = 3, HTML(r"(Please add the plate number or experiment number as a suffix to the end of your file, for example: "<b>MyExperimentName_plate1.csv</b>", and ensure
                                                      that <b>"plate1"</b> corresponds to the <b>"plate1"</b> in the plate layout excel file. It is important that there are <b>no other numbers in the file name</b> 
                                                      so that we can link the plate number and file number to each other.)")),
                    p(),
                    Image(src = "2_tutorial/2.3.1_magpix_raw.png", width = 800, height = 800, imageFit = "container")
                  ),
                  PivotItem(
                    headerText = "2.3.2. Bio-Plex",
                    Text(variant = "large", "Raw Data from Bio-Plex Machines"),
                    p(),
                    Text(variant = "medium", "Isolate names will tend to be written as 'X1', 'X2', 'X3'... and saved as an .xlsx file. Specifics on Bio-Plex machines will be added upon request"),
                    p(),
                    Image(src = "2_tutorial/2.3.2_bioplex_raw.png", width = 800, height = 200, imageFit = "container")
                  )
                )
              )
            )
          )
        ),
        p(),
        ######### ------------ Navigating the App ------------
        div(
          id = "tutorial/app_nav",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "3. Navigating the App"), alignContent = "start"),
            makeCard(
              id = "tutorial/app_nav1", 
              title = NA, 
              content = list(
                # Insert Image and/or Video
                Text(variant = "large", "3.1. Importing your Data"),
                Text(variant = "medium", HTML("This is where you can: 
                                          <ul>
                                            <li><b>label</b> your experiment/project so that all your processed data files, quality control reports can have the project name as an identifier, </li>
                                            <li><b>upload</b> your raw data and plate layout, and</li>
                                            <li><make sure that your <b>raw data</b>, <b>run information</b> and <b>plate layout</b> look as expected</li>
                                          </ul><p>See the section <a href='#tutorial/import_data' class-`link`>Importing your data</a>.</p>")),
                Text(variant = "large", "3.2. Quality Control"),
                Text(variant = "medium", HTML("This is where you can: 
                                          <ul>
                                            <li>perform <b>quality control</b> of your run,</li>
                                            <li>data is processed to convert <b>Median Fluorescent Intensity (MFI)</b> to <b>Relative Antibody Units (RAU)</b> based on the standard curves present on each plate</li>
                                            <li><b>download</b> the processed data,s tandard curve data and a quality control report</li>
                                          </ul><p>For more details, see the section <a href='#tutorial/quality_control' class-`link`>Quality Control</a>.")),
                Text(variant = "large", "3.3. Classifying your Samples"),
                Text(variant = "medium", HTML("This is where you can run the classification algorithm on your data to determine whether unknown samples are seropositive or seronegative, (i.e. exposure to <em>Plasmodium vivax</em> in the last 9 months, or not).")),
                Text(variant = "medium", HTML(r"(We have developed two algorithms that were trained on the same dataset but differ on antigen choice:
                                          <ul>
                                            <li>If you are processing data for all eight antigens in the <b>PvSeroTaT</b> sero-surveillance tool, please use "PvSeroTaT Algorithm",</li>
                                            <li>If you are processing data <b>without LF016</b>, please use "PvSeroTaT Algorithm without LF016".</li>
                                          </ul>)")),
                Text(variant = "medium", HTML(r"(Note that this app <b>cannot</b> be used for data processing of samples with a different antigen panel to the one from <b>PvSeroTaT</b> sero-surveillance tool. 
                                          For more details, see the section <a href='#tutorial/classify' class-`link`>Classifying your Samples</a>.)")),
                Text(variant = "large", "3.4. Data Visualisation"),
                Text(variant = "medium", HTML("This is where you can visualise your data interactively within the app. You can take a look at box plots of the antibody data for each antigen in your panel based
                                          on the converted Relative Antibody Units (RAU) and visualise the exposure status of your samples based on the <b>classification algorithm</b> and <b>random forest votes threshold</b>. 
                                          For more details, see the section <a href='#tutorial/data_vis' class-`link`>Data Visualisation</a>."))
              )
            )
          )
        ),
        p(),
        ######### ------------ Importing your data ------------
        div(
          id = "tutorial/import_data",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "4. Importing your Data"), alignContent = "start"),
            makeCard(
              id = "tutorial/import_data/main", 
              title = NA, 
              content = list(
                Text(variant = "medium", HTML(r"(You can import your raw data (.csv or .xlsx) and plate layout (.xlsx) files by clicking on the "Upload" buttons and navigating to the path where your files are located (see below).
                       You can change the experiment name by typing directly in the text box with the experiment name that best describes your particular data e.g., you can label it as
                       "<b>MyExperimentName_plate1</b>". 
                       <br><br>The analysis date will default to Today's date, but you can modify this by clicking on the date and selecting the date in the calendar. You can also insert
                       experiment notes to provide more details or notes, as necessary. These notes will be displayed on your quality control report when you download it.)")),
                MessageBar(HTML(r"(Try to keep your experiment name/filename free of spaces and instead use '_' or '-'. All of your downloadable files (processed data, quality control report, etc) will be labeled
                       as "<b>ExperimentName_Date_VersionNumber</b>")")),
                Text(variant = "medium", HTML("A step-by-step guide is available in the <b>Input Data</b> Page which will walk you through how to input your files successfully.")),
                Image(src = "2_tutorial/4.1_input_data_1.png", width = "auto", height = "auto") 
              )
            ),
            makeCard(
              id = "tutorial/import_data/import", 
              title = "4.1. Import Your Experiment Information and Files", 
              content = list(
                Text(variant = "medium", "Click on the tabs below to follow a more in-depth description of each step."),
                Pivot(
                  PivotItem(
                    headerText = "Step 1: Input Data Interface",
                    MessageBar(HTML(r"(The app is designed so that each "Step" is numbered in order so you can easily follow along. The necessary steps are asterixed in red.)")),
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", "This is how the input data interface should look like when you open the app."), 
                        p(),
                        Text(variant = "medium", HTML(r"(1. First enter your experiment name. If you do not have one unique to your project, the automated text "experiment1" will also work.)")),
                        p(),
                        Text(variant = "medium", "2. Next, enter the date. This will automatically change to today's date, but you can change to be the date of your experiment runs if you desire."),
                        p(),
                        Text(variant = "medium", "3. This next step is not necessary (and is not asterixed), where you can add any experiment notes that will be rendered in your QC report."),
                        p(),
                        Text(variant = "medium", "4. Then you will select the platform which your luminex assay was run: Currently there are two options: MAGPIX or BioPlex Machine.
                         This will effect how the data is processed as the raw data outputs differ significantly."), 
                        p(), 
                        Text(variant = "medium", "5. OPTIONAL: You can select for only a standard curve to be printed. This is if you are only performing quality control runs and do not have any isolate samples."), 
                        p(), 
                        MessageBar(messageBarType = 3, HTML(r"(If you cannot see your data correctly in the "Check Raw Data" or "Check Run Info" tabs, it is likely you have not chosen the correct platform option for your data!)"))
                      ), 
                      second_col = Image(
                        src = "2_tutorial/4.1_input_data_2.png", 
                        width = "550px",   # Set a specific width (e.g., 500px)
                        height = "auto"    # Automatically adjust the height to maintain aspect ratio
                      )
                    )
                  ),
                  PivotItem(
                    headerText = "Step 2: Uploading Raw and Plate Layouts",
                    MessageBar(messageBarType = 3, HTML("All data uploaded must be de-identified and not re-identifiable (see<a href='https://posit.co/about/posit-service-terms-of-use/' class='_blank'>Posit Service Terms of Use</a>)")), 
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML("One or multiple <b>Raw Data</b> files can be uploaded each time. Please ensure that the file types are all the same (i.e., all .csv or .xlsx) for consistency.")), 
                        p(),
                        Text(variant = "medium", HTML("There can only be one <b>Plate Layout</b> file which must be an .xlsx excel file. See the section on <a href='#tutorial/rawdata' class-`link`>Raw Data Requirements</a> 
                                                  for how to construct your plate layout file.")), 
                        p(),
                        Text(variant = "medium", HTML(r"(When your raw data and plate layouts are uploaded correctly, your screen should look like this, with the green success message, "Files Imported!")"))
                      ), 
                      second_col = Image(src = "2_tutorial/4.1_input_data_3.png", width = "550px", height = "auto")
                    )
                  ),
                  PivotItem(
                    headerText = "Step 3: Save Inputs",
                    MessageBar("If you get an error when uploading your files, make sure that you have correctly highlighted which machine you used for your raw data (MAGPIX or BioPlex) and plate layout file is in .xlsx format."),   
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML(r"(When you are ready to begin data processing, press "<b>Save Inputs</b>".)")),
                        p(),
                        Text(variant = "medium", HTML(r"(If you would like to change any of the data import entries or upload new raw data files or plate layouts, you will need to click the "<b>Save Inputs</b>" again.)"))
                      ), 
                      second_col = Image(src = "2_tutorial/4.1_input_data_4.png", width = "550px", height = "auto") 
                    )
                  ),
                  PivotItem(
                    headerText = "Step 4: Example",
                    MessageBar("To corroborate your understanding, please follow this example!"),
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML(r"(1. Let's change the experiment name to "tutorial_example", enter today's date, the experiment notes to "This is an example of the QC and processing of the example data for the PvSeroTaT tutorial".)")),
                        p(),
                        Text(variant = "medium", HTML(r"(2. We are using a <b>Magpix</b> raw data file so we click the "Magpix" button.)")),
                        p(),
                        Text(variant = "medium", HTML(r"(3. We can then upload our <a href='https://github.com/dionnecargy/PvSeroApp/tree/main/data' class='_blank'>raw data files</a> and 
                                                      <a href='https://github.com/dionnecargy/PvSeroApp/blob/main/data/example_platelayout_1.xlsx' class='_blank'>plate layout</a>.)")),
                        p(),
                        Text(variant = "medium", "Your screen should now look like the image on the right hand side here."),
                        p(),
                        Text(variant = "medium", "And you will also receive a message at the bottom left hand corner of your screen indicating whether the raw data and plate layouts are read correctly."),
                        p(),
                        renderTwoCols(
                          first_col = Image(src = "2_tutorial/4.1_input_data_6.png", width = "200px", height = "auto"), 
                          second_col = Image(src = "2_tutorial/4.1_input_data_7.png", width = "200px", height = "auto")
                        ),
                        p(),
                        Text(variant = "medium", "Congratulations 🎉")
                      ), 
                      second_col = Image(src = "2_tutorial/4.1_input_data_5.png", width = "550px", height = "auto")
                    )
                  )
                )
              )
            ), 
            makeCard(
              id = "tutorial/import_data/check", 
              title = "4.2. Check Your Data Is Loaded Correctly",
              content = list(
                Text(variant = "medium", "Click each tab below to see how we verify that the data you entered has been successfully added to the app!"),
                Pivot(
                  PivotItem(
                    headerText = "Step 1: Checking the Raw Data", 
                    MessageBar("We first want to cross-check that our raw data file/s were correctly uploaded."),
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML(r"(You will notice that the "Check the raw data" tab has populated with a preview of your raw data file (see below). You can quickly check here whether it looks as expected.)")),
                        p(),
                        Text(variant = "medium", HTML(r"(Here, you can cross-check that the raw data file/s that you uploaded were correct by looking at the "Raw data filename/s" (highlighted here in a red box and arrow).)")),
                        p(),
                        MessageBar(messageBarType = 3, HTML(r"(If they are not the right file/s, you can click on the "Import Your Data" tab again, upload the correct file, and save your new inputs!)"))
                      ), 
                      second_col = Image(src = "2_tutorial/4.2_check_data_1.png", width = "550px", height = "auto") 
                    )
                  ),
                  PivotItem(
                    headerText = "Step 2: Checking the Run Information", 
                    MessageBar(),
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML(r"(The "Check Run Info" tab can also be used to only look at run specifications and check that they are expected.)")), 
                        p(),
                        Text(variant = "medium", HTML(r"(You can check that the <span style="color:#C50F1F; font-weight: bold">Batch</span>, <span style="color:#4C3867; font-weight: bold">ProtocolName</span> and
                                                  <span style="color:#2C72A8; font-weight: bold">Sample Volume</span> are correct and labeled as you expect them to be.)")),
                        p(),
                        Text(variant = "medium", HTML(r"(In the case of the example data, the <span style="color:#C50F1F; font-weight: bold">Batch</span> should be labeled
                                                  <span style="color:#C50F1F; font-weight: bold">"Example Plate"</span>, the <span style="color:#4C3867; font-weight: bold">Protocol</span> should be labeled 
                                                  <span style="color:#4C3867; font-weight: bold">"PvSeroTaT_v1.0"</span>, and the <span style="color:#2C72A8; font-weight: bold">Sample Volume</span> should be 
                                                  <span style="color:#2C72A8; font-weight: bold">"50uL"</span> as we can see in the right hand side image here. )"))
                      ), 
                      second_col = Image(src = "2_tutorial/4.2_check_data_2.png", width = "550px", height = "auto") 
                    )
                  ),
                  PivotItem(
                    headerText = "Step 3: Checking the Plate Layout", 
                    MessageBar(HTML(r"(You can use the example "Plate Layout Template" provided in our example data. Ensure the word "Plate" is included in the first column so that the app can determine the position of 
                                each sample and match it to the raw data outputs.)")),
                    p(),
                    renderTwoCols(
                      first_width = "40%", second_width = "60%",
                      first_col = list(
                        Text(variant = "medium", HTML(r"(You can click on the "Check Plate Layout" tab to make sure the plate layout looks as expected. It is very important to make sure your samples are labeled
                    correctly so that they are interpreted correctly by the app. You can also cross-check that the plate layout file you uploaded was the correct one, by looking at the
                    "Plate Layout Filename" directly under the tab.)")),
                    p(), 
                    MessageBar(messageBarType = 3, HTML(r"("If the wrong file was uploaded, you can click on the "Upload" button under "Upload Plate Layout" again and upload the correct file. Remember to click "Save Inputs" again!)")),
                    p(),
                    Text(variant = "medium", HTML(r"(Make sure that your sample labels in the plate layout are as described in the <a href='#tutorial/rawdata' class-`link`>Raw Data Requirements</a> section.)")),
                    p(),
                    Text("Also make sure that there are no extra cells filled out other than the plate wells and sample names."), 
                    p(), 
                    Text(variant = "medium", "If you haven't run a full plate, you can leave the wells with no samples completely blank (i.e. no labels)."), 
                    p(), 
                    Text(variant = "medium", "In the case of the example data, the plate layout should appear as on the right hand side here.")
                      ), 
                    second_col = list(
                      Image(src = "2_tutorial/4.2_check_data_3.png", width = "550px", height = "auto"), 
                      MessageBar(HTML(r"(You can look at each tab by pressing the "Next →" or "Previous ←" buttons provided. You are allowed to upload an .xlsx file with extra tabs (i.e., there is no "matching" raw data file). 
                                    Tabs without a corresponding raw data file will not be processed in the future steps.)"))
                    )
                    )
                  )
                )
              )
            )
          )
        ),
        p(),
        ######### ------------ Quality control ------------
        div(
          id = "tutorial/quality_control",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "5. Quality Control"), alignContent = "start"),
            Text(variant = "medium", HTML(r"(This section allows you to check the quality of your Luminex run, you can navigate each tab individually and also click the "<b>Download</b>" tab to download your processed sample data, 
                                          the standard curve values and the quality control report. All of the plots in the "Quality Control" section will be included in the QC report (.pdf) when downloaded.)")),
            makeCard(
              id = "tutorial/quality_control/location", 
              title = "5.1. Initialising the Quality Control Report", 
              content = list(
                Text(variant = "medium", "As there are epidemiological differences in antibody levels for various antigens, we have based our calculations (so far) on either a PNG or Ethiopian pool of standards. The first step of quality control analysis is 
                     to tell the app which control you are using, so that the appropriate standard curve calculations are performed."),
                p(),
                Image(src = "2_tutorial/5.1_qc_tute_1.png", width = "auto", height = "auto")
              )
            ), 
            makeCard(
              id = "tutorial/quality_control/check", 
              title = "5.2. Checking the Quality Control Reports", 
              content = list(
                MessageBar(HTML("For each of these plots, there are buttons on the top right of the plot that can be used to download the plot as a png (<i class='fas fa-camera'></i>), as well as other capabilties: <i class='fas fa-search'></i> zoom,  
              <i class='fas fa-arrows-alt'></i> move, <i class='fas fa-plus-square'></i> zoom in, <i class='fas fa-minus-square'></i> zoom out, <i class='fas fa-expand-arrows-alt'></i> autoscale, <i class='fas fa-home'></i> reset axes).")),
              Pivot(
                PivotItem(
                  headerText = "Step 1: Standard Curves",
                  p(),
                  renderTwoCols(
                    first_width = "30%", second_width = "70%",
                    first_col = list(
                      Text(variant = "medium", "The standard curve plots are generated from the antibody data from the standards you indicated in your plate layout (e.g. S1-S10) and Median Fluorescent Intensity (MFI) units are displayed in log10-scale."),
                      p(),
                      Text(variant = "medium", "In the case of the PvSeroTaT multi-antigen panel, the antigens will be displayed and in general your standard curves should look relatively linear (only when the y-axis is on logarithmic scale)"),
                      p(),
                      Text(variant = "medium", "The figure is interactive! You can hover over each dot point to see the Standard Curve Refernce ID (Sample), MFI and Plate [image below]."),
                      p(),
                      Image(src = "2_tutorial/5.2_qc_tute_3.png", width = "225px", height = "auto"),
                      p(),
                      Text(variant = "medium", "And you can click on which plates you would like to see in the legend [image below]."),
                      p(),
                      Image(src = "2_tutorial/5.2_qc_tute_4.png", width = "275px", height = "auto"),
                      p(),
                      Text(variant = "medium", "You will also notice that there are grey dots behind your data points. These indicate the WEHI standard curves that we have performed in house. These can be used as a guide to see if your standard curves fall within a similar line.")
                    ),
                    second_col = Image(src = "2_tutorial/5.2_qc_tute_2.png", width = "auto", height = "auto")
                  )
                ), 
                PivotItem(
                  headerText = "Step 2: Plate Quality Control",
                  p(),
                  renderTwoCols(
                    first_width = "30%", second_width = "70%",
                    first_col = list(
                      Text(variant = "medium","A summary of the bead counts for each plate well are displayed, with blue indicating there are sufficient beads (≥15) or red when there are not enough."), 
                      p(), 
                      Text(variant = "medium", " If any of the wells are red, they should be double-checked manually and re-run on a new plate if required."),
                      p(),
                      Text(variant = "medium", HTML(r"(The app will inform you whether there are "No repeats necessary" or provide a list of samples to be re-run. In the example data, the beads in plate 2 wells <b>A1</b> and <b>A2</b> will need to be repeated.)"))
                    ), 
                    second_col = Image(src = "2_tutorial/5.2_qc_tute_5.png", width = "auto", height = "auto")
                  )
                ), 
                PivotItem(
                  headerText = "Step 3: Blank Samples",
                  p(),
                  renderTwoCols(
                    first_width = "30%", second_width = "70%",
                    first_col = list(
                      Text(variant = "medium", HTML( r"(The Median Fluorescent Intensity (MFI) units for each antigen is displayed for your blank samples.
                                                     In general, each blank sample should have ≤50 MFI for each antigen, if they are higher they should be cross-checked manually.)")),
                      p(),
                      Text(variant = "medium", HTML("In the example data, blank samples recorded higher MFI values for <b>LF005</b> on plate 1 and should be checked to confirm this is expected from the assay."))
                    ), 
                    second_col = Image(src = "2_tutorial/5.2_qc_tute_6.png", width = "auto", height = "auto")
                  )
                ), 
                PivotItem(
                  headerText = "Step 4: Model Results",
                  p(),
                  renderTwoCols(
                    first_width = "30%", second_width = "70%",
                    first_col = list(
                      Text(variant = "medium", HTML(r"(The automated data processing in this app allows you to convert your Median Fluorescent Intensity (MFI) data into Relative Antibody Units (RAU) by fitting a 5-parameter
                          logistic function to the standard curve on a per-antigen level.)")),
                      p(),
                      Text(variant = "medium", "The results from this log-log conversion should look relatively linear for each antigen."),
                      p(),
                      MessageBar(HTML(r"(You can view each plate by pressing the "Next →" or "Previous ←" buttons provided.)"))
                    ), 
                    second_col = Image(src = "2_tutorial/5.2_qc_tute_7.png", width = "auto", height = "auto")
                  )
                ), 
                PivotItem(
                  headerText = "Step 5: Sample Results",
                  p(),
                  renderTwoCols(
                    first_width = "30%", second_width = "70%",
                    first_col = list(
                      Text(variant = "medium", HTML(r"(The results from the data processing are displayed in an interactive table that can be explored directly within the app and/or the data can also be downloaded in .csv format
                                                    in the <a href='#tutorial/download_processed' class=`link`>Download</a> tab.)")),
                      p(),
                      Text(variant = "medium", "The converted Relative Antibody Units (RAU) are displayed for each antigen and for each sample. Scroll to explore the data.")
                    ),
                    second_col = Image(src = "2_tutorial/5.2_qc_tute_8.png", width = "auto", height = "auto")
                  )
                ), 
                PivotItem(
                  headerText = "Step 6: Example",
                  Text(variant = "medium", HTML(r"(You can explore the processed data directly within the app, for example you can use the search bar to search for a particular sample name. 
                                                In the example below we filter for sample <b>ABC036</b> as shown in the red box at the top right hand side of the tab.)")),
                  Image(src = "2_tutorial/5.2_qc_tute_9.png", width = "auto", height = "auto"), 
                  Text(variant = "medium", HTML(r"(You can also re-order specific columns and sort them from highest to lowest, or vice versa by clicking on the triangles next to the column name. 
                                                In the example below we sorted from lowest to highest RAU value for <b>EBP</b>, as indicated in the red box.)")),
                  Image(src = "2_tutorial/5.2_qc_tute_10.png", width = "auto", height = "auto"),
                  p(),
                  Text(variant = "medium", HTML(r"(When you are finished you can download the data and quality control report by clicking on the <a href='#tutorial/download_processed' class-`link`>Download</a> tab.)")),
                  p(),
                  MessageBar("You can quickly check that your data processing has gone as expected by counting the number of entries in this table is the same as the number of samples (excluding standards and blanks)
                             in your plate layout, and to check that RAU values have been calculated for all of your antigens!")
                )
              )
              )
            ),
            makeCard(
              id = "tutorial/quality_control/download", 
              title = "5.3. Downloading Your Processed Data", 
              content = list(
                Text(variant = "medium", HTML(r"(You can download your processed data by clicking on the button "Download MFI/RAU Data". In some cases it may be useful to compare your standard curve data across multiple 
                 plate runs, so you can also download the standard curve data separately by clicking the button "Download Standard Curve Data". You can also download your quality control report, which includes 
                 all of the plots in the <a href='#tutorial/quality_control' class-`link`>Quality Control</a> tab as well as the information you entered in the <a href='#tutorial/import_data' class-`link`>Import Data</a>
                 tab (i.e., experiment name, notes, date and uploaded files).)")),
                Image(src = "2_tutorial/5.3_qc_tute_11.png", width = "auto", height = "auto")
              )
            )
          )
        ),
        p(),
        ######### ------------ Classifying your samples ------------
        div(
          id = "tutorial/classify",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "6. Classifying Your Samples"), alignContent = "start"),
            Text(variant = "medium", HTML("The built-in classification algorithm in this application should only be applied to data generated from the PvSeroTaT Multi-Antigen Luminex Assay to read more about this algorithm 
                                          click on the <a href = '#algorithm_page' class-'link'>Algorithm</a> section.")), 
            makeCard(
              id = "tutorial/classify/select", 
              title = "6.1. Select The Appropriate Algorithm", 
              content = list(
                renderTwoCols(
                  first_col = list(
                    Text(variant = "medium", HTML("We have developed two algorithms that were trained on the same datasets but have silghtly different antigen profiles.<br><br>
                                          <ul>
                                            <li><b>PvSeroApp Algorithm</b>: The PvSeroApp Model contains all top 8 antigens as described in the<a href='#algorithm' class-'link'>Algorithm Section</a>.</li>
                                            <li><b>PvSeroApp without LF016</b>: As the name suggests, this model contains the antigens in PvSeroApp except for LF016.</li>
                                          <ul>")), 
                    Text(variant = "medium", HTML("For specific details about the PvSeroApp algorithms, click on the <a href='#algorithm' class-'link'>Algorithm</a> Section in the navigation menu.")),
                    p()
                  ), 
                  second_col = Image(src = "2_tutorial/6.1_classify_tute_1.png", width = "auto", height = "auto")
                )
              )
            ), 
            makeCard(
              id = "tutorial/classify/perform", 
              title = "6.2. Perform Classification", 
              content = list(
                Text(variant = "medium", HTML(r"(You can select the algorithm that is appropriate for your data by clicking on the buttons on the left panel (see above). The example data was generated using the PvSeroTaT assay with all eight antigens 
                 and therefore you should select the "PvSeroApp Algorithm" for this tutorial.)"))
              )
            ), 
            makeCard(
              id = "tutorial/classify/results", 
              title = "6.3. Check Your Classification Results", 
              content = list(
                Text(variant = "medium", HTML("The results from the classification are displayed in an interactive table that can be explored directly within the app and/or the data can also be downloaded in .csv format in the 
                  adjacent <a href='#tutorial/download_classification' class-`link`>Download Data</a> tab. The exposure status (positive/negative) is displayed for each sample and for all classifiers.
                  Navigate to different pages using the numbers to view the data for all samples.")),
                p(),
                renderTwoCols(
                  first_col = list(
                    MessageBar(HTML(r"(Make sure that your data was processed using the correct algorithm by cross-checking the "Algorithm Choice" and "Threshold" which will display the algorithm and threshold that was applied.)")),
                    p(),
                    Text(variant = "medium", HTML("To form an educated guess about the positivity/exposure status for your samples, take into consideration all relevant epidemiological information that you may have available to you (e.g. time since 
                       possible exposure, timing of sampling with respect to incidence of <em>P. vivax</em> infections/cases) and explore the results using the classifier/s.")),
                    p(), 
                    MessageBar(messageBarType = 3, HTML("Remember that although the algorithms have high specificity and sensitivity, they are not 100% and so false positives/negatives may still occur. 
                       See the <a href='#algorithm' class-'link'>Algorithm Section</a> for more information."))
                  ), 
                  second_col = Image(src = "2_tutorial/6.3_classify_tute_2.png", width = "auto", height = "auto")
                )
              )
            ), 
            makeCard(
              id = "tutorial/classify/download", 
              title = "6.4. Downloading Your Classification Data", 
              content = list(
                Text(variant = "medium", HTML(r"(You can download your classification data by clicking on the button "Download Classification Data". Make sure that you click on the correct button depending on the algorithm 
                                          you applied to your data. The downloaded .csv file will include the algorithm, threshold and version in the file name, 
                                          which you can also cross-check to make sure the appropriate algorithm and threshold was applied to classify the data.)"))
              )
            )
          ) 
        ),
        p(),
        ######### ------------ Data visualisation ------------
        div(
          id = "tutorial/data_vis",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "7. Data Visualisation"), alignContent = "start"),
            Text(variant = "medium", "This web application has in-built data visualisation capabilities that allow you to explore your data."),
            MessageBar(HTML("For each of these plots, there are buttons on the top right of the plot that can be used to download the plot as a png (<i class='fas fa-camera'></i>), as well as other capabilties: <i class='fas fa-search'></i> zoom,  
              <i class='fas fa-arrows-alt'></i> move, <i class='fas fa-plus-square'></i> zoom in, <i class='fas fa-minus-square'></i> zoom out, <i class='fas fa-expand-arrows-alt'></i> autoscale, <i class='fas fa-home'></i> reset axes).")),
            makeCard(
              id = "tutorial/data_vis/class_boxplot",
              title = "7.1. Relative Antibody Units (RAU) per Antigen stratified by Classification", 
              content = list(
                Text(variant = "medium", "Here are the results of all classification thresholds explored in the PvSeroApp: balanced, 85%, 90% and 95% sensitivity, 85%, 90% and 95% specificity. The N's for the random forest model classifications based on
                     these classifications are displayed. You can visualise the RAU values for seropositive and seronegative isolates for each threshold by clicking on the threshold of interest (shown in blue here: clicking on the balanced threshold) and then 
                     visualising the boxplot (median, min and max RAU IQR values)."),
                p(),
                Image(src = "2_tutorial/7.1_datavis_tute_1.png", width = "auto", height = "auto")
              )
            ),
            makeCard(
              id = "tutorial/data_vis/mfi_boxplot",
              title = "7.2. Median Fluorescent Intensity (MFI) Boxplot Per Antigen", 
              content = list(
                Text(variant = "medium", "The box plots allow you to take a look at the converted Median Fluorescent Intensity (MFI) for each of the antigens in the panel. This plot is interactive and you can hover your mouse 
                 over each antigen box plot to see the min, median, max MFI and IQR values."),
                p(),
                Text(variant = "medium", "The grey boxplots in the background indicate the MFI values for the dataset used to train the PvSeroApp model."),
                p(),
                Image(src = "2_tutorial/7.2_datavis_tute_2.png", width = "auto", height = "auto"),
                p(),
                Text(variant = "medium", "By clicking on the antigens in the legend you can see the boxplot of the data observed for our study. This can be used as a comparison or visual check to see if your values are within a similar range. "),
                p(),
                Image(src = "2_tutorial/7.2_datavis_tute_3.png", width = "auto", height = "auto")
              )
            ),
            makeCard(
              id = "tutorial/data_vis/rau_boxplot",
              title = "7.3. Relative Antibody Units (RAU) Boxplot Per Antigen", 
              content = list(
                Text(variant = "medium","The box plots allow you to take a look at the converted Relative Antibody Units (RAU) for each of the antigens in the panel. This plot is interactive and you can hover your mouse 
                 over each antigen box plot to see the min, median, max MFI and IQR values."),
                p(),
                Text(variant = "medium", "The grey boxplots in the background indicate the MFI values for the dataset used to train the PvSeroApp model."),
                p(),
                Image(src = "2_tutorial/7.3_datavis_tute_4.png", width = "auto", height = "auto"), 
                p(),
                Text(variant = "medium", "By clicking on the antigens in the legend you can see the boxplot of the data observed for our study. This can be used as a comparison or visual check to see if your values are within a similar range. "),
                p(),
                Image(src = "2_tutorial/7.3_datavis_tute_5.png", width = "auto", height = "auto")
              )
            ),
            makeCard(
              id = "tutorial/data_vis/beads_perantigen",
              title = "7.4. Beads Counts per Antigen", 
              content = list(
                Text(variant = "medium", "This dot plot shows the raw bead counts for each antigen in each plate. This is intended as a quality control check to be able to identify which bead may be an issue."),
                p(),
                Image(src = "2_tutorial/7.4_datavis_tute_6.png", width = "auto", height = "auto"),
                p(),
                Text(variant = "medium", "By hovering over the dots you will be able to see the Sample ID (e.g., Blank2 [left] or ABC141 [right]), the Bead Count, Location on the plate (e.g., A2 [left], E9 [right]) and which plate this sample was on. 
                     The red dotted line indicates an arbitrary low threshold of 15 beads."),
                p(),
                renderTwoCols(
                  first_col = Image(src = "2_tutorial/7.4_datavis_tute_7.png", width = "400px", height = "auto"), 
                  second_col = Image(src = "2_tutorial/7.4_datavis_tute_8.png", width = "400px", height = "auto")
                )
              )
            )
          )
        ),
        p(),
        ######### ------------ Output from this tutorial ------------
        div(
          id = "tutorial/output_tutorial",
          Stack(
            tokens = list(childrenGap = 10),
            Separator(Text(variant = "xxLarge", "8. Output From This Tutorial"), alignContent = "start"),
            Text(variant = "medium", "If you followed along in this tutorial and downloaded your processed data, QC report and classification data, you can compare your files to the expected output from this tutorial."),
            Text(variant = "medium", HTML("Click here to download an example of the output from this tutorial <a href='https://github.com/dionnecargy/PvSeroApp/blob/main/data/example_output.zip' class='link'>here</a>.")),
            renderTwoCols(
              first_width = "80%", second_width = "20%", 
              first_col = Text(variant = "medium", HTML("If you have any <a href='#feature' class='link'>Requests for Features</a> or would like to <a href='#bug' class='link'>Report A Bug</a> please see the sections in the left-hand menu bar. 
                                          Alternatively there is a shortcut to these links at the top menu bar of the PvSeroApp with the GitHub Logo where you can publish your issues directly with the developer [see image to the right].")),
              second_col = Image(src = "2_tutorial/8_github_toggle.png", width = "200px", height = "auto")
            )
          )
        )
      ),
      ######### ------------ TOC (RHS)------------
      div(
        class = "nav-panel",
        style = "border-left: 1px solid #ccc; padding-left: 10px; position: sticky; top: 0; height: 100vh;",
        Text(variant = "large", "Contents"),
        Nav(
          groups = list(
            list(
              links = list(
                list(name = "1. Introduction", url = "#tutorial/intro", key = "intro"),
                list(
                  name = "2. Raw Data Requirements",
                  url = "#tutorial/rawdata", 
                  key = "raw0",
                  expandAriaLabel = "Expand Home section",
                  collapseAriaLabel = "Collapse Home section",
                  links = list(
                    list(name = "2.1. Antigen Label", url = "#tutorial/rawdata/antigens", key = "raw1"),
                    list(name = "2.2. Plate Layout", url = "#tutorial/rawdata/plate", key = "raw2"),
                    list(name = "2.3. Raw Data Types", url = "#tutorial/rawdata/platform", key = "raw3")
                  ),
                  isExpanded = TRUE
                ),
                list(
                  name = "3. Navigating the App",
                  url = "#tutorial/app_nav", 
                  key = "app_nav0"
                ),
                list(
                  name = "4. Importing Your Data",
                  url = "#tutorial/import_data/main", 
                  key = "import0",
                  expandAriaLabel = "Expand Home section",
                  collapseAriaLabel = "Collapse Home section",
                  links = list(
                    list(name = "4.1. Import Data", url = "#tutorial/import_data/import", key = "import1"),
                    list(name = "4.2. Check Data Is Loaded", url = "#tutorial/import_data/check", key = "import2")
                  ),
                  isExpanded = TRUE
                ),
                list(
                  name = "5. Quality Control",
                  url = "#tutorial/quality_control", 
                  key = "qc0",
                  expandAriaLabel = "Expand Home section",
                  collapseAriaLabel = "Collapse Home section",
                  links = list(
                    list(name = "5.1. Quality Control Report", url = "#tutorial/quality_control/location", key = "qc1"),
                    list(name = "5.1. Quality Control Check", url = "#tutorial/quality_control/check", key = "qc2"),
                    list(name = "5.2. Downloading Processed Data", url = "#tutorial/quality_control/download", key = "qc3")
                  ),
                  isExpanded = TRUE
                ),
                list(
                  name = "6. Classifying Your Samples",
                  url = "#tutorial/classify", 
                  key = "classify0",
                  links = list(
                    list(name = "6.1. Select Algorithm", url = "#tutorial/classify/select", key = "classify1"),
                    list(name = "6.2. Perform Classification", url = "#tutorial/classify/perform", key = "classify2"),
                    list(name = "6.3. Check Classified Results", url = "#tutorial/classify/results", key = "classify3"),
                    list(name = "6.4. Download Classified Data", url = "#tutorial/classify/download", key = "classify4")
                  ),
                  isExpanded = TRUE
                ),
                list(
                  name = "7. Data Visualisation",
                  url = "#tutorial/data_vis", 
                  key = "data_vis0"
                ),
                list(
                  name = "8. Output From This Tutorial",
                  url = "#tutorial/output_tutorial",
                  key = "output_tute"
                )
              )
            )
          )
        )
      )
    )
  )
}

###############################################################################
# --------------------------- Algorithm Page ---------------------------
###############################################################################

algorithm_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "The PvSeroTaT Algorithm"),
    Separator(),
    Text(variant = "medium", HTML("Here we present the machine learning method to identify individuals recently infected with <em>Plasmodium vivax<em> (within the last 9 months).")),
    p(),
    makeCard(
      id = "algorithm/samples_in_model", 
      title = "1. Samples Used In the Model", 
      content = list(
        Text(variant = "medium", HTML("Plasma samples from year-long observational cohort studies conducted in malaria-endemic regions in Thailand (Kanchanaburi and Ratchaburi provinces), Brazil (Manaus) and Solomon Islands (Ngella) were 
                                  measured for antigen-specific IgG antibody responses toward a panel of eight antigens using the method outlined in <a href='https://doi.org/10.1038/s41591-020-0841-4' target='_blank'>Longley et al 2020</a>.")),
        p(),
        Text(variant = "medium", HTML("Enrolled individuals in the year-long cohort studies provided a blood sample every month. Light microscopy and qPCR targeting the blood-stages of <em>P. vivax</em> were performed to 
                                      detect which individuals were infected and at which time point during these studies. IgG antibody responses towards a panel of <em>P. vivax</em> antigens were measured at the last visit of the 
                                      year-long study, enabling us to characterise antibody responses related with time since <em>P. vivax</em> infection. Negative controls from the Australian Red Cross, Brazil Red Cross, 
                                      Thai Red Cross and the Volunteer Blood Donor Registry in Victoria, Australia were included. These data were used as our <b>training dataset</b>.")),
        p(), 
        DTOutput("methods1"), 
        p(),
        MessageBar("For more information about the study design relevant to the development of this model, please refer to our manuscript IN PREPARATION.")
      )
    ),
    makeCard(
      id = "algorithm/antigens_in_model",
      title = "2. Antigens Used In the Model", 
      content = list(
        renderTwoCols(
          first_col = list(
            Text(variant = "medium", HTML("This work develops a sero-diagnostic test to balance the selection of serological exposure markers that are associated with high classification performance, with the selection of proteins that are easier
                                  to manufacture and are more stable.")),
            p(),
            Text(variant = "medium", HTML("Table 2 outlines the top eight <em>P. vivax</em> proteins selected in the model (discussed further below in the random forest classification methods), 
                                  their associated lifecycle stage and description of the proteins and expression system."))
          ),
          second_col = DTOutput("methods2")
        ),
        p(),
        MessageBar("For more details on the exploration of the top combinations of antigens considered as part of this test, their contribution to the performance of the model and single-antigen classification results, 
                   please refer to our manuscript IN PREPARATION.")
      )
    ),
    makeCard(
      id = "algorithm/random_forest", 
      title = "3. The Random Forest Model", 
      content = list(
        Text(variant = "medium", HTML(r"(Random forests are a machine learning algorithm which creates an "ensemble" or "sets" of decision trees (i.e., a "forest") from slightly different versions of the training dataset. 
                                      Each ensemble makes a separate prediction which is then averaged to get a more accurate and stable final prediction, resulting in good classification performance compared to a single decision tree 
                                      and little overfitting to the training data due to the use of multiple trees.)")),
        p(),
        Text(variant = "medium", HTML(r"(We trained a <b>random forest</b> classification algorithm to learn the patterns in antibody response for someone infected in the previous nine months. Because of our sampling method (outlined in 
                                      <a href='#algorithm/samples_in_model' class-`link`>Samples Used In the Model</a>), our training dataset includes "true positives" and "true negatives" which allows us to compare to the model predicted 
                                      "positives" and "negatives".)")),
        p(),
        Text(variant = "medium", HTML(r"(The Random Forest Model is trained and tested:<ul>
                                      <li>Create a random forest for every possible combination between two and eight antigens for the eleven top proteins and perform a 10-fold cross-validation with five-repeats*</li>
                                      <li>Once selected top 8 combination, hyperparameters are tuned to find the optimal values (mtry, nodes)</li>
                                      <li>These hyperparameter values are used to train the model on all of the data from the longitudinal cohort studies and negative controls used to classify new data (.rds file)</li>
                                      <li>The same parameter values are used and a 10-fold cross-validation with five repeats is performed on the training data to identify the maximium thresholds (.csv)</li>
                                      <li>The final trained model is ready to be applied to classify new data</li></ul>
                                      )")),
        Text(variant = "medium", HTML(r"(*Random sampling of the training dataset (e.g., 90% of the data) to then assess the predictive accuracy on the remaining (e.g., 10% of the data), for example using bootstrap aggregation (i.e., "bagging"). 
                                      This is repeated multiple times to perform "cross-validation" to give a clearer indication of the predictive accuracy of decision trees.)")),
        p(),
        Text(variant = "medium", HTML(r"(With respect to the PvSeroTaT classiciation model:<ul>
                                      <li>A decision tree makes a decision to "classify" whether someone has been recently infected with <em>P. vivax</em>.</li>
                                      <li>These sets of decision trees are uncorrelated.</li>
                                      <li>This results in a tree-like model of decisions with various possible outcomes. /li>
                                      <li>Each tree "votes" on how they classify the data and the majority of votes leads to the final class.</li>
                                      <li>For example, trees will assign a "seropositive" vote if the Relative Antibody Units (see tutorial) across the eight antigens are at or above a certain threshold.</li>
                                      </ul>)")),
        Text(variant = "medium", HTML("A <b>random forest</b> classification algorithm was created for all possible combinations for the eight antigens using 10-fold cross-validations with five repeats. 
                                      The final random forest was fit with 1,000 trees and all hyperparameters were default."))
      )
    ),
    makeCard(
      id = "algorithm/sens_spec", 
      title = "4. Sensitivity and Specificity",
      content = list(
        Text(variant = "large", "4.1. Some Definitions"), 
        Text(variant = "medium", HTML(r"(<ul> 
          <li><span style="color:#9373C0;">True Positive (TP)</span>, <span style="color:#2AA0A4;">True Negative (TN)</span>, <span style="color:#E3008C;">False Positive (FP)</span>, <span style="color:#CA5010;">False Negative (FN)</span>.</li>
          <li>Sensitivity: Tests the proportion of people who test positive among all of those who actually have the disease. Sensitivity = <span style="color:#9373C0;">TP</span> / (<span style="color:#9373C0;">TP</span> + <span style="color:#CA5010;">FN</span>).</li>
          <li>Specificity: Tests the proportion of people who test negative among all those who actually do not have the disease. Spec = <span style="color:#2AA0A4;">TN</span> / (<span style="color:#2AA0A4;">TN</span> + <span style="color:#E3008C;">FP</span>).</li>
          <li>Positive Predictive Value (PPV): Probability that following a positive test result, that individual will truly have the specific disease. PPV = <span style="color:#9373C0;">TP</span> / (<span style="color:#9373C0;">TP</span> + <span style="color:#E3008C;">FP</span>).</li>
          <li>Negative Predictive Value (NPV): Probability that following a negative test result, the individual will truly not have that specific disease. NPV = <span style="color:#2AA0A4;">TN</span> / (<span style="color:#2AA0A4;">TN</span> + <span style="color:#CA5010;">FN</span>).</li></ul>)")),
        Text(variant = "large", "4.2. Classification Performance: Receiver Operator Characteristic Curve"), 
        Text(variant = "medium", HTML("The Receiver Operator Characteristic (ROC) Curve is an indicator of the trade-off between a classification algorithm's sensitivity and specificity.
                                      Increasing <b>sensitivity</b> allows us to improve the ability to correctly identify true positive cases, while increasing <b>specificity</b> improves the ability to correctly identify true negative cases.")),
        Text(variant = "medium", HTML(r"(In our <a href='#model' class-`link`>classification page</a>, we have provided multiple options for you to choose which classification you would like to run:<ul>
                                        <li><b>Balanced: 81% Specificity / 81% Sensitivity</b>: Specificity and sensitivity are both at their highest</li>
                                        <li><b>90% Specificity / 67.5% Sensitivity</b>: 90% specificity is chosen an <span style="color:#C50F1F; font-weight: bold">even lower*</span> ability to correctly identify <span style="color:#9373C0;">True Positives</span> but 
                                      an <span style="color:#107C10; font-weight: bold">even greater**</span> ability to correctly identify <span style="color:#2AA0A4; font-weight: bold">True Negatives</span>.</li>
                                    </ul>)")), 
        Text(variant = "medium", "* = Than the sensitivity in the balanced threshold; ** = Than the specificity in the balanced threshold"), 
        renderTwoCols(
          first_col = list(
            Image(src = "3_algorithm/roc_longitudinal.png", width = "auto", height = "auto"), 
            Text(variant = "small", HTML(r"(Figure 1. Receiver Operating Characteristic (ROC) curves for the random forest classification algorithm thrained on the Thailand, Solomon Islands, Brazil and Australia serological data. The Area under the ROC (AUC) is 0.874. 
                 The red dashed lines are provided to highlight the "balanced" sensitivity and specificity according to the "balanced" random forest votes threshold.)"))
          ),
          
          second_col = list(
            Image(src = "3_algorithm/rf_votes_sens_spec.png", width = "auto", height = "auto"), 
            Text(variant = "small", HTML(r"(Figure 2. Random forest votes threshold for the sensitivity and specificity values. The value where sensitivity and specificity overlap is known as the "balanced" random forest votes threshold (or "balanced threshold" for short).)"))
          )
        ),
        p(),
        Text(variant = "large", "4.3. Classification Performance: The Confusion Matrix"), 
        renderTwoCols(
          first_col = list(
            Text(variant = "medium", HTML(r"(The confusion matrix provides a measure of the classifier's accuracy and is a 2x2 table of the "Truth" from our longitudinal dataset (columns) and the models's "Prediction" when we classify these data (rows)
                                      based on our trained model. The Top Left are the <span style="color:#9373C0; font-weight: bold">True Positives</span> and the Bottom Right are the <span style="color:#2AA0A4; font-weight: bold">True Negatives</span>. )")),
            p(),
            Text(variant = "medium", HTML(r"(In our dataset, for the "balanced" threshold, 366 individuals of 780 were correctly identified as "seropositive", while 1,769 of 1,855 were correctly identified as "seronegative". 
                                          The <b>PPV</b> is 0.469, <b>NPV</b> is 0.954, indicating that this model (a) may result in many false positives (b) and when the test result is <b>negative</b>, it is very likely that the individual truly does not have a recent infection.
                                          This is important in <b>low transmission</b> of <em>P. vivax</em> settings where the prevalence is low (e.g., in hypendemic countries where parasite rate is 0-10% in children aged 2-9 years as per the 
                                          <a href='https://www.who.int/publications/i/item/9789240038400' class-`link`>World Health Organisation Terminology, 2021</a>), therefore the rate of false positives is expected (low PPV). Here we are able to define the majority of 
                                          negative results will likely be correct (high NPV).)"))
          ), 
          second_col = list(
            gt_output("methods3")
          )
        )
      )
    )
  )
}

###############################################################################
# --------------------------- Inputs Page ---------------------------
###############################################################################
input_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "Import data"),
    Separator(),
    Text(variant = "medium", 
         HTML(r"(You can directly import the .xlsx output file from the Luminex machine to this app for data processing. Check that the file looks as expected using the  
               <b>"Check Raw Data"</b> tab and that the run info is correct using the <b>"Check Run Info"</b>.)")),
    p(),
    downloadLink("template_zip", "Click here to download the example data."),
    downloadLink("template_excel", "Click here for a template for the plate layout."),
    p(),
    Text(variant = "medium", 
         HTML(r"(In order to match the sample IDs with the raw data, import your .xlsx plate layout file. Make sure your plate layout is formatted correctly compared to the example, 
               you can check using the <b>"Check Plate Layout"</b> tab. There should be no extra rows or columns other than the plate map.)")),
    p(),
    MessageBar(messageBarType = 3, HTML("All data uploaded must be de-identified and not re-identifiable (see<a href='https://posit.co/about/posit-service-terms-of-use/' class='_blank'>Posit Service Terms of Use</a>)")),
    p(),
    MessageBar(messageBarType = 0, HTML(r"(You can label your raw data files using "luminexfile_plate1.csv" or if it is a <b>repeat</b> plate then you can use the convention "luminexfile_repeatplate1.csv". See the
                                        <a href='https://github.com/dionnecargy/PvSeroApp' class='_blank'>FAQs</a> on our GitHub for more details. For security reasons the app does not allow you to add multiple files 
                                        from different folders. For ease, please create a new folder with all of the files you would like to upload to the PvSeroApp so that you can add the files at once.)")),
    p(),
    useShinyjs(),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10), # Adjust spacing between buttons
      div(
        DefaultButton.shinyInput(
          "toggleTeachingBubble",
          id = "target",
          text = "Step-by-Step Tutorial"
        ),
        reactOutput("teaching_bubble_ui")
      ),
      DefaultButton.shinyInput(
        "previous_button",
        text = NULL,
        style = "background-color: #9e9b99; color: white; width: 20px;",
        iconProps = list(iconName = "ChevronLeft")
      ),
      DefaultButton.shinyInput(
        "next_button",
        text = NULL,
        style = "background-color: #54b054; color: white; width: 20px;",
        iconProps = list(iconName = "ChevronRight")
      )
    ),
    Separator(),
    Stack(
      tokens = list(childrenGap = 20),
      # Sub-Pages
      Pivot(
        PivotItem(
          headerText = "Import Your Data",
          renderTwoCols(
            first_width = 400, second_width = 250,
            first_col = list(
              div(
                tokens = list(childrenGap = 15),
                children = list(
                  # Input experiment name (default: experiment1)
                  TextField.shinyInput(
                    inputId = "experiment_name",
                    label = "1. Enter your Experiment Name:",
                    placeholder = "experiment1",
                    value = "experiment1",
                    required = TRUE,
                    styles = list(root = list(width = 300))
                  ),
                  # Input the date (default: today's date)
                  DatePicker.shinyInput(
                    inputId = "date",
                    placeholder = Sys.Date(),
                    label = "2. Enter Date:",
                    format = "yyyy-mm-dd",
                    isRequired = TRUE,
                    style = list(width = 300)
                  ),
                  # Input the date (optional)
                  TextField.shinyInput(
                    inputId = "experiment_notes",
                    label = "3. Enter any Experiment Notes:",
                    placeholder = "Enter experiment details/notes here",
                    styles = list(root = list(width = 300))
                  ),
                  # Input the platform used (default: magpix)
                  Label(HTML("4. Select Platform: <span style='color:#a4262c;'>*</span>")),
                  div(
                    radioButtons(
                      "platform",
                      label = NULL,
                        choices = list("Bio-Plex" = "bioplex", "MAGPIX" = "magpix", "INTELLIFLEX" = "intelliflex"),
                      selected = "magpix"),
                    style = "margin-top: 10px;"
                  ), 
                  # Only shown when Magpix is selected
                  conditionalPanel(
                    condition = "input.platform == 'magpix'",
                    Label("Magpix software version"),
                    div(
                      radioButtons(
                        "magpix_version",
                        label = NULL,
                        choices = c("4.2", "4.3"),
                        selected = "4.2"
                      ),
                      style = "margin-left: 20px; margin-top: 10px;"
                    )
                  ),
                  # Input for standard curve only or not  (optional)
                  Label(HTML("5. Are you only looking at the standard curve? <span style='color:#a4262c;'>*</span>")),
                  div(
                    radioButtons(
                      "standardcurveonly",
                      label = NULL,
                      choices = list("Yes", "No"),
                      selected = "No"),
                    style = "margin-top: 10px;"
                  ),
                  # Input type of standard curve (default: 10-point)
                  Label(HTML("6. What type of standard curve are you using? <span style='color:#a4262c;'>*</span>")),
                  div(
                    radioButtons(
                      "std_point",
                      label = NULL,
                      choices = list("5-point" = 5, "10-point" = 10),
                      selected = 10
                    ),
                    style = "margin-top: 10px;"
                  )
                )
              )
            ), 
            second_col = list(
              div(
                tokens = list(childrenGap = 15),
                children = list(
                  Label(HTML("7. Upload Files: <span style='color:#a4262c;'>*</span>")),
                  Text(variant = "medium", "Upload Raw Data Files (.xlsx or .csv):"),
                  fileInput("raw_data", label = NULL, multiple = TRUE, accept = c(".csv", ".xlsx"), buttonLabel = NULL),
                  uiOutput("uploadMessage1"),  # Output to display the success message
                  Text(variant = "medium", "Upload Plate Layout (.xlsx):"),
                  fileInput("plate_layout", label = NULL, multiple = TRUE, accept = ".xlsx", buttonLabel = NULL),
                  uiOutput("uploadMessage2"), # Output to display the success message
                  Label(HTML("8. Save Inputs: <span style='color:#a4262c;'>*</span>")),
                  div(
                    style = "margin-bottom: 10px;",  # Space for the text above the button
                    PrimaryButton.shinyInput(
                      "save_inputs",
                      text = "Save Inputs",
                      iconProps = list(iconName = "Save"),
                      style = "margin-bottom: 10px; display: block;"  # Ensures the button is on a new line
                    )
                  ),
                  uiOutput("notification")
                )
              )
            )
          ),
          Separator(),
          Label("Click here to reset the inputs!"),
          div(
            style = "margin-bottom: 10px;",  # Space for the text above the button
            PrimaryButton.shinyInput(
              "resetAll",
              text = "Reset All",
              iconProps = list(iconName = "Refresh"),
              style = "margin-bottom: 10px; display: block;"  # Ensures the button is on a new line
            )
          )
        ),
        # Check Raw Data
        PivotItem(headerText = "Check Raw Data", 
                  MessageBar("Review the raw data imported from the file."),
                  textOutput("raw_data_filename"), 
                  # withSpinner(uiOutput("alldata"), type = 8) # add spinner
                  uiOutput("alldata")
        ),
        # Check Run Info
        PivotItem(headerText = "Check Run Info", 
                  MessageBar("Verify the run information."),
                  textOutput("raw_data_filename"),
                  # withSpinner(uiOutput("runinfo"), type = 8) # add spinner 
                  uiOutput("runinfo")
        ),
        # Check Plate Layout
        PivotItem(
          headerText = "Check Plate Layout", 
          MessageBar("Inspect the plate layout."),
          MessageBar(
            messageBarType = 3,  # 0 = info (default), 1 = error, 2 = blocked, 3 = warning, 4 = success
            isMultiline = TRUE,
            "Important things to note: Make sure that your standards are labeled appropriately starting with 'S' so that the app can recognize your standards and if you have 
                more than one blank sample, make sure you label them as 'Blank 1', 'Blank 2', etc. (for more details on the specific layout requirements, see 'Tutorial')."
          ),
          textOutput("plate_layout_filename"),
          div(
            class = "plot-container", 
            actionButton("dec_plate", "←", class = "btn-primary"),
            div(class = "plate-window", uiOutput("individual_plate")),
            actionButton("inc_plate", "→", class = "btn-primary")
          )
        )
      )
    )
  )
}

###############################################################################
# --------------------------- QC Page ---------------------------
###############################################################################
check_page <- function() {
  fluentPage(
    useWaiter(),
    useShinyjs(),
    Text(variant = "xxLarge", "Quality control"),
    Separator(),
    Text(variant = "medium", HTML("The automated data processing in this app allows you to convert your <b>Median Fluorescence Units (MFI)</b> data into <b>Relative Antibody Units (RAU)</b> by fitting 
         a 5-parameter logistic function to the standard curve on a per-antigen level. 
         <br><br>Use the different tabs to visualise the quality control of your run!")),
    p(),
    MessageBar(messageBarType = 3, "You must first pick a standard pool from the dropdown menu below. Please check your laboratory records if you are uncertain which pool was used."),
    p(),
    div(
      Dropdown.shinyInput(
        "dropdown_stds", 
        label = "Choose Your Standard Curve Pool",
        required = TRUE,
        placeholder = "Select a Pool Option", 
        options = list(
          list(key = "PNG", text = "PNG Pool"),
          list(key = "ETH", text = "Ethiopian Pool")
        ),
        styles = list(root = list(width = 300))),
      textOutput("dropdownValue")
    ),
    p(),
    MessageBar("Click the buttons below to download the data and quality control report. The report and .zip file may take some time to load."),
    p(),
    waiterOnBusy(html = spin_3(), color = transparent(.5)),
    div(
      style = "display: flex; gap: 10px;",
      PrimaryButton.shinyInput(
        "downloadButtonData",
        text = "MFI/RAU data (.CSV)",
        iconProps = list(iconName = "Download")
      ),
      div(style = "visibility: hidden;", downloadButton("downloadData", "")),
      PrimaryButton.shinyInput(
        "downloadButtonStds",
        text = "Standard Curve Data (.CSV)",
        iconProps = list(iconName = "Download")
      ),
      div(style = "visibility: hidden;", downloadButton("downloadStds","")),
      PrimaryButton.shinyInput(
        "downloadButtonReport",
        text = "Quality Control Report (.PDF)",
        iconProps = list(iconName = "Download")
      ),
      div(style = "visibility: hidden;", downloadButton("report", "")),
      PrimaryButton.shinyInput(
        "downloadButtonZip",
        text = "All Files (.ZIP)",
        iconProps = list(iconName = "Download")
      ),
      div(style = "visibility: hidden;", triggerWaiter(downloadButton("download_zip", "")))
    ), 
    Separator(),
    Pivot(
      PivotItem(
        headerText = "Standard Curves",
        MessageBar(HTML(r"(Check the standard curves for each protein below. Grey dots in the background indicate the standard curve range observed at WEHI for "PNG" or "Ethiopian" 
                        samples where there are known differences in the standard curve.)")),
        plotlyOutput("stdcurve", height = "1000px")
      ),
      PivotItem(
        headerText = "Plate QC", 
        MessageBar("For quality control of each plate, we check that each well should have ≥15 beads/well. Any wells with <15 beads/well are indicated in red below and should be double-checked manually."),
        uiOutput("plateqc"),
        conditionalPanel(
          condition = "output.check_repeats_text !== null",
          textOutput("check_repeats_text")
        ),
        conditionalPanel(
          condition = "output.check_repeats_table !== null",
          DT::dataTableOutput("check_repeats_table")
        )
      ), 
      PivotItem(
        headerText = "Blank Samples",
        MessageBar("Blank samples should record MFI<50 for each protein. If any proteins are above the dashed line, they should be double-checked manually. 
                   If you have more than one blank sample in your plate, make sure to label them as 'Blank1', 'Blank2' and so forth, otherwise the data shown here will be a cumulative result of all your blanks."), 
        uiOutput("blanks")
      ), 
      PivotItem(
        headerText = "Model Results",
        MessageBar("The 5-paramater logistic function is a log-log model that is used to obtain a more linear relationship. Check that the results from the model look relatively linear for each protein."),
        div(
          class = "plot-container", 
          actionButton("dec", "←", class = "btn-primary"),
          div(class = "plot-window", plotOutput("individual_plot", height = "1000px", width = "100%")),
          actionButton("inc", "→", class = "btn-primary")
        )
      ), 
      PivotItem(
        headerText = "Sample Results",
        MessageBar("The table below displays the RAU conversions and is interactive (i.e., you can search and filter as required). Click above to download your processed data."), 
        DT::dataTableOutput("results")
      )
    )
  )
}


###############################################################################
# --------------------------- Run Classification Page ---------------------------
###############################################################################
model_page <- function() {
  fluentPage(
    waiterOnBusy(html = spin_3(), color = transparent(.5)),
    Text(variant = "xxLarge", "Classify exposure"),
    Separator(),
    Text(variant = "medium", HTML("Using this tool, you can classify individuals as seropositive or seronegative for recent <em>Plasmodium vivax</em> exposure (i.e. within previous 9 months). 
                                    You can include additional columns, such as sample IDs.")), 
    p(),
    MessageBar(messageBarType = 3, # 0 = info (default), 1 = error, 2 = blocked, 3 = warning, 4 = success
               "Disclaimer: the results obtained from this classification are for research purposes only and should not be considered a diagnosis."),
    p(),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10), # Adjust spacing between buttons
      div(
        DefaultButton.shinyInput(
          "toggleTeachingBubble_CE",
          id = "target_CE",
          text = "Step-by-Step Tutorial"
        ),
        reactOutput("teaching_bubble_CE")
      ),
      DefaultButton.shinyInput(
        "previous_button_CE",
        text = NULL,
        style = "background-color: #9e9b99; color: white; width: 20px;",
        iconProps = list(iconName = "ChevronLeft")
      ),
      DefaultButton.shinyInput(
        "next_button_CE",
        text = NULL,
        style = "background-color: #54b054; color: white; width: 20px;",
        iconProps = list(iconName = "ChevronRight")
      )
    ),
    Separator(),
    Stack(
      tokens = list(childrenGap = 5),
      horizontal = TRUE,
      children = list(
        # Step 1: Select Algorithm
        div(
          style = list(width = "33.3%"),
          Label(HTML("1. Select Algorithm Type: <span style='color:#a4262c;'>*</span>")),
          div(
            radioButtons(
              "algorithm",
              label = NULL,
              choices = list(
                "PvSeroApp Algorithm" = "antibody_model",
                "PvSeroApp Algorithm without LF016" = "antibody_model_excLF016"
              ),
              selected = "antibody_model"
            ),
            style = "margin-top: 10px;"
          )
        ),
        # Step 2: Select Sensitivity/Specificity
        div(
          style = list(width = "33.3%"),
          Label(HTML("2. Select Sensitivity/Specificity Type: <span style='color:#a4262c;'>*</span>")),
          div(
            radioButtons(
              "sens_spec",
              label = NULL,
              choices = list(
                "Balanced: 81% Specificity / 81% Sensitivity" = "balanced",
                # "85% Sensitivity / 75% Specificity" = "85% sensitivity",
                # "90% Sensitivity / 61.6% Specificity" = "90% sensitivity",
                # "95% Sensitivity / 43.4% Specificity" = "95% sensitivity",
                # "75% Sensitivity / 85% Specificity" = "85% specificity",
                "90% Specificity / 67.5% Sensitivity" = "90% specificity"#,
                # "52.4% Sensitivity / 95% Specificity" = "95% specificity"
              ),
              selected = "balanced"
            ),
            style = "margin-top: 10px;"
          )
        ),
        # Step 3: Run Button 
        div(
          style = list(width = "33.3%"),
          Label(HTML("3. Press Button to Run Classification: <span style='color:#a4262c;'>*</span>")),
          PrimaryButton.shinyInput(
            "run_classification",
            text = "Run Classification",
            iconProps = list(iconName = "Play")
          )
        )
      )
    ),
    Label(HTML("Classification results:")),
    Separator(),
    textOutput("result"),
    tableOutput("classification_summary"),
    PrimaryButton.shinyInput(
      "downloadButtonClassify",
      text = "Download Classification Data (.CSV)",
      iconProps = list(iconName = "Download")
    ),
    div(style = "visibility: hidden;", downloadButton("download_classification", label = ""))
  )
}

###############################################################################
# --------------------------- Data Visualisation Page ---------------------------
###############################################################################
datavis_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "Data Visualisation"),
    Separator(),
    MessageBar("Data Visualisation features can be made available upon request. Please note that the figure may take some time to load."),
    p(),
    Text(variant = "large", HTML("<b>Classification</b>")),
    Separator(),
    Label(HTML("<b>Select Sensitivity/Specificity Type: <span style='color:#a4262c;'>*</span></b>")),
    MessageBar("Click on the Sensitivity/Specificity Random Forest Threshold of interest to see a boxplot appear! You can click on any threshold to see how increasing or decreasing each value changes the interquartile range and median of RAU values."),
    DTOutput("allclassifytable"),
    p(),
    Label(HTML("<b>Relative Antibody Units (RAU) per Antigen stratified by Classification</b>")),
    plotlyOutput("classify_plots"),
    p(),
    Text(variant = "large", HTML("<b>Quality Control Checks: Supplementary</b>")),
    Separator(),
    MessageBar("Here we present your data in the coloured boxplots and the dataset used in this model is presented in grey behind. By clicking on the antigens in the legend you can see the 
               boxplot of the data observed for our study. This can be used as a comparison or visual check to see if your values are within a similar range. Please note that these MFI and RAU values
               will vary by epidemiological setting."),
    p(),
    Label(HTML("<b>Median Fluorescent Intensity (MFI) Per Antigen</b>")),
    p(),
    Text(variant = "medium", "The box plots allow you to take a look at the converted Median Fluorescent Intensity (MFI) for each of the antigens in the panel. 
         This plot is interactive and you can click on the antigen names on the right hand side to see the difference with the grey background (our training dataset)."),
    p(),
    plotlyOutput("mfi_plotly"),
    p(),
    Label(HTML("<b>Relative Antibody Units (RAU) Per Antigen</b>")), 
    p(),
    Text(variant = "medium", "The box plots allow you to take a look at the converted Relative Antibody Units (RAU) for each of the antigens in the panel. 
         This plot is interactive and you can click on the antigen names on the right hand side to see the difference with the grey background (our training dataset)."),
    p(),
    plotlyOutput("rau_plotly"),
    p(),
    Label(HTML("<b>Bead Counts per Antigen</b>")), 
    p(),
    Text(variant = "medium", "This dot plot is also interactive and displays the raw bead counts for each antigen in each plate. The red dotted line indicates an arbitrary low threshold of 15 beads."),
    p(),
    plotlyOutput("bead_count_plotly")
  )
}

###############################################################################
# --------------------------- App Version History Page ---------------------------
###############################################################################

apphistory_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "PvSeroApp Version History"),
    Separator(),
    # Image(src = "4_feedback/app_history.png", width = "800px", height = "auto"),
    # p(),
    Text(variant = "large", HTML("<b>v0</b>")),
    Text(variant = "medium", "All version numbers starting with 0 indicate working releases where the developer was making updates regularly and troubleshooting features. The classification algorithm remains the same as version 1.0.0 onwards and each future release will be defined by the algorithm updates."),
    p(),
    Text(variant = "large", HTML("<b>Test v0.1.0</b>")),
    Text(variant = "medium", "This is the first testing release of the PvSeroApp Web Application!"),
    p(),
    Text(variant = "large", HTML("<b>v0.2.0</b>")),
    Text(variant = "medium", "This is the version that contains a classification model that works as anticipated. There are some minor errors that need to be fixed (e.g., pdf not exporting, dark mode not active, tutorial descriptions and images need updating)."),
    p(),
    Text(variant = "large", HTML("<b>v1.0.0</b>")),
    Text(variant = "medium", "Key updates made to the tutorial, inputs more flexible, data visualisation options and export data. The standard curve conversion factor for Ethiopian pools to the Papua New Guinea pool has been added. Same classification model as v0.1.0 and v0.2.0."),
    p(),
    Text(variant = "large", HTML("<b>v1.1.0</b>")),
    Text(variant = "medium", "Update to the Ethiopian standard curve conversion function `MFItoRAU_ETH` to account for the relative minimum dilution. The app now has loading spinners and notifications for troubleshooting."),
    p(),
    Text(variant = "large", HTML("<b>v1.2.0</b>")),
    Text(variant = "medium", 
         HTML(r"(Large update:<ul>
              <li>Added QC pass/fail column in classification output</li>
              <li>Added QC pass/fail column in MFI to RAU output</li>
              <li>Rename column name in classification output to threshold chosen</li>
              <li>Add "ETH" or "PNG" pool name to output</li>
              <li>Add "Repeat" to input file name</li>
              <li>Cleaned functions to reduce redundancy</li>
              <li>Added spinners to download buttons</li>
              <li>Added European CSV compatibility</li>
              <li> Checked PNG and ETH pool MFI to RAU conversions</li>
              </ul> )")),
    p(),
    Text(variant = "large", HTML("<b>v1.3.0</b>")),
    Text(variant = "medium", 
         HTML(r"(This new release includes:<ul>
              <li>Fixed bioplex compatibility in the MFItoRAU_ETH function (see pull request #5)</li>
              <li>Fixed melt_csv for .csv upload (see pull request #6)</li>
              <li>Conversion between RAU using the Ethiopian pool and RAU using the PNG pool (see pull request #7)</li>
              <li>Fixed readSeroData for .xlsx magpix (see pull request #8)</li>
              </ul>
         )")),
    p(),
    Text(variant = "large", HTML("<b>v1.3.1.</b>")),
    Text(variant = "medium", "Bug Fixes as described in pull request #9 and removed warning for check_platforms function."),
    p(),
    Text(variant = "large", HTML("<b>v1.4.0.</b>")),
    Text(variant = "medium", 
         HTML(r"(This new release includes:<ul>
              <li>Added intelliflex compatibility to the entire application</li>
              <li>Fixed typos throughout the application</li>
              <li>Added multiple plate layout capability inputs (see pull request #10)</li>
              <li>All functions are now solely from the SeroTrackR R package</li>
              </ul>
         )")),    
    p(),
    Text(variant = "large", HTML("<b>v1.5.0.</b>")),
    Text(variant = "medium", 
         HTML(r"(This new release includes:<ul>
              <li>Added xPONENT software compatibility to the application</li>
              <li>Added standard curve types as an input to the application</li>
              <li>Updated functions to be compatible with SeroTrackR v1.0.0</li>
              </ul>
         )"))
  )
}

###############################################################################
# --------------------------- Request a Feature Page ---------------------------
###############################################################################

requestfeature_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "Request A Feature"),
    Separator(),
    Text(variant = "medium", HTML(r"(You can <a href='https://github.com/dionnecargy/pvseroapp/issues' class-'link’>Request A Feature</a> on <b>GitHub</b>. 
                                  On this page, you can suggest new functionalities, improvements, or enhancements for a project. 
                                  A well-written feature request should include: <ul>
                        <li>A clear description of the feature.</li>
                        <li>Use cases explaining why it's needed.</li>
                        <li>Potential benefits for users or the project.</li>
                        <li>Examples (if applicable).</li>
                    </ul>)")),
    p(),
    MessageBar(messageBarType = 3, 
               HTML("Please note that this is a <b>Public Forum</b> where <b>anyone</b> can view any issues and bugs, as well as solutions. Do <b>not</b> upload any 
                    confidential data that can be saved by anyone.")),
    p(),
    Text(variant = "medium", 
         HTML("Alternatively, if you wish to contact the developers by email, please email us <a href='mailto:argyropoulos.d@wehi.edu.au' class-'link’>here</a>. 
              If you choose this option, please still follow the steps below in your email.")),
    p(),
    Text(variant = "large", "Step 1: Create an Issue"), 
    p(),
    Text(variant = "medium", HTML(r"(This page displays all open and closed issues and requests. Users can search for specific issues and filter them by labels, status etc. To create your own feature request,
                                  click on the "New Issue" button shown in the RED box in the image below.)")),
    p(),
    Image(src = "4_feedback/report_a_bug_1.png", width = "1000px", height = "auto"),
    p(),
    Text(variant = "medium", HTML("This will take you to a menu where you can create a <b>Feature Request</b>. Click this button.")),
    p(),
    Image(src = "4_feedback/request_a_feature_1.png", width = "1000px", height = "auto"),
    p(),
    Text(variant = "large", "Step 3: Describe your Feature Request"), 
    p(),
    Text(variant = "medium", "Each issue has a title, description, comments section and metadata such as tags, assignments and milestones. "),
    Text(variant = "medium", "1. Please write a concise title using keywords, and we would greatly appreciate a clear description. Follow the prompts in the template if you are unsure what to write."),
    Text(variant = "medium", "2. If you have an image or file/s that can support your vision, please attach the image or file/s where the BLUE box is shown in the image below. "),
    Text(variant = "medium", HTML(r"(3. To upload the issue to GitHub, click on the "Create" button shown in the RED box in the image below.)")),
    p(),
    Image(src = "4_feedback/request_a_feature_2.png", width = "1000px", height = "auto"),  
    p(),
    Text(variant = "medium", HTML(r"(Optional: Before you upload your issue, you can click the "Label" Button as indicated in the YELLOW box in the image below to add
                                  relevant tags for better categorisation.)")),
    p(),
    Image(src = "4_feedback/request_a_feature_3.png", width = "1000px", height = "auto")
  )
}

###############################################################################
# --------------------------- Report A Bug Page ---------------------------
###############################################################################
reportbug_page <- function() {
  fluentPage(
    Text(variant = "xxLarge", "Report A Bug"),
    Separator(),
    Text(variant = "medium", HTML("You can <a href='https://github.com/dionnecargy/pvseroapp/issues' class-'link’>Report A Bug</a> on <b>GitHub</b>. 
                                  Here you will be prompted to write out the problem you are facing and are encouraged to add any images or files that may be used for
                                  troubleshooting purposes.If your issue has been resolved or turned out to be a user error with a known fix, consider posting it on GitHub. 
                                  This helps others who may encounter the same problem find a solution more easily.")),
    p(),
    MessageBar(messageBarType = 3, 
               HTML("Please note that this is a <b>Public Forum</b> where <b>anyone</b> can view any issues and bugs, as well as solutions. Do <b>not</b> upload any 
                    confidential data that can be saved by anyone.")),
    p(),
    Text(variant = "medium", 
         HTML("Alternatively, if you wish to contact the developers by email, please email us <a href='mailto:argyropoulos.d@wehi.edu.au' class-'link’>here</a>. 
              If you choose this option, please still follow the steps below in your email.")),
    p(),
    Text(variant = "large", "Step 1: Create an Issue"), 
    p(),
    Text(variant = "medium", HTML(r"(This page displays all open and closed issues. Users can search for specific issues and filter them by labels, status etc. To create your own issue,
                                  click on the "New Issue" button shown in the RED box in the image below.)")),
    p(),
    Image(src = "4_feedback/report_a_bug_1.png", width = "1000px", height = "auto"),
    p(),
    Text(variant = "medium", HTML("This will take you to a menu where you can create a <b>Bug Report</b>. Click this button.")),
    p(),
    Image(src = "4_feedback/report_a_bug_2.png", width = "1000px", height = "auto"),
    p(),
    Text(variant = "large", "Step 3: Describe your Issue"), 
    p(),
    Text(variant = "medium", "Each issue has a title, description, comments section and metadata such as tags, assignments and milestones. "),
    Text(variant = "medium", "1. Please write a concise title using keywords, and we would greatly appreciate a clear description of your issue with images to support. Follow the prompts in the template if you are unsure what to write."),
    Text(variant = "medium", "2. Images can be attached where the BLUE box is shown in the image below. "),
    Text(variant = "medium", HTML(r"(3. To upload the issue to GitHub, click on the "Create" button shown in the RED box in the image below.)")),
    p(),
    Image(src = "4_feedback/report_a_bug_3.png", width = "1000px", height = "auto"),  
    p(),
    Text(variant = "medium", HTML(r"(Optional: Before you upload your issue, you can click the "Label" Button as indicated in the YELLOW box in the image below to add
                                  relevant tags for better categorisation.)")),
    p(),
    Image(src = "4_feedback/report_a_bug_4.png", width = "1000px", height = "auto")
  )
}
