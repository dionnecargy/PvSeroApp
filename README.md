# <img src="https://github.com/dionnecargy/pvseroapp/blob/main/www/PvSeroApp.png" width="25%" height="25%" align="left"/> PvSeroApp
The source code for the PvSeroApp Shiny web application, which streamlines the data processing of the multi-antigen Luminex-based <em>Plasmodium vivax</em> serological data and applies the machine learning classification algorithm to identify individuals with recent exposure to <em>P. vivax</em>.

# FAQs
## Question 1. 
>[!NOTE]
>How do I name my raw luminex files?

Please name them with the folowing convention with `plate1` with no capitals or spaces, and no other numbers in the file name. 
- If you're using .xlsx files:
```
luminexfile_plate1.xlsx
luminexfile_plate2.xlsx #....etc.
```
- If you're using .csv files:
```
luminexfile_plate1.csv
luminexfile_plate2.csv #....etc.
```

## Question 2. 
>[!NOTE]
>How do I name my plate layout files?

You can label it however you like - however you only **need ONE plate layout .xlsx file**! Within this document, each **tab** should correspond to a **plate** and should be labelled `plate1`, `plate2`, ..., etc. (no capitals or spaces). 

## Question 3. 
>[!TIP]
>How can I access the "Step-by-Step tutorial"?

On the [Input](https://dionnecargy.shinyapps.io/pvserotat/#input) page and [Classify Exposure](https://dionnecargy.shinyapps.io/pvserotat/#model) page there is a quick step-by-step tutorial with left and right buttons. Just simply click the "Step-By-Step Tutorial" button and it will lead you through the rest!
