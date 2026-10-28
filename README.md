[![DOI](https://zenodo.org/badge/923808390.svg)](https://doi.org/10.5281/zenodo.23095141)

# <img src="https://github.com/dionnecargy/pvseroapp/blob/main/www/hex/PvSeroApp_sticker.png" width="25%" height="25%" align="left"/> PvSeroApp
The source code for the PvSeroApp Shiny web application, which streamlines the data processing of the multi-antigen Luminex-based <em>Plasmodium vivax</em> serological data and applies the machine learning classification algorithm to identify individuals with recent exposure to <em>P. vivax</em>.

## Citing PvSeroApp 

If you are using the `{PvSeroApp}` in your work, please cite: 

> Smith L, Argyropoulos DC, Bareng APN, Lin J, Kiernan-Walker N, Lamont M, Abraham A, Lim PS, Wu KW, William T, Anstey NM, Grigg MJ, Sattabongkot J, Lacerda M, Vahi V, Mazhari R, Mueller I, Longley RJ. *An optimised serological machine learning model enabling targeted test-and-treat for Plasmodium vivax malaria.* medRxiv 2026. <https://doi.org/10.64898/2026.08.09.26355326>

If you would like to customise the outputs of the `{PvSeroApp}`, see the `{SeroTrackR}` R package [here](https://github.com/dionnecargy/SeroTrackR). This R package contains the source code used in the `{PvSeroApp}` and is intended for experienced users who are comfortable with implementing the code in their own R sessions. 

## FAQs
### Question 1. 
>[!NOTE]
>How do I name my raw luminex files?

Please name them with the following convention with `plate1` (other characters and capitalisations are OPTIONAL). The important part is that the labels correspond to the plate layout files as indicated in the next Question!
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

As of v1.2.0 you can label your plates with "REPEATplate1" or "Repeatplate1" or "repeatplate1" e.g., "luminexfile_todaysdate_repeatplate3.csv". 

The code searches for the string `plate` and the number that follows. It also accepts `repeatplate` and the number that follows. The words `repeat` and `plate` are allowed to have any capitalisations. Other numbers are allowed in the name as well, but ensure they are spaced with an underscore or another string e.g., `02473292_experiment20238_plate1_03042025.csv` is acceptable. 

### Question 2. 
>[!NOTE]
>How do I name my plate layout files?

You can label it however you like - however you only **need ONE plate layout .xlsx file**! Within this document, each **tab** should correspond to a **plate** and should be labelled `plate1`, `plate2`, ..., etc. (no capitals or spaces). 

### Question 3. 
>[!TIP]
>How can I access the "Step-by-Step tutorial"?

On the [Input](https://dionnecargy.shinyapps.io/pvserotat/#input) page and [Classify Exposure](https://dionnecargy.shinyapps.io/pvserotat/#model) page there is a quick step-by-step tutorial with left and right buttons. Just simply click the "Step-By-Step Tutorial" button and it will lead you through the rest!

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"> CC BY-NC-SA 4.0 International Licence</a>.
