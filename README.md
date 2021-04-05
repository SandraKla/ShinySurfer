# Software tool for the Analysis and Visualization of MRI images of the brain

[![License](https://img.shields.io/github/license/SandraKla/MRI_Analysis.svg)]()

<img src="www/example.png" align="center"/>

For more information use the [Homepage](https://sandrakla.github.io/ShinySurfer_Homepage/).

## Installation 

Download the Zip-File from this Shiny App and set your working direction to this path and run:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")}
```

```bash
library(shiny)
runApp("app.R")
```
Or use the function ```runGitHub()``` from the package *shiny*:

```bash
library(shiny)
runGitHub("ShinySurfer", "SandraKla")
```

All required packages must be downloaded before starting this app. For more information about the required packages use the [Homepage](https://sandrakla.github.io/ShinySurfer_Homepage/).

## Usage

### Load data with the old format

The file must be in .xls or .xlsx format (see OASIS.xlsx). And the check-button "Old data format" must be activated! The data order of the columns is ID, sex, age. The remaining columns are 74 regions of the left hemisphere and 74 regions of the right hemisphere. All column names must exist, but related data can be null. In this OASIS data set, there are 300 data sets in total.

### New format

The .txt files obtained after analysis in Freesurfer can be immediately used further in the Shiny app (see a2009s_thickness_lh.txt and a2009s_thickness_rh.txt). The check-button "Old data format" should not be activated this time! The file demograhics.csv has additional information about the patients (like gender and age).

### Names Correction (only old format)

Different users have different names for the same brain region, so in this Shiny App you can import a file for this. The file must be also in .xls or .xlsx format. You can customize the brain region name to the name you want with the template areas.xlsx. In this file, the left column is the original name of the region 
(do not change) and in the right column is the user-defined name. Use for this the Names correction file input.

### Functions

You can filter the columns and information as you wish. To restart the Shiny App use the Restart-Button. After the data has been loaded, the following functions are available: Quality Control with Raincloud Plots, Descriptive Statistics with the Brain Map, Linear and Lasso Regression. 