# Software tool for the Analysis and Visualization of MRI images of the brain

[![License](https://img.shields.io/github/license/SandraKla/MRI_Analysis.svg)]()

<img src="www/example.png" align="center"/>

For more information use the [Homepage](https://sandrakla.github.io/ShinySurfer_Homepage/).

*** New ***: 
- Old data format is when you load the OASIS.xlsx data in Upload data and ignore the first two file inputs + click on the CheckBox (preprocessed) (without any bugs :)) 
- Now you can directly add your data from Freesurfer for left and right hemisphere as .txt
and add a .csv file with more data about the patients (like age)
- this is equal to a2009s_thickness_lh.txt, a2009s_thickness_rh.txt and demographics.csv
- Freesurfer must be loaded in the first two file inputs panels and more data under Upload data -> this is in production

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

Under Start > Upload data > Load the dataset OASIS.xlsx as an example dataset
