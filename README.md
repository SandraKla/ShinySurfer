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