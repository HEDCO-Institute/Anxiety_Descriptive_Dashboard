# Anxiety Prevention Descriptive Dashboard

**Date of Release:** 11/26/2025  
**Title:** Findings From School-Based Anxiety Prevention Research<br>
**Repo Authors:** Shaina Trevino & Aden Bhagwat


## **🔹 Overview**

This repository contains the code and data to produce our interactive [Anxiety Prevention Descriptive Dashboard](https://hedco-institute.shinyapps.io/anxiety_descriptive_dashboard/). 

This repository follows **[AEA Data and Code Availability Standards](https://datacodestandard.org/)** and includes:
- Datasets used to generate reported results.
- R code necessary to reproduce quantitative results reported.
- Computational environment details to ensure reproducibility.


## **🔹 Data and Code Availability Statement**
### **Data Sources**
The data used in this dashboard include study characteristics and effect size information from our larger [living systematic review on school-based anxiety prevention](https://github.com/HEDCO-Institute/Anxiety_Prevention_Overview).
- Datasets from the living review used for this dashboard reflect a fixed version of the data, captured during deployment. 

The following datasets used for the dashboard are available in the `data` subfolder:


| Data File | Description | Data Structure |
|-----------|-------------|-----------| 
| `app_data.csv` | Cleaned data exported to create the dashboard | One row per eligible study | 
| `APO_study_level.xlsx` | Extracted descriptive data for eligible primary studies | One row per eligible primary study |
| `Anxiety_diagnosis.xlsx` | Extracted effect size data for eligible primary studies reporting anxiety diagnosis | One row per effect size (outcome + group + timepoint)|
| `Anxiety_Symptoms.xlsx` | Extracted effect size data for eligible primary studies reporting anxiety symptoms | One row per effect size (outcome + group + timepoint)|
| `Depression_Symptoms.xlsx` | Extracted effect size data for eligible primary studies reporting depression symptoms | One row per effect size (outcome + group + timepoint)|
| `Educational_achievement.xlsx` | Extracted effect size data for eligible primary studies reporting educational achievement | One row per effect size (outcome + group + timepoint)|
| `Suidical_ideation.xlsx` | Extracted effect size data for eligible primary studies reporting suicidal ideation | One row per effect size (outcome + group + timepoint)|
| `Well_being.xlsx` | Extracted effect size data for eligible primary studies reporting well-being | One row per effect size (outcome + group + timepoint)|
<br>

### **Code**
The scripts used to generate this dashboard are in the main root folder:
- `app.R` contains all the code to load the app data and generate the dashboard
- `data_cleaning.R` contains code to load raw data, clean, and export the `app_data.csv` file for the dashboard


### **Handling of Missing Data**
- Missing values in the datasets are coded as `-999`, `Not Reported`, or `NA`, indicating that those values were not reported in studies/reviews.

## **🔹 Instructions for Replication**

### **Data Preparation and Analysis**
To replicate our results: 

**If you have Rstudio and Git installed and connected to your GitHub account:**

1. Clone the [repository](https://github.com/HEDCO-Institute/Anxiety_Descriptive_Dashboard) to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Open the `Anxiety_Descriptive_Dashboard.Rproj` R project in R Studio (this should automatically activate the `renv` environment)
1. Open and run the `app.R` script 

**If you need to install or connect R, Rstudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install Git](https://happygitwithr.com/install-git.html)
1. [Link Git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in Rstudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:** 

1. Download the ZIP file from the [repository](https://github.com/HEDCO-Institute/Anxiety_Descriptive_Dashboard)
1. Extract all files to your local machine
1. Open the `Anxiety_Descriptive_Dashboard.Rproj` R project in R Studio (this will automatically set the working directory and activate the `renv` environment)
1. Open and run the `app.R` script 


## **🔹 Computational Requirements**
### **Software Environment**
- **R Version:** 4.2.2  
- **Operating System:** Windows 10 Enterprise (x86_64-w64-mingw32/x64)  

### **Reproducing the Environment**
Opening the `Anxiety_Descriptive_Dashboard` R project will automatically install the correct package versions and set up the environment using the `renv` package. To manually load the environment:

1. Install `renv` (if not already installed):
```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
```

2. Restore any missing packages:
```r
renv::restore()
```

3. If needed, load the environment:
```r
renv::load()
```

Once the environment is restored, run the script starting with loading the necessary packages:
```r
pacman::p_load(tidyverse, rio, here, janitor, xml2, metafor, DT, shiny, plotly, openxlsx,
               countrycode, forestplot, reactable, htmltools, stringi, shinyWidgets, shinyjs, readr)

```


## **🔹 Licensing**
The code and data in this replication package are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0); see the LICENSE file in the main root directory for full terms



## **🔹 Contact Information**
For questions about this replication package, contact:  
✉️ **Shaina Trevino** (strevino@uoregon.edu)  

