CAPSTONE DETAILS:

Dissertation Title: The Impacts of Financial Crisis Bailouts on Inward Foreign Direct Investment

Student Name: Laeg T. Winder

CODE DETAILS:
Software: R version 4.5.1

Packages: readxl, dplyr, tidyr, stringr, janitor, openxlsx, ggplot2, Synth, knitr, kableExtra, modelsummary, zoo

Note: The code produces a number of tables and figures so note the core replication files are (MasterFinal, FinalData and README) along with the Raw Data folder)

HOW TO RUN:

Open "MasterFinal" in R.

In "DIRECTORY & SHARED CONSTANTS" Insert the file path for this unzipped folder into the directory variable e.g.,


# ============================================================
# DIRECTORY & SHARED CONSTANTS
# ============================================================

Directory <- "INSERT FILE PATH"


The file path should be formatted in this fashion:
"C:\\User\\...\\Replication Package


I included an install packages line:

# install.packages(c("readxl","dplyr","tidyr","stringr","janitor","openxlsx",
#                    "ggplot2","Synth","knitr","kableExtra","modelsummary","zoo"))

Remove the hashtags if you do not have the needed packages to install.

Then Run.


DATASET CONSTRUCTION DETAILS:

"Controls" was produced by manually transcribing the reported values from the paper Grossman & Woll 2014 into excel. the paper can found in the capstones references section.

"Data5" was constructed with a cleaning code but since I needed to manually add an excel file I kept the cleaning code separate from the main replication file, the cleaning code is included in the "Raw Data" folder and is run in the same fashion as the main code.