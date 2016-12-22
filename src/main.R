
# Run this script to re-do the analysis, beginning to end.
#  Be sure to set the working directory to the root
#  directory of this project before running.

# Loading the libraries in order here.  In particular,
#   issues can arise if you load plyr after dplyr.

library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(reshape2)
library(zoo)
library(tools)
library(ggplot2)
library(cowplot)
library(broom)
library(lubridate)

# Parse the .HSP files, and create the 15 minute (and monthly) .csv files
source("src/parseHSPdir.R")

# Parse the .DAT files.  This will overwrite some of the 15 minute data files.
#   Specifically, PDE1 and PDE2, where there is more information in the .DAT
#   files than in the .HSP files.
source("src/dat-to-csv.R")

# Create filled .csv files, where filled means gaps are converted to zeros.
#   Also use the annotations.csv file to add flags to the filled .csv files.
source("src/create-filled-timeseries.R")

# Create daily .csf files by aggregating (sum rainfall by day)
source("src/create-daily-timeseries.R")

# Compare the daily values to NOAA daily values, and create the
#   questionablevals.csv file of outliers (high Cook's number).

# Also the flags.to.use variable controls which data are included
#   in the analysis.  The default, "Normal", excludes everything that
#   isn't "Normal" flagged.  Adding "UNK" to the list includes all
#   the values flagged as a result of the questionableVals.csv file.
flags.to.use <- c("Normal")
source("src/compare-with-NOAA.R")

# Graph out the length of data record plus data issues for each station
source("src/graph-timeseries-datespans.R")
