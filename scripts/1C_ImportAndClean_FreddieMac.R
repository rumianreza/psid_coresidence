# Packages
library(openxlsx); library(dplyr); library(tidyr); library(lubridate)
###############################################################################
### WORKING DIRECTORY #########################################################
cd <- file.path ("C:", "Users", "Owner", "Dropbox",
                 "psid_coresidence", "scripts", fsep = "/")
setwd(cd)
###############################################################################
###############################################################################
### R SCRIPT AUTHOR: Rumian Reza // rumian.reza@unt.edu

### PURPOSE: Import Freddie Mac Housing Price Index (master file),
# calculate annual price index for each state, export results.

### DATE CREATED: Nov 13 2021

### NOTES: 
### CHANGES:

# Dec 31 2021
###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)
input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox",
                        "psid_coresidence", "data", fsep = "/")

fm1 <- read.csv(paste(input_dir1, "fmhpi_master_file.csv", sep = "/")) 

###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)

###############################################################################
###############################################################################

fm1 |>
        select(-Month, -GEO_Code, -Index_NSA) |> 
        filter(GEO_Type == "State") |>
        group_by(Year, GEO_Name) |>
        mutate(fmhpi = mean(Index_SA)) |>
        select(-Index_SA, -GEO_Type) |>
        rename(state = GEO_Name) |>
        unique() -> fm2

write.csv(fm2, paste(input_dir1, "annual_fmhpi.csv", sep = "/"), row.names = F)
