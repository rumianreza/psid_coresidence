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

### PURPOSE: Create annual seasonally adjusted state level unemployment
# rates.

### DATE CREATED: 11/13/2021

### NOTES: 

# Data source: Systematic searches on FRED's database. (FRED
# collected data from BLS).

### CHANGES:

# Dec 31 2021
###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)
input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                        "data", "FRED", fsep = "/")


ur1 <- read.csv(paste(input_dir1, "UR1.csv", sep = "/")) %>% select(-FLUR.1)
ur2 <- read.csv(paste(input_dir1, "UR2.csv", sep = "/")) 
ur3 <- read.csv(paste(input_dir1, "UR3.csv", sep = "/")) %>% select(-CAUR)
ur4 <- read.csv(paste(input_dir1, "UR4.csv", sep = "/"))
ur5 <- read.csv(paste(input_dir1, "UR5.csv", sep = "/"))


###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)

input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox", 
                        "psid_coresidence", "data", fsep = "/")
csv_out <- paste(input_dir1, "annual_state_UR.csv", sep = "/")
###############################################################################
###############################################################################

ur <- list(ur1, ur2, ur3, ur4, ur5)
df1 <- data.frame()
for(i in 1:length(ur)){
    dft <- ur[[i]] %>% mutate(year = year(ymd(DATE))) %>% select(-DATE)
    dft <-  pivot_longer(dft, cols = ends_with("UR"), names_to = "state")
    dft <- dft %>% group_by(state, year) %>% mutate(state_ur = mean(value))
    ur[[i]] <- dft %>% select(-value) %>% unique()
    df1 <- rbind(df1, ur[[i]])
}

df1 |> 
    arrange(state, year) |>
    separate(state, into = c("state", "_"), sep = "UR") |>
    select(-"_") -> df2
head(df2)

write.csv(df2, csv_out, row.names = F)
