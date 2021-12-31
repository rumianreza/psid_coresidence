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

### PURPOSE: 

#Drop observations for which there are outliers.
# Impose additional restrictions.
# ALSO: adjust housing price index and expenditure variables for inflation.

### DATE CREATED: 11/17/2021

### NOTES: 

# I'm identifying outliers and anomalies from the following Stata output file:

# output_summary2-2021-11-17-edit.docx 


### CHANGES:

# Dec 31 2021

###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)
input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox",
                        "psid_coresidence", "data", fsep = "/")
df1 <- read.csv(paste(input_dir1, "analysis_sample_5655_v1.csv", sep = "/"))
fr1 <- read.csv(paste(input_dir1, "FRED", "CPIAUCSL.csv", sep = "/"))
###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)

csv_out <- paste(input_dir1, "analysis_sample_5655_v2.csv", sep = "/")

###############################################################################
###############################################################################

# Count the number of outliers / anomalies.
length(unique((df1 %>% filter(g1_child_exp > 8000))$g1_id))
length(unique((df1 %>% filter(g0_max_age == 0))$g1_id))
length(unique((df1 %>% filter(g1_transfer_inc > 40000))$g1_id))
length(unique((df1 %>% filter(g1_age >= 70))$g1_id))
length(unique((df1 %>% filter(g1_educ < 5))$g1_id))
length(unique((df1 %>% filter(g0_d_educ == 0))$g1_id))
length(unique((df1 %>% filter(g0_m_educ < 5))$g1_id))



hist(df1$g1_transfer_inc)

mean(df1$g1_transfer_inc) + 5*sd(df1$g1_transfer_inc)
mean(df1$g1_age) + 3*sd(df1$g1_age)

# Additional sample restrictions.
df2 <- df1 %>% filter(g1_age >= 18,
                      g1_child_exp >= 0,
                      g1_child_exp <= 8000,
                      g0_max_age != 0,
                      g1_educ >= 5,
                      g0_transfer_inc <= 40000)

# Count the number of waves each cross-sectional unit appears in.
df3 <- (df2
        %>% group_by(g1_id)
        %>% mutate(T = row_number())
        %>% mutate(num_waves = max(T))
        %>% ungroup(g1_id))

# Number of unique individuals
length(unique(df2$g1_id))

# Frequency table: number of waves that individuals appear in
table(df3$num_waves)

### INFLATION ADJUSTMENTS
names(fr1) <- c("date", "CPI")
fr2 <- fr1 %>% mutate(year = year(ymd(date)), .keep = "unused")

df4 <- left_join(df3, fr2, by = c("wave" = "year"))

df5 <- df4 %>% mutate(g0_transfer_inc = 100*g0_transfer_inc/CPI,
                      g1_child_exp = 100*g1_child_exp/CPI,
                      fmhpi = 100*fmhpi/CPI)


### HOW MANY OF THOSE G1 WHO ARE CORESIDING WITH G0 ARE RATHER YOUNG?
#length(unique((df5 %>% filter(g1_age > 22))$g1_id))
#table(df5$coreside, df5$wave)

#dff <- df5 %>% filter(g1_age >22)
#table(dff$coreside, dff$wave)

#View(df5 %>% filter(g0_d_educ == 0))

write.csv(df5, csv_out, row.names = F)
