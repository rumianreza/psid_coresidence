# Packages
library(openxlsx); library(dplyr); library(tidyr)
###############################################################################
### WORKING DIRECTORY #########################################################
cd <- file.path ("C:", "Users", "Owner", "Dropbox",
                 "psid_coresidence", "scripts", fsep = "/")
setwd(cd)
###############################################################################
###############################################################################
### R SCRIPT AUTHOR: Rumian Reza // rumian.reza@unt.edu

### PURPOSE: 

# (1) Link G1 to G0 using the Retrospective FIMS.
# (2) Create generational variables to be used in the regressions.
# (3) Impose sample restrictions.
# (4) Merge Freddie Mac Housing Price Index, State unemployment rates

# Output will be the analysis sample.

### DATE CREATED: 11/12/2021

### NOTES: 

# There are some mothers who have consistently NA values.
# (e.g. see the variables g0_m_pension, g0_m_rent, g0_m_own)

### CHANGES:

# Dec 31 2021
###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)
input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox",
                        "psid_coresidence", "data", fsep = "/")
psid1 <- read.csv(paste(input_dir1, "psid_data_v1.csv", sep = "/"))
fims1 <- read.xlsx(paste(input_dir1, "fim12001_gid_BA_2_BAL_wide.xlsx",
                         sep = "/"),
                   sheet = 1, startRow = 1, colNames = T,
                   rowNames = F, detectDates = F, skipEmptyRows = F,
                   skipEmptyCols = F, na.strings = "NA",
                   fillMergedCells = F)
fmhpi1 <- read.csv(paste(input_dir1, "annual_fmhpi.csv", sep = "/"))
st_ur1 <- read.csv(paste(input_dir1, "annual_state_UR.csv", sep = "/"))
###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)

csv_out <- paste(input_dir1, "analysis_sample_5655_v1.csv", sep = "/")
###############################################################################
###############################################################################

###############################################################################
##### PRELIMINARY FIMS RESTRICTIONS ###########################################
fims2 <- fims1 %>% mutate(g1_id = ER30001*1000 + ER30002,
                          g0_d_id = ER30001_P_F*1000 + ER30002_P_F,
                          g0_m_id = ER30001_P_M*1000 + ER30002_P_M,
                          .keep = "unused")


# Identify individuals who have adoptive mothers/fathers.
# Omit these individuals from the sample.
a_idx <- (fims2
          %>% filter(!is.na(ER30001_P_AF) | !is.na(ER30001_P_AM))
          %>% select(g1_id))
fims3 <- (fims2
          %>% filter(!(g1_id %in% a_idx$g1_id))
          %>% select(-ends_with("AF"), -ends_with("AM")))

# Retain individuals who have mothers.
fims4 <- fims3 %>% filter(!is.na(g0_m_id))

###############################################################################
##### MERGE PSID WITH FIMS ####################################################

psid_tmp <- psid1
names(psid_tmp) <- paste("g1", names(psid1), sep = "_")
df1 <- left_join(fims4, psid_tmp, by = "g1_id")
psid_tmp <- psid1
names(psid_tmp) <- paste("g0_d", names(psid1), sep = "_")
df1 <- left_join(df1, psid_tmp, by = c("g0_d_id" = "g0_d_id", "g1_wave" = "g0_d_wave"))
psid_tmp <- psid1
names(psid_tmp) <- paste("g0_m", names(psid1), sep = "_")
df1 <- left_join(df1, psid_tmp, by = c("g0_m_id" = "g0_m_id", "g1_wave" = "g0_m_wave"))
df1 <- df1 %>% rename(wave = g1_wave)

# Retain young adults who satisfy the following requirements:
# - at least aged 17 years
# - is a sample member
# - has a PSID (observed) mother who is a sample mother
# - Young adult is "present" in the FU (see sequence number).
# - mother's marital status is observed.
# - Education level is observed.
# - State is observed.
# - number of children in G1's family unit is observed.
# - homeownership status of either G0 parent is observed.

df2 <- df1 %>% filter(g1_sample_or_not %in% 1:3,
                      g0_m_sample_or_not %in% 1:3,
                      g1_seq_num %in% 1:30 | g1_move_out == 1,
                      g1_age > 17,
                      g0_m_id > 0,
                      g1_educ_na == 0,
                      g1_emp_na == 0,
                      g0_m_emp_na == 0,
                      g1_emp_na == 0,
                      g1_kids_FU_na == 0,
                      g1_hh_id_na == 0,
                      g1_state_na == 0,
                      g1_health_na == 0,
                      g1_race_na == 0,
                      g1_child_exp_na == 0,
                      g0_d_transfer_inc_na + g0_m_transfer_inc_na <= 1,
                      g0_d_pension_hd_na + g0_d_pension_sp_na <= 1,
                      g0_d_own_na + g0_m_own_na <= 1)
                  #    g1_splitoff_flag == 1)

length(unique(df2$g1_id))

### Histogram of some variables of interest.
table(df2$g0_m_married, df2$wave)
table(df2$g1_age, df2$wave)
table(df2$g1_pubhealth, df2$wave)
table(df2$g1_emphealth, df2$wave)
table(df2$g1_coll_grad, df2$wave)

table(df2$g0_m_spouse_FU, df2$wave)

# Define some generational variables (including outcome variable).

df3 <- df2 %>% mutate(g0_pubhealth = as.integer(g0_d_pubhealth + g0_m_pubhealth > 0),
                      g0_emphealth = as.integer(g0_d_emphealth + g0_m_emphealth > 0),
                      g0_m_pension = as.integer(g0_m_spouse_FU + g0_m_pension_sp == 2 |
                                                  g0_m_hd_FU + g0_m_pension_hd == 2),
                      g0_d_pension = as.integer(g0_d_spouse_FU + g0_d_pension_sp == 2 |
                                                  g0_d_hd_FU + g0_d_pension_hd == 2),
                      g0_pension_both = as.integer(g0_d_pension + g0_m_pension > 1)
                    #  g0_transfer_inc = if_else(g0_d_hd_FU == 1,
                    #                            g0_d_transfer_inc,
                    #                            if_else(g0_m_hd_FU == 1,
                    #                                    g0_m_transfer_inc,
                    #                                    g1_transfer_inc,
                    #                                    missing = NULL),
                    #                            missing = NULL)
                    )
# Maximum function is quirky; need to group by G1 ID and wave first.
df4 <- (df3
        %>% group_by(g1_id, wave)
        %>% mutate(coreside = as.integer(length(intersect(g1_hh_id, 
                                                          c(g0_d_hh_id,
                                                            g0_m_hh_id))) >0),
                   g0_own = max(g0_d_own, g0_m_own, na.rm = T),
                   g0_rent = max(g0_d_rent, g0_d_rent, na.rm = T),
                   g0_pension = max(g0_d_pension, g0_m_pension, na.rm = T),
                   g0_medicare = max(g0_d_medicare, g0_m_medicare, na.rm = T),
                   g0_max_age = max(g0_d_age, g0_m_age, na.rm = T),
                   g0_transfer_inc= if_else(g0_d_hd_FU + g0_m_hd_FU == 2,
                                            max(g0_d_transfer_inc,
                                                g0_m_transfer_inc),
                                            if_else(g0_d_hd_FU == 1 & g0_m_hd_FU == 0,
                                                    g0_d_transfer_inc,
                                                    if_else(g0_d_hd_FU == 0 & g0_m_hd_FU == 1,
                                                            g0_m_transfer_inc,
                                                            0,
                                                            missing = NULL),
                                                    missing = NULL),
                                            missing = NULL),
                   g0_transfer_inc_nonZero = max(g0_d_transfer_notZero,
                                                 g0_m_transfer_notZero,
                                                 na.rm =T))
        %>% ungroup(g1_id, wave))

# Merge state-level unemployment rates and housing price indices.
df5 <- left_join(df4, fmhpi1, by = c("g1_state" = "state", "wave" = "Year"))
df5 <- left_join(df5, st_ur1, by = c("g1_state" = "state", "wave" = "year"))


##### Write to file.
write.csv(df5, csv_out, row.names = F)
