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

### PURPOSE: Import from PSID Data Center download, then convert
# raw files to be in long format (i.e., rows uniquely identified 
# by ID-year combo.) Clean/define/re-define each annually-related PSID variable.

### DATE CREATED: Nov 10 2021

### NOTES: 

# QUESTION: Do Data Center downloads automatically merge family-level
# data with individual-level data (when data at both levels are present)?

# ANSWER: Yes!

### CHANGES:

# Dec 31 2021
###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)
input_dir1 <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                        "data","PSID_variablenames", fsep = "/")
input_dir2 <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                        "data", "PSID_datacenter", "J299783_VariableList_v1",
                        fsep = "/")
input_dir3 <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                        "data", "PSID_datacenter", "J299788_SummaryVariables_v1",
                        fsep = "/")
state_codes_dir <- file.path("C:", "Users", "Owner", "Dropbox",
                             "psid_coresidence", "data", fsep = "/")

varlist <- read.csv(paste(input_dir1, "VariableList_v1.csv", sep = "/"))

vars1 <- read.xlsx(paste(input_dir2, "J299783.xlsx", sep = "/"),
                   sheet = 1, startRow = 1, colNames = T,
                   rowNames = F, detectDates = F, skipEmptyRows = F,
                   skipEmptyCols = F, na.strings = "NA",
                   fillMergedCells = F)

summa1 <- read.xlsx(paste(input_dir3, "J299788.xlsx", sep = "/"),
                    sheet = 1, startRow = 1, colNames = T,
                    rowNames = F, detectDates = F, skipEmptyRows = F,
                    skipEmptyCols = F, na.strings = "NA",
                    fillMergedCells = F)

###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)
output_dir1 <- file.path("C:", "Users", "Owner", "Dropbox",
                         "psid_coresidence", "data",fsep = "/")

###############################################################################
##### INITIALIZATION ##########################################################

dfnames <- data.frame(names(vars1))
names(dfnames) <- "names"

# Identify imported PSID variables that weren't in the variable list.
non_list <- anti_join(dfnames, varlist, by = "names")
non_list

# ER21001, ER25001, ER36001, ER42001, and ER47301 are release numbers
# for the family-level data. ER30000 is the release number for the
# individual-level data.

vars1 |>
  mutate(id = ER30001*1000 + ER30002, .keep = "unused") |>
  select(-c(ER21001, ER25001, ER36001, ER42001,
                        ER47301, ER30000)) -> vars2

# Initialize data frame to be merged with cleaned variables.
df1 <- data.frame(vars2$id) |> rename(id = vars2.id)

###############################################################################
###############################################################################
###############################################################################
################## INDIVIDUAL-LEVEL VARIABLES #################################
###############################################################################
###############################################################################
###############################################################################

###############################################################################
###### AGE ####################################################################

varlist |>
  filter(vars == "Age") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2
varlist |>
  filter(vars == "Age") -> a
vars2 |> 
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)

# How many values should we consider?
# table(dft2$value)

dft2 |>
  mutate(age = if_else(value < 120, value, 0, missing = NULL)) |>
  select(-value) -> dft3

df1 <- left_join(df1, dft3, by = "id")

###############################################################################
###### EDUCATION VARIABLE(S) ##################################################

a <- varlist |> filter(vars == "Education")
dft1 <- vars2 |> select(id, a$names)
names(dft1) <- c("id", a$waves)
dft2 <- dft1 |> pivot_longer(cols = starts_with("2"),
                            names_to = "wave")

# How many values should we consider?
table(dft2$value)

# Definitions (all waves):

# = 1-11 level of grade school completed
# = 12 high school graduate
# = 13-15 some college completed
# = 16 college graduate (4 years of college)
# = 17 schooling beyond 4 years of higher education

dft2 |>
  mutate(educ_na = as.integer(is.na(value)),
         hs_grad = as.integer(value >= 12),
         some_coll = as.integer(value >= 13 & value <= 15),
         coll_grad = as.integer(value >= 16),
         grad_school = as.integer(value == 17),
         educ = if_else(value <= 17, value, 0,NULL)) |>
  select(-value) -> dft3

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### EMPLOYMENT VARIABLE(S) #################################################

varlist |>
  filter(vars == "EmploymentStatus") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

# How many values should we consider?
table(dft2$value)

# Definitions (all waves):
# = 0 unobserved, under 16, etc.
# = 1 working
# = 2 temporarily laid off
# = 3 Looking for work, unemployed
# = 4 Retired
# = 5 permanently disabled
# = 6 "housewife"; keeping house
# = 7 Student
# = 8 Other
# = 9 NA; DK

dft2 |>
  mutate(emp_na = as.integer(is.na(value)),
         employed = as.integer(value == 1),
         laid_off = as.integer(value == 2),
         job_search = as.integer(value == 3),
         retired = as.integer(value == 4),
         disabled = as.integer(value == 5),
         student = as.integer(value == 7)) |>
  select(-value) -> dft3

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### MARITAL STATUS #########################################################
varlist |>
  filter(vars == "MarriedPairsIndicator") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)

dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

# How many values should we consider?
table(dft2$value, dft2$wave)

# Definitions:
# = 1 if individual is in a pair recognized as married head/wife, 
#     head/cohabitor, or married head/husband of head.

# = 2 if individual is in a pair recognized as married/cohabiting but secondary
#   in their respective FU (e.g., married couple but not involving the head)

# = 3 if individual is in a pair recognized as married/cohabiting but tertiary
#   in their respective FU

# = 4 if individual is in a pair recgonized as married/cohabiting
#   but quaternary in their respective FU

dft2 |>
  mutate(married_na = as.integer(is.na(value)),
         married = as.integer(value > 0)) |>
  select(-value) -> dft3

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### RELATIONSHIP TO HEAD ###################################################
varlist |>
  filter(vars == "RelationshipToHead") -> a
vars2 |> 
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

# How many values should we consider?
table(dft2$value)

# Definitions:
# = 10 Head
# = 20 Legal Wife of Head
# = 90 Legal Husband of Head
# = 22 Female cohabitor who has lived with Head for 12 months or more
# = 88 First-year cohabitor of Head
# = 98 Other nonrelatives (including homosexual partners, etc.)

# = 30 Son or Daughter of Head (including adopted but not stepchildren)
# = 33 Stepson or Stepdaughter of Head (children of Wife but not Head)
# = 35 Son or Daughter of Female cohabitor (but not Head)
# = 37 Son-in-law or Daughter-in-law of Head (including stepchild-in-law)
# = 38 Foster son or Foster daughter of Head, NOT legally adopted

# = 40 Brother or Sister of Head (including step/half sisters/brothers)
# = 47 Brother-in-law/Sister-in-law of Head
# = 48 Brother/sister of Head's Cohabitor

# = 50 Father/mother of Head (including stepparents)
# = 57 Father-in-law/Mother-in-law of Head (including parents of wives)
# = 58 Father/Mother of Head's Cohabitor
# = 60 Grandson/Granddaughter of Head (including those of the wife)

# ... there's a lot more......

dft2 |>
  mutate(hd_FU = as.integer(value == 10),
         spouse_FU = as.integer(value %in% c(20, 90)),
         hd_spouse_FU = as.integer(value %in% c(10,20,90)),
         cohab_FU = as.integer(value %in% c(20,90,22,88)),
         child_hd_FU = as.integer(value %in% c(30,33,37,38)),
         parent_hd_FU = as.integer(value %in% c(50,57,58))) |>
  select(-value) -> dft3

### NOTE: This temporary data frame (dft3) will be used
# below in the variables pertaining to RACE. Therefore, I 
# preserve this data frame in the next line of code.
df_rel_to_head <- dft3

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### SEQUENCE NUMBER ########################################################

varlist |>
  filter(vars == "SequenceNumber") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

dft2 |>
  mutate(move_out = as.integer(value %in% 71:80),
         absent = as.integer(value %in% 51:59)) |>
  rename(seq_num = value) -> dft3

df1 <- left_join(df1, dft3, by = c("id","wave"))

###############################################################################
###### SUMMARY VARIABLES ########################################################

summa1 |>
  mutate(id= ER30001*1000 + ER30002, .keep = "unused") |>
  select(-ER30000) |>
  mutate(female = as.integer(ER32000 == 2),
         mom_id = as.integer(ER32009*1000 + ER32010),
         dad_id = as.integer(ER32016*1000 + ER32017)) |>
  rename(sample_or_not = ER32006) |>
  select(-starts_with("ER")) -> summa2

df1 <- left_join(df1, summa2, by = "id")



###############################################################################
###############################################################################
###############################################################################
################## FAMILY-LEVEL VARIABLES #####################################
###############################################################################
###############################################################################
###############################################################################

###############################################################################
###### CHILDCARE EXPENDITURE ##################################################

varlist |>
  filter(vars == "ChildcareExpenditures") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

dft2 |>
  mutate(child_exp_na = as.integer(is.na(value))) |>
  rename(child_exp = value) |>
  mutate(pos_child_exp = as.integer(child_exp > 0)) -> dft3

table(dft3$pos_child_exp, dft3$wave)

df1 <- left_join(df1, dft3, by = c("id", "wave"))


###############################################################################
###### HOME OWNERSHIP #########################################################

varlist |>
  filter(vars == "OwnOrRent") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

# How many values should we consider?
table(dft2$value, dft2$wave)

# Definitions:
# = 1 FU owns current residence (including mobile homeowners who rent lots)
# = 5 FU pays rents for current residence
# = 8 FU neither owns nor rents the current residence
# = 9 Wild code (?)

dft2 |>
  mutate(own_na = as.integer(is.na(value)),
         own = as.integer(value == 1),
         rent = as.integer(value == 5)) |>
  select(-value) -> dft3

# Contingency tables
table(dft3$own,dft3$rent, dft3$wave)

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### HOUSEHOLD ID ###########################################################

varlist |>
  filter(vars == "HouseholdID") -> a
vars2 |>
  select(id, a$names) -> dft1
names(dft1) <- c("id", a$waves)
dft1 |>
  pivot_longer(cols = starts_with("2"), names_to = "wave") -> dft2

dft2 |> 
  mutate(hh_id_na = as.integer(is.na(value))) |>
  rename(hh_id = value) -> dft3

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### HEALTH INSURANCE #######################################################

a <- varlist |> filter(vars == "HealthInsurance_Men1")
b <- varlist |> filter(vars == "HealthInsurance_Men2")
c <- varlist |> filter(vars == "HealthInsurance_Men3")
dft1_a <- vars2 |> select(id, a$names)
dft1_b <- vars2 |> select(id, b$names)
dft1_c <- vars2 |> select(id, c$names)
names(dft1_a) <- c("id", a$waves)
names(dft1_b) <- c("id", b$waves)
names(dft1_c) <- c("id", c$waves)

dft2_a <- dft1_a |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")
dft2_b <- dft1_b |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")
dft2_c <- dft1_c |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# How many values should we consider?
table(dft2_a$value, dft2_a$wave)
table(dft2_b$value, dft2_b$wave)
table(dft2_c$value, dft2_c$wave)


# Huge spike in Medicaid after 2007! (though NOT contemporaneous)
# Meanwhile, state-sponsored health plans plummet after 2007.
# This could indicate people switching away from state-sponsored
# health plans and to Medicaid. WHY? (is Great Recession to blame?)


# Definitions (all waves) (ALL RE: PREVIOUS TWO YEARS);
# = 1 Employer provided health insurance
# = 2 Private health insurance purchased directly
# = 3 Medicare
# = 4 Medi-Gap
# = 5 Medicaid/Medical Assistance/Medi-Cal
# = 6 Military health care/VA
# = 7 CHAMPUS/TRICARE/CHAMP-VA
# = 8 Indian Health Insurance
# = 9 State-sponsored health plan
# = 10 Other govt program
# = 97 Other
# = 98 DK
# = 99 NA/refused
# = 0 not covered by health plan, etc. (see codebook...)

dft3_a <- (dft2_a
         |> mutate(health_men1_na = as.integer(is.na(value)),
                    prihealth_men1 = as.integer(value %in% c(1,2)),
                    pubhealth_men1 = as.integer(value %in% 3:10),
                    emphealth_men1 = as.integer(value == 1),
                    medicare_men1 = as.integer(value == 3),
                    medicaid_men1 = as.integer(value == 5))
         |> select(-value)) 
dft3_b <- (dft2_b
           |> mutate(health_men2_na = as.integer(is.na(value)),
                      prihealth_men2 = as.integer(value %in% c(1,2)),
                      pubhealth_men2 = as.integer(value %in% 3:10),
                      emphealth_men2 = as.integer(value == 1),
                      medicare_men2 = as.integer(value == 3),
                      medicaid_men2 = as.integer(value == 5))
           |> select(-value))
dft3_c <- (dft2_c
           |> mutate(health_men3_na = as.integer(is.na(value)),
                      prihealth_men3 = as.integer(value %in% c(1,2)),
                      pubhealth_men3 = as.integer(value %in% 3:10),
                      emphealth_men3 = as.integer(value == 1),
                      medicare_men3 = as.integer(value == 3),
                      medicaid_men3 = as.integer(value == 5))
           |> select(-value))

dft3 <- left_join(left_join(dft3_a, dft3_b, by = c("id", "wave")),
                  dft3_c,
                  by = c("id", "wave"))

dft4 <- (dft3 
         |> mutate(health_na = as.integer((health_men1_na +
                                              health_men2_na +
                                              health_men3_na) == 3),
                    prihealth = as.integer((prihealth_men1 +
                                                   prihealth_men2 +
                                                   prihealth_men3) > 0),
                         pubhealth = as.integer((pubhealth_men1 +
                                                   pubhealth_men2 +
                                                   pubhealth_men3) > 0),
                         emphealth = as.integer((emphealth_men1 +
                                                   emphealth_men2 +
                                                   emphealth_men3) > 0),
                         medicare = as.integer((medicare_men1 +
                                                   medicare_men2 +
                                                   medicare_men3) > 0),
                         medicaid = as.integer((medicaid_men1 +
                                                  medicaid_men2 +
                                                  medicaid_men3) > 0))
         |> select(-ends_with(c("men1", "men2","men3")))
         )

table(dft4$prihealth, dft4$wave)
table(dft4$pubhealth, dft4$wave)
table(dft4$emphealth, dft4$wave)
table(dft4$medicare, dft4$wave)
table(dft4$medicaid, dft4$wave)

# Big increase in Medicaid-like enrollments after 2007...

df1 <- left_join(df1, dft4, by = c("id", "wave"))


###############################################################################
###### NUMBER OF CHILDREN IN FU ###############################################

a <- varlist |> filter(vars == "NumberOfChildrenInFU")
dft1 <- vars2 |> select(id, a$names)
names(dft1) <- c("id", a$waves)

dft2 <- dft1 |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# How many values should we consider?
table(dft2$value, dft2$wave)

dft3 <- (dft2 |> mutate(kids_FU_na = as.integer(is.na(value)),
                         kid1_FU = as.integer(value == 1),
                        kid2_FU = as.integer(value == 2),
                        kid3_FU = as.integer(value == 3),
                        kid4_FU = as.integer(value >= 4),
                        kids_FU = as.integer(value))
         |> select(-value))

df1 <- left_join(df1, dft3, by = c("id", "wave"))


###############################################################################
###### PENSION ################################################################

a <- varlist |> filter(vars == "WhetherPension_Head")
b <- varlist |> filter(vars == "WhetherPension_Spouse")

dft1_a <- vars2 |> select(id, a$names)
dft1_b <- vars2 |> select(id, b$names)

names(dft1_a) <- c("id", a$waves)
names(dft1_b) <- c("id", b$waves)


dft2_a <- dft1_a |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")
dft2_b <- dft1_b |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# What values need to be accounted for?
table(dft2_a$value, dft2_a$wave)
table(dft2_b$value, dft2_b$wave)

# Definitions (Question: is head/wife of head of FU covered by 
#   employer-provided pension or retirement plan?):
# = 1 Yes
# = 5 No
# = 8 DK
# = 9 NA/refused
# = 0

dft3_a <- (dft2_a
           |> mutate(pension_hd_na = as.integer(is.na(value)),
                      pension_hd = as.integer(value == 1))
           |> select(-value))
dft3_b <- (dft2_b
           |> mutate(pension_sp_na = as.integer(is.na(value)),
                      pension_sp = as.integer(value == 1))
           |> select(-value))
dft3 <- left_join(dft3_a, dft3_b, by = c("id", "wave"))

dft4 <- dft3 |> mutate(pension_hd_sp = as.integer((pension_hd +
                                                      pension_sp) == 2))

table(dft4$pension_hd, dft4$wave)
table(dft4$pension_sp, dft4$wave)
table(dft4$pension_hd_sp, dft4$wave)

df1 <- left_join(df1, dft4, by = c("id", "wave"))

###############################################################################
###### RACE ###################################################################

# A word from PSID Q/A about measuring the Race variable:
# https://www.psc.isr.umich.edu/dis/data/kb/answer/1025.html
# "If you are interested in just heads and spouses, 
# find the race variable in the family level variables.
# If you are interested in other persons you will have 
# to make some assumptions. You can assign a person the race 
# they identify with once. For instance, a person who is a child
# in 1988 (with no race), will have a race when they are a family
# head.

a <- varlist |> filter(vars == "RaceOfHead_Men1")
b <- varlist |> filter(vars == "RaceOfHead_Men2")
c <- varlist |> filter(vars == "RaceOfHead_Men3")
d <- varlist |> filter(vars == "RaceOfSpouse_Men1")
e <- varlist |> filter(vars == "RaceOfSpouse_Men2")
f <- varlist |> filter(vars == "RaceOfSpouse_Men3")

dft1_a <- vars2 |> select(id, a$names)
dft1_b <- vars2 |> select(id, b$names)
dft1_c <- vars2 |> select(id, c$names)
dft1_d <- vars2 |> select(id, d$names)
dft1_e <- vars2 |> select(id, e$names)
dft1_f <- vars2 |> select(id, f$names)

names(dft1_a) <- c("id", a$waves)
names(dft1_b) <- c("id", b$waves)
names(dft1_c) <- c("id", c$waves)
names(dft1_d) <- c("id", d$waves)
names(dft1_e) <- c("id", e$waves)
names(dft1_f) <- c("id", f$waves)

dft2_a <- dft1_a |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")
dft2_b <- dft1_b |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")
dft2_c <- dft1_c |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")
dft2_d <- dft1_d |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")
dft2_e <- dft1_e |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")
dft2_f <- dft1_f |> pivot_longer(cols = starts_with("2"),
                                  names_to = "wave")

# How many values should we consider?
table(dft2_a$value, dft2_a$wave)
table(dft2_b$value, dft2_b$wave)
table(dft2_c$value, dft2_c$wave)
table(dft2_d$value, dft2_d$wave)
table(dft2_e$value, dft2_e$wave)
table(dft2_f$value, dft2_f$wave)

# Definitions (abbreviated!)
# = 1 White
# = 2 Black
# = 3 American Indian or Alaska Native
# = 4 Asian
# = 5 Native Hawaiian / Pacific Islander
# = 7 Other

dft3_a <- (dft2_a
           |> mutate(white_men1 = as.integer(value == 1),
                      black_men1 = as.integer(value == 2),
                      other_men1 = as.integer(value > 2))
           |> select(-value))
dft3_b <- (dft2_b
           |> mutate(white_men2 = as.integer(value == 1),
                      black_men2 = as.integer(value == 2),
                      other_men2 = as.integer(value > 2))
           |> select(-value))
dft3_c <- (dft2_c
           |> mutate(white_men3 = as.integer(value == 1),
                      black_men3 = as.integer(value == 2),
                      other_men3 = as.integer(value > 2))
           |> select(-value))

dft4_1 <- (left_join(left_join(dft3_a, dft3_b, by = c("id", "wave")),
                   dft3_c, by = c("id","wave"))
           |> mutate(white_hd = if_else(white_men1 != 0,
                                         white_men1,
                                         if_else(white_men2 != 0,
                                                 white_men2,
                                                 white_men3,
                                                 missing = NULL),
                                         missing = NULL), 
                      black_hd = if_else(black_men1 != 0,
                                         black_men1,
                                         if_else(black_men2 != 0,
                                                 black_men2,
                                                 black_men3,
                                                 missing = NULL),
                                         missing = NULL),
                      other_hd = if_else(other_men1 != 0,
                                         other_men1,
                                         if_else(other_men2 != 0,
                                                 other_men2,
                                                 other_men3,
                                                 missing = NULL),
                                         missing = NULL))
           |> select(-ends_with(c("men1", "men2", "men3")))
           )

# Some are both white and black.
table(dft4_1$white_hd, dft4_1$black_hd)

dft3_d <- (dft2_d
           |> mutate(white_men1 = as.integer(value == 1),
                      black_men1 = as.integer(value == 2),
                      other_men1 = as.integer(value > 2))
           |> select(-value))
dft3_e <- (dft2_e
           |> mutate(white_men2 = as.integer(value == 1),
                      black_men2 = as.integer(value == 2),
                      other_men2 = as.integer(value > 2))
           |> select(-value))
dft3_f <- (dft2_f
           |> mutate(white_men3 = as.integer(value == 1),
                      black_men3 = as.integer(value == 2),
                      other_men3 = as.integer(value > 2))
           |> select(-value))

dft4_2 <- (left_join(left_join(dft3_d, dft3_e, by = c("id", "wave")),
                     dft3_f, by = c("id","wave"))
           |> mutate(white_sp = if_else(white_men1 != 0,
                                         white_men1,
                                         if_else(white_men2 != 0,
                                                 white_men2,
                                                 white_men3,
                                                 missing = NULL),
                                         missing = NULL), 
                      black_sp = if_else(black_men1 != 0,
                                         black_men1,
                                         if_else(black_men2 != 0,
                                                 black_men2,
                                                 black_men3,
                                                 missing = NULL),
                                         missing = NULL),
                      other_sp = if_else(other_men1 != 0,
                                         other_men1,
                                         if_else(other_men2 != 0,
                                                 other_men2,
                                                 other_men3,
                                                 missing = NULL),
                                         missing = NULL))
           |> select(-ends_with(c("men1", "men2", "men3")))
           )

# Some are both white and black. (though less than husbands)
table(dft4_2$white_sp, dft4_2$black_sp)

dft4 <- left_join(dft4_1, dft4_2, by = c("id", "wave"))

### Identify Heads/Spouses of Heads by using "Relationship to Head" variable.
### NOTE: First, run the code chunk under RELATIONSHIP TO HEAD (omitting
### the concluding left_join statement.)
### That is, I record those individuals whose races are directly measured.
### A lot of copy-and-paste!

head(df_rel_to_head)

dft5 <- left_join(dft4, df_rel_to_head, by = c("id", "wave"))
dft6 <- (dft5
         |> mutate(is_white_hd = if_else(white_hd == 1 & hd_FU == 1,
                                          1, 0, missing = NULL),
                    is_black_hd = if_else(black_hd == 1 & hd_FU == 1,
                                          1, 0, missing = NULL),
                    is_other_hd = if_else(other_hd == 1 & hd_FU == 1,
                                          1, 0, missing = NULL),
                    is_white_sp = if_else(white_sp == 1 & spouse_FU == 1,
                                          1, 0, missing = NULL),
                    is_black_sp = if_else(black_sp == 1 & spouse_FU == 1,
                                          1, 0, missing = NULL),
                    is_other_sp = if_else(other_sp == 1 & spouse_FU == 1,
                                          1, 0, missing = NULL))
         )

# Convert is_white_hd, is_black_hd, ..., is_other_sp into
# time-invariant variables. (It is possible for race to be
# observed in some years but not others.)
dft7 <- (dft6
           |> group_by(id)
           |> mutate(is_white_hd = as.integer(1 %in% is_white_hd),
                      is_black_hd = as.integer(1 %in% is_black_hd),
                      is_other_hd = as.integer(1 %in% is_other_hd),
                      is_white_sp = as.integer(1 %in% is_white_sp),
                      is_black_sp = as.integer(1 %in% is_black_sp),
                      is_other_sp = as.integer(1 %in% is_other_sp))
           |> ungroup(id))

dft8 <- dft7 |> mutate(r_white = as.integer(is_white_hd + is_white_sp > 0),
                        r_black = as.integer(is_black_hd + is_black_sp > 0),
                        r_other = as.integer(is_other_hd + is_other_sp > 0))

### Flag individuals whose race will need to be imputed.
# Partition the temp data frame based on this flag.
### I import FIMS (balanced map) to impute race
### based on biological parents' race.

dft9 <- dft8 |> mutate(r_imp_flag = as.integer(r_white + r_black +
                                                r_other == 0))
dft9_a <- (dft9 |> filter(r_imp_flag == 1)
           |> select(-r_white, -r_black, -r_other))
dft9_b <- dft9 |> filter(r_imp_flag == 0)

fims1 <- read.xlsx(paste(state_codes_dir, "fim12001_gid_BA_2_BAL_wide.xlsx",
                         sep = "/"), sheet = 1, startRow = 1, colNames = T,
                   rowNames = F, detectDates = F, skipEmptyRows = F,
                   skipEmptyCols = F, na.strings = "NA",
                   fillMergedCells = F)

# Define IDs of individuals and their biological parents.
# Also, retain the part of FIMS that needs race imputed.
fims2 <- (fims1
          |> mutate(kid_id = ER30001*1000 + ER30002,
                     mom_id = ER30001_P_F*1000 + ER30002_P_F,
                     dad_id = ER30001_P_M*1000 + ER30002_P_M)
          |> select(kid_id, dad_id, mom_id)
          |> filter(kid_id %in% dft9$id))

# Merge race variables for mothers and fathers with children's IDs.
dft10_b <- dft9_b |> select(id, r_white, r_black, r_other) |> unique()
names(dft10_b) <- paste("mom",names(dft10_b), sep = "_")
fims3 <- left_join(fims2, dft10_b, by = "mom_id")
dft10_b <- dft9_b |> select(id, r_white, r_black, r_other) |> unique()
names(dft10_b) <- paste("dad",names(dft10_b), sep = "_")
fims3 <- left_join(fims3, dft10_b, by = "dad_id")

# Impute race
fims4 <- (fims3
          |> mutate(r_white = as.integer(dad_r_white + mom_r_white > 0),
                     r_black = as.integer(dad_r_black + mom_r_black > 0),
                     r_other = as.integer(dad_r_other + mom_r_other > 0))
          |> select(kid_id, r_white, r_black, r_other))


# Merge back with the partition of the data who needed race imputed.
dft9_a <- left_join(dft9_a, fims4, by = c("id" = "kid_id"))

# Combine the partitions to restore the data set.
dft10 <- rbind(dft9_a, dft9_b)

nrow(dft10) == nrow(dft9)

# Ensure that the race variables are time-invariant.
dft11 <- (dft10 |> group_by(id)
          |> mutate(r_white = as.integer(1 %in% r_white),
                     r_black = as.integer(1 %in% r_black),
                     r_other = as.integer(1 %in% r_other))
          |> ungroup(id))

# Check if these variables are time-invariant.
table(dft11$r_white, dft11$wave)
table(dft11$r_black, dft11$wave)
table(dft11$r_other, dft11$wave)
table(dft11$r_imp_flag, dft11$wave)

# Finally, flag those who do not have race observed/imputed.
dft12 <- dft11 |> mutate(race_na = as.integer(r_white + r_black +
                                                 r_other == 0))

# Check for time-invariance.
table(dft12$race_na, dft12$wave)

# I need to retain only 6 variables, then I'm DONE.
dft13 <- (dft12
          |> select(id, r_white, r_black, r_other, r_imp_flag, race_na)
          |> unique())

df1 <- left_join(df1, dft13, by = "id")


###############################################################################
###### STATE OF RESIDENCE #####################################################

a <- varlist |> filter(vars == "StateNow")
dft1 <- vars2 |> select(id, a$names)
names(dft1) <- c("id", a$waves)

dft2 <- dft1 |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# We retrieve PSID state codes here:
# https://psidonline.isr.umich.edu/data/Documentation/PSIDStateCodes.pdf

state_codes <- read.csv(paste(state_codes_dir,
                              "PSIDStateCodes.csv", sep = "/"))
dft3 <- (left_join(dft2, state_codes, by = "value")
         |> mutate(state_na = as.integer(is.na(value)))
         |> select(-value))

df1 <- left_join(df1, dft3, by = c("id", "wave"))


###############################################################################
###### SPLITOFF INDICATOR #####################################################

a <- varlist |> filter(vars == "Splitoff_Indicator")
dft1 <- vars2 |> select(id, a$names)
names(dft1) <- c("id", a$waves)

dft2 <- dft1 |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# What values do we need to account for?
table(dft2$value, dft2$wave)

# Definition:
# = 1 Reinterview family (i.e., usual FU)
# = 2 Splitoff from Reinterview family
# = 3 Recontact family
# = 4 Splitoff from recontact family

# What do these values' definitions mean, exactly?
# Look at 2019 User Guide, section 4.3 (Response Rates) on p.22:
# https://psidonline.isr.umich.edu/data/Documentation/UserGuide2019.pdf

# Reinterview families: FUs that were interviewed in the prior wave
# Recontact families: FUs interviewed two waves prior to current
# wave, but not the "immediately prior wave"
# Split-off families: "individuals who became economically 
# independent," i.e., "creating their own FU"
# Recontact split-off families: FUs that split off from recontact
# families at the time of the interview

# NOTE: Split-offs received the value of 2 or 4 only in the wave
# that they split off. In subsequent waves, they are coded as
# reinterview families.
dft3 <- (dft2 |> group_by(id)
         |> mutate(splitoff_flag = as.integer(2 %in% value|
                                                4 %in% value))
         |> ungroup(id)
         |> mutate(splitoff = as.integer(value %in% c(2,4)),
                    splitoff_na = as.integer(is.na(value)))
         |> select(-value)
         )

table(dft3$splitoff, dft3$wave)

df1 <- left_join(df1, dft3, by = c("id", "wave"))

###############################################################################
###### TRANSFER INCOME ###############################################

a <- varlist |> filter(vars == "TransferIncome_HeadAndSpouse")
dft1 <- vars2 |> select(id, a$names)
names(dft1) <- c("id", a$waves)

dft2 <- dft1 |> pivot_longer(cols = starts_with("2"),
                              names_to = "wave")

# Definition:
# Continuous variable.
# Total transfer income (of head and spouse) based on last year's tax reports.
# (i.e., in between biennial waves), EXCLUDING social security.
# PSID recommends considering the exclusion outliers (above 99th percentile.)

dft3 <- (dft2 |> mutate(transfer_inc = value,
                         transfer_notZero = as.integer(value > 0),
                         transfer_inc_na = as.integer(is.na(value)))
         |> select(-value))

table(dft3$transfer_notZero, dft3$wave)

dft4 <- dft3 |> select(-transfer_notZero)

df1 <- left_join(df1, dft3, by = c("id", "wave"))


###############################################################################
###### WRITE TO FILE ##########################################################

write.csv(df1, paste(output_dir1, 
                     "psid_data_v1.csv", 
                     sep = "/"), 
          row.names = F)

