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

### PURPOSE: Creating list of variable names for copying and pasting
# into the "variable list" data retrieval method from the PSID data center.

### DATE CREATED: Nov 8 2021

### NOTES: I copied and pasted the long strings from PSID variable searches.
# The bracketed 2-integer sub-strings help identify the year pertaining to
# a particular variable (non-summary variables are year-specific). These 
# sub-strings appear in the long strings automatically, so I need to separate
# them manually. (I do this in this script.)

### CHANGES: 

# Dec 31 2021

###############################################################################
###############################################################################
### FILES THIS R SCRIPT USES (INPUT)


###############################################################################
### FILES THIS R SCRIPT CREATES (OUTPUT)
outputf <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                     "data", "PSID_variablenames", "family", fsep = "/")
outputi <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                     "data", "PSID_variablenames", "individual", fsep = "/")
output <- file.path("C:", "Users", "Owner", "Dropbox", "psid_coresidence",
                    "data", "PSID_variablenames", fsep = "/")

# Family-level variables.
child_exp_out <- paste(outputf, "ChildcareExpenditures.csv", sep = "/")
hi1_out <- paste(outputf, "HealthInsurance_Men1.csv", sep = "/")
hi2_out <- paste(outputf, "HealthInsurance_Men2.csv", sep = "/")
hi3_out <- paste(outputf, "HealthInsurance_Men3.csv", sep = "/")
house_id_out <- paste(outputf, "HouseholdID.csv", sep = "/")
marital_status_hd_out <- paste(outputf, "MaritalStatusOfHead.csv", sep = "/")
num_child_fu_out <- paste(outputf, "NumberOfChildrenInFU.csv", sep = "/")
own_or_rent_out <- paste(outputf, "OwnOrRent.csv", sep = "/")
pension_hd_out <- paste(outputf, "WhetherPension_Head.csv", sep = "/")
pension_sp_out <- paste(outputf, "WhetherPension_Spouse.csv", sep = "/")
race1_out <- paste(outputf, "RaceOfHead_Men1.csv", sep = "/")
race2_out <- paste(outputf, "RaceOfHead_Men2.csv", sep = "/")
race3_out <- paste(outputf, "RaceOfHead_Men3.csv", sep = "/")
race_sp1_out <- paste(outputf, "RaceOfSpouse_Men1.csv", sep = "/")
race_sp2_out <- paste(outputf, "RaceOfSpouse_Men2.csv", sep = "/")
race_sp3_out <- paste(outputf, "RaceOfSpouse_Men3.csv", sep = "/")

race_s_out <- paste(outputf, "RaceOfHead_SpanishDescent.csv", sep = "/")
splitoff_out <- paste(outputf, "Splitoff_Indicator.csv", sep = "/")
state_now_out <- paste(outputf, "StateNow.csv", sep = "/")
transfers_out <- paste(outputf, "TransferIncome_HeadAndSpouse.csv", sep = "/")


# Individual-level variables.
age_out <- paste(outputi, "Age.csv", sep = "/")
emp_status_out <- paste(outputi, "EmploymentStatus.csv", sep = "/")
educ_out <- paste(outputi, "Education.csv", sep = "/")
int_id_out <- paste(outputi, "InterviewID.csv", sep = "/")
marr_pairs_ind_out <- paste(outputi, "MarriedPairsIndicator.csv", sep = "/")
rel_to_head_out <- paste(outputi, "RelationshipToHead.csv", sep = "/")
seq_num_out <- paste(outputi, "SequenceNumber.csv", sep = "/")

### MASTER VARIABLE LIST
varlist_out <- paste(output, "VariableList_v1.csv", sep = "/")

###############################################################################
###############################################################################

# Function for creating a data frame listing the variable names by wave.
vlist <- function(vars1){
    vars2 <- strsplit(vars1, " ")
    waves <- integer()
    names <- character()
    for(a in vars2){
        for(b in a){
          # Extract 2-digit integer (within the brackets)
          waves <- append(waves, as.integer(substring(b, 2, 3)))
          # Extract variable name (i.e, the rest of the vector entry)
          names <- append(names, substring(b, 5))
        }
    }
  
    # Create 4-digit waves
    for(i in 1:length(waves)){
        if(waves[i] <= 21){
          waves[i] <- 2000 + waves[i]
          } else{
              waves[i] <- 1900 + waves[i]
          }
    }
    
    # Create data table
    data.frame(cbind(names,waves))
}

###############################################################################
##### FAMILY-LEVEL VARIABLES. #################################################

# Child care expenditures (by family in previous year).
child_exp1 <- "[99]ER16515D1 [01]ER20456D1 [03]ER24138D1 [05]ER28037D2 [07]ER41027D2 [09]ER46971D2 [11]ER52395D2 [13]ER58212D2 [15]ER65438 [17]ER71516 [19]ER77564"
child_exp <- vlist(child_exp1)
write.csv(child_exp, child_exp_out, row.names = F)

# Health insurance mentions 1/2/3.
hi_men1 <- "[99]ER33518 [01]ER33618 [03]ER33718 [05]ER33819 [07]ER33919 [09]ER34022 [11]ER34121"
hi1 <- vlist(hi_men1)
write.csv(hi1, hi1_out, row.names = F)
hi_men2 <- "[99]ER33519 [01]ER33619 [03]ER33719 [05]ER33820 [07]ER33920 [09]ER34023 [11]ER34122"
hi2 <- vlist(hi_men2)
write.csv(hi2, hi2_out, row.names = F)
hi_men3 <- "[99]ER33520 [01]ER33620 [03]ER33720 [05]ER33821 [07]ER33921 [09]ER34024 [11]ER34123"
hi3 <- vlist(hi_men3)
write.csv(hi3, hi3_out, row.names = F)

# Household ID number. (can be shared among FUs)
house_id1 <- "[69]V1015 [70]V1766 [71]V2345 [72]V2979 [73]V3310 [74]V3730 [75]V4231 [76]V5113 [77]V5681 [78]V6220 [79]V6814 [80]V7456 [81]V8110 [85]V12443 [86]V13682 [87]V14732 [88]V16207 [89]V17584 [90]V18936 [91]V20236 [92]V21542 [93]V23356 [94]ER4159R [95]ER6999R [96]ER9250R [97]ER12223R [99]ER16447 [01]ER20393 [03]ER24170 [05]ER28069 [07]ER41059 [09]ER47003 [11]ER52427 [13]ER58245 [15]ER65481 [17]ER71560 [19]ER77621"
house_id <- vlist(house_id1)
write.csv(house_id, house_id_out, row.names = F)

# Marital status (of head).
marital_status_hd1 <- "[77]V5502 [78]V6034 [79]V6659 [80]V7261 [81]V7952 [82]V8603 [83]V9276 [84]V10426 [85]V11612 [86]V13017 [87]V14120 [88]V15136 [89]V16637 [90]V18055 [91]V19355 [92]V20657 [93]V22412 [94]ER2014 [95]ER5013 [96]ER7013 [97]ER10016 [99]ER13021 [01]ER17024 [03]ER21023 [05]ER25023 [07]ER36023 [09]ER42023 [11]ER47323 [13]ER53023 [15]ER60024 [17]ER66024 [19]ER72024"
marital_status_hd <- vlist(marital_status_hd1)
write.csv(marital_status_hd, marital_status_hd_out, row.names = F)

# Number of children in the family unit.
num_child_fu1 <- "[68]V398 [69]V550 [70]V1242 [71]V1945 [72]V2545 [73]V3098 [74]V3511 [75]V3924 [76]V4439 [77]V5353 [78]V5853 [79]V6465 [80]V7070 [81]V7661 [82]V8355 [83]V8964 [84]V10422 [85]V11609 [86]V13014 [87]V14117 [88]V15133 [89]V16634 [90]V18052 [91]V19352 [92]V20654 [93]V22409 [94]ER2010 [95]ER5009 [96]ER7009 [97]ER10012 [99]ER13013 [01]ER17016 [03]ER21020 [05]ER25020 [07]ER36020 [09]ER42020 [11]ER47320 [13]ER53020 [15]ER60021 [17]ER66021 [19]ER72021"
num_child_fu <- vlist(num_child_fu1)
write.csv(num_child_fu, num_child_fu_out, row.names = F)

# Own or rent a home.
own_or_rent1 <- "[68]V103 [69]V593 [70]V1264 [71]V1967 [72]V2566 [73]V3108 [74]V3522 [75]V3939 [76]V4450 [77]V5364 [78]V5864 [79]V6479 [80]V7084 [81]V7675 [82]V8364 [83]V8974 [84]V10437 [85]V11618 [86]V13023 [87]V14126 [88]V15140 [89]V16641 [90]V18072 [91]V19372 [92]V20672 [93]V22427 [94]ER2032 [95]ER5031 [96]ER7031 [97]ER10035 [99]ER13040 [01]ER17043 [03]ER21042 [05]ER25028 [07]ER36028 [09]ER42029 [11]ER47329 [13]ER53029 [15]ER60030 [17]ER66030 [19]ER72030"
own_or_rent <- vlist(own_or_rent1)
write.csv(own_or_rent, own_or_rent_out, row.names = F)

# Whether employer-provided pension or retirement plan.
pension_hd1 <- "[75]V4004 [84]V10480 [89]V16809 [99]ER15156 [01]ER19327 [03]ER22722 [05]ER26703 [07]ER37739 [09]ER43712 [11]ER49057 [13]ER54813 [15]ER61933 [17]ER67987 [19]ER74015"
pension_hd <- vlist(pension_hd1)
write.csv(pension_hd, pension_hd_out, row.names = F)
pension_sp1 <- "[84]V10694 [89]V17128 [99]ER15302 [01]ER19470 [03]ER22866 [05]ER26847 [07]ER37971 [09]ER43944 [11]ER49276 [13]ER55029 [15]ER62150 [17]ER68204 [19]ER74222"
pension_sp <- vlist(pension_sp1)
write.csv(pension_sp, pension_sp_out, row.names = F)

# Race variables: Race of head mentions 1/2/3. Spanish descent of head.
race_men1 <- "[68]V181 [69]V801 [70]V1490 [71]V2202 [72]V2828 [73]V3300 [74]V3720 [75]V4204 [76]V5096 [77]V5662 [78]V6209 [79]V6802 [80]V7447 [81]V8099 [82]V8723 [83]V9408 [84]V11055 [85]V11938 [86]V13565 [87]V14612 [88]V16086 [89]V17483 [90]V18814 [91]V20114 [92]V21420 [93]V23276 [94]ER3944 [95]ER6814 [96]ER9060 [97]ER11848 [99]ER15928 [01]ER19989 [03]ER23426 [05]ER27393 [07]ER40565 [09]ER46543 [11]ER51904 [13]ER57659 [15]ER64810 [17]ER70882 [19]ER76897"
race1 <- vlist(race_men1)
write.csv(race1, race1_out, row.names = F)
race_men2 <- "[85]V11939 [86]V13566 [87]V14613 [88]V16087 [89]V17484 [90]V18815 [91]V20115 [92]V21421 [93]V23277 [94]ER3945 [95]ER6815 [96]ER9061 [97]ER11849 [99]ER15929 [01]ER19990 [03]ER23427 [05]ER27394 [07]ER40566 [09]ER46544 [11]ER51905 [13]ER57660 [15]ER64811 [17]ER70883 [19]ER76898"
race2 <- vlist(race_men2)
write.csv(race2, race2_out, row.names = F)
race_men3 <- "[94]ER3946 [95]ER6816 [96]ER9062 [97]ER11850 [99]ER15930 [01]ER19991 [03]ER23428 [05]ER27395 [07]ER40567 [09]ER46545 [11]ER51906 [13]ER57661 [15]ER64812 [17]ER70884 [19]ER76899"
race3 <- vlist(race_men3)
write.csv(race3, race3_out, row.names = F)
race_span <- "[85]V11937 [86]V13564 [87]V14611 [88]V16085 [89]V17482 [90]V18813 [91]V20113 [92]V21419 [93]V23275 [94]ER3941 [95]ER6811 [96]ER9057 [05]ER27392 [07]ER40564 [09]ER46542 [11]ER51903 [13]ER57658 [15]ER64809 [17]ER70881 [19]ER7689"
race_s <- vlist(race_span)
write.csv(race_s, race_s_out, row.names = F)

# Race variables: Race of spouse mentions 1/2/3.
race_sp_men1 <- "[85]V12293 [86]V13500 [87]V14547 [88]V16021 [89]V17418 [90]V18749 [91]V20049 [92]V21355 [93]V23212 [94]ER3883 [95]ER6753 [96]ER8999 [97]ER11760 [99]ER15836 [01]ER19897 [03]ER23334 [05]ER27297 [07]ER40472 [09]ER46449 [11]ER51810 [13]ER57549 [15]ER64671 [17]ER70744 [19]ER76752"
race_sp1 <- vlist(race_sp_men1)
write.csv(race_sp1, race_sp1_out, row.names = F)
race_sp_men2 <- "[85]V12294 [86]V13501 [87]V14548 [88]V16022 [89]V17419 [90]V18750 [91]V20050 [92]V21356 [93]V23213 [94]ER3884 [95]ER6754 [96]ER9000 [97]ER11761 [99]ER15837 [01]ER19898 [03]ER23335 [05]ER27298 [07]ER40473 [09]ER46450 [11]ER51811 [13]ER57550 [15]ER64672 [17]ER70745 [19]ER76753"
race_sp2 <- vlist(race_sp_men2)
write.csv(race_sp2, race_sp2_out, row.names = F)
race_sp_men3 <- "[94]ER3885 [95]ER6755 [96]ER9001 [97]ER11762 [99]ER15838 [01]ER19899 [03]ER23336 [05]ER27299 [07]ER40474 [09]ER46451 [11]ER51812 [13]ER57551 [15]ER64673 [17]ER70746 [19]ER76754"
race_sp3 <- vlist(race_sp_men3)
write.csv(race_sp3, race_sp3_out, row.names = F)


# Splitoff indicator (whether FU split off from PSID nuclear family)
splitoff1 <- "[69]V909 [70]V1106 [71]V1806 [72]V2407 [73]V3007 [74]V3407 [75]V3807 [76]V4307 [77]V5207 [78]V5707 [79]V6307 [80]V6907 [81]V7507 [82]V8207 [83]V8807 [84]V10007 [85]V11107 [86]V12507 [87]V13707 [88]V14807 [89]V16307 [90]V17707 [91]V19007 [92]V20307 [93]V21606 [94]ER2005F [95]ER5005F [96]ER7005F [97]ER10005F [99]ER13005E [01]ER17006 [03]ER21005 [05]ER25005 [07]ER36005 [09]ER42005 [11]ER47305 [13]ER53005 [15]ER60005 [17]ER66005 [19]ER72005"
splitoff <- vlist(splitoff1)
write.csv(splitoff, splitoff_out, row.names = F)

# State now: visit http://psidonline.isr.umich.edu/data/Documentation/PSIDStateCodes.pdf
state_now1 <- "[68]V93 [69]V537 [70]V1103 [71]V1803 [72]V2403 [73]V3003 [74]V3403 [75]V3803 [76]V4303 [77]V5203 [78]V5703 [79]V6303 [80]V6903 [81]V7503 [82]V8203 [83]V8803 [84]V10003 [85]V11103 [86]V12503 [87]V13703 [88]V14803 [89]V16303 [90]V17703 [91]V19003 [92]V20303 [93]V21603 [94]ER4156 [95]ER6996 [96]ER9247 [97]ER12221 [99]ER13004 [01]ER17004 [03]ER21003 [05]ER25003 [07]ER36003 [09]ER42003 [11]ER47303 [13]ER53003 [15]ER60003 [17]ER66003 [19]ER72003"
state_now <- vlist(state_now1)
write.csv(state_now, state_now_out, row.names = F)

# Total transfer income of head + spouse (excluding social security)
# Outliers (beyond 99th percentile) are "encouraged" to be excluded.
transfers1 <- "[70]V1220 [71]V1922 [72]V2523 [73]V3076 [74]V3488 [75]V3889 [76]V4404 [77]V5316 [78]V5815 [79]V6426 [80]V7016 [81]V7608 [82]V8301 [83]V8909 [84]V10305 [85]V11461 [86]V12868 [87]V13970 [88]V14985 [89]V16485 [90]V17901 [91]V19201 [92]V20501 [93]V22366 [94]ER4147 [95]ER6987 [96]ER9238 [97]ER12071 [99]ER16454 [01]ER20450 [03]ER24101 [05]ER28002 [07]ER40992 [09]ER46900 [11]ER52308 [13]ER58117 [15]ER65314 [17]ER71391 [19]ER77413"
transfers <- vlist(transfers1)
write.csv(transfers, transfers_out, row.names = F)

###############################################################################
##### INDIVIDUAL-LEVEL VARIABLES. #############################################

# Age.
age1 <- "[68]ER30004 [69]ER30023 [70]ER30046 [71]ER30070 [72]ER30094 [73]ER30120 [74]ER30141 [75]ER30163 [76]ER30191 [77]ER30220 [78]ER30249 [79]ER30286 [80]ER30316 [81]ER30346 [82]ER30376 [83]ER30402 [84]ER30432 [85]ER30466 [86]ER30501 [87]ER30538 [88]ER30573 [89]ER30609 [90]ER30645 [91]ER30692 [92]ER30736 [93]ER30809 [94]ER33104 [95]ER33204 [96]ER33304 [97]ER33404 [99]ER33504 [01]ER33604 [03]ER33704 [05]ER33804 [07]ER33904 [09]ER34004 [11]ER34104 [13]ER34204 [15]ER34305 [17]ER34504 [19]ER34704"
age <- vlist(age1)
write.csv(age, age_out, row.names = F)

# Employment status (employed, looking for work disabled, retired, student, etc.)
emp_status1 <- "[79]ER30293 [80]ER30323 [81]ER30353 [82]ER30382 [83]ER30411 [84]ER30441 [85]ER30474 [86]ER30509 [87]ER30545 [88]ER30580 [89]ER30616 [90]ER30653 [91]ER30699 [92]ER30744 [93]ER30816 [94]ER33111 [95]ER33211 [96]ER33311 [97]ER33411 [99]ER33512 [01]ER33612 [03]ER33712 [05]ER33813 [07]ER33913 [09]ER34016 [11]ER34116 [13]ER34216 [15]ER34317 [17]ER34516 [19]ER34716"
emp_status <- vlist(emp_status1)
write.csv(emp_status, emp_status_out, row.names = F)

# Education level (grades 1 through 17).
educ1 <- "[68]ER30010 [70]ER30052 [71]ER30076 [72]ER30100 [73]ER30126 [74]ER30147 [75]ER30169 [76]ER30197 [77]ER30226 [78]ER30255 [79]ER30296 [80]ER30326 [81]ER30356 [82]ER30384 [83]ER30413 [84]ER30443 [85]ER30478 [86]ER30513 [87]ER30549 [88]ER30584 [89]ER30620 [90]ER30657 [91]ER30703 [92]ER30748 [93]ER30820 [94]ER33115 [95]ER33215 [96]ER33315 [97]ER33415 [99]ER33516 [01]ER33616 [03]ER33716 [05]ER33817 [07]ER33917 [09]ER34020 [11]ER34119 [13]ER34230 [15]ER34349 [17]ER34548 [19]ER34752"
educ <- vlist(educ1)
write.csv(educ, educ_out, row.names = F)

# Interview ID number (to be matched with family-level data).
int_id1 <- "[68]ER30001 [69]ER30020 [70]ER30043 [71]ER30067 [72]ER30091 [73]ER30117 [74]ER30138 [75]ER30160 [76]ER30188 [77]ER30217 [78]ER30246 [79]ER30283 [80]ER30313 [81]ER30343 [82]ER30373 [83]ER30399 [84]ER30429 [85]ER30463 [86]ER30498 [87]ER30535 [88]ER30570 [89]ER30606 [90]ER30642 [91]ER30689 [92]ER30733 [93]ER30806 [94]ER33101 [95]ER33201 [96]ER33301 [97]ER33401 [99]ER33501 [01]ER33601 [03]ER33701 [05]ER33801 [07]ER33901 [09]ER34001 [11]ER34101 [13]ER34201 [15]ER34301 [17]ER34501 [19]ER34701"
int_id <- vlist(int_id1)
write.csv(int_id, int_id_out, row.names = F)

# Married pairs indicator: are person and spouse are married&present in FU?
# Further, =1 if the married pair are head/spouse of head.
marr_pairs_ind1 <- "[68]ER30005 [69]ER30024 [70]ER30047 [71]ER30071 [72]ER30095 [73]ER30121 [74]ER30142 [75]ER30164 [76]ER30192 [77]ER30221 [78]ER30250 [79]ER30287 [80]ER30317 [81]ER30347 [82]ER30377 [83]ER30405 [84]ER30435 [85]ER30469 [86]ER30504 [87]ER30541 [88]ER30576 [89]ER30612 [90]ER30648 [91]ER30695 [92]ER30739 [93]ER30812 [94]ER33107 [95]ER33207 [96]ER33307 [97]ER33407 [99]ER33507 [01]ER33607 [03]ER33707 [05]ER33807 [07]ER33907 [09]ER34007 [11]ER34107 [13]ER34207 [15]ER34308 [17]ER34507 [19]ER34707"
marr_pairs_ind <- vlist(marr_pairs_ind1)
write.csv(marr_pairs_ind, marr_pairs_ind_out, row.names = F)

# Relationship to head.
rel_to_head1 <- "[68]ER30003 [69]ER30022 [70]ER30045 [71]ER30069 [72]ER30093 [73]ER30119 [74]ER30140 [75]ER30162 [76]ER30190 [77]ER30219 [78]ER30248 [79]ER30285 [80]ER30315 [81]ER30345 [82]ER30375 [83]ER30401 [84]ER30431 [85]ER30465 [86]ER30500 [87]ER30537 [88]ER30572 [89]ER30608 [90]ER30644 [91]ER30691 [92]ER30735 [93]ER30808 [94]ER33103 [95]ER33203 [96]ER33303 [97]ER33403 [99]ER33503 [01]ER33603 [03]ER33703 [05]ER33803 [07]ER33903 [09]ER34003 [11]ER34103 [13]ER34203 [15]ER34303 [17]ER34503 [19]ER34703"
rel_to_head <- vlist(rel_to_head1)
write.csv(rel_to_head, rel_to_head_out, row.names = F)

# Sequence number.
seq_num1 <- "[69]ER30021 [70]ER30044 [71]ER30068 [72]ER30092 [73]ER30118 [74]ER30139 [75]ER30161 [76]ER30189 [77]ER30218 [78]ER30247 [79]ER30284 [80]ER30314 [81]ER30344 [82]ER30374 [83]ER30400 [84]ER30430 [85]ER30464 [86]ER30499 [87]ER30536 [88]ER30571 [89]ER30607 [90]ER30643 [91]ER30690 [92]ER30734 [93]ER30807 [94]ER33102 [95]ER33202 [96]ER33302 [97]ER33402 [99]ER33502 [01]ER33602 [03]ER33702 [05]ER33802 [07]ER33902 [09]ER34002 [11]ER34102 [13]ER34202 [15]ER34302 [17]ER34502 [19]ER34702"
seq_num <- vlist(seq_num1)
write.csv(seq_num, seq_num_out, row.names = F)

###############################################################################
##### 2003-2011 VARIABLES. ####################################################

df <- data.frame()

for(wd in list(outputf, outputi)){
    setwd(wd)
    for(csv in dir()){
      df_t <- read.csv(csv)
      df_t <- df_t %>% filter(waves %in% 2003:2011)
      df_t$vars <- strsplit(csv, ".csv")[[1]]
      df <- rbind(df, df_t)
    }
}

write.csv(df, varlist_out, row.names = F)
