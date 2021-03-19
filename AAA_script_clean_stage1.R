

##########################################################################################################
# This file consolidates the data preprocessing/cleaning for the AAA research study. 
# All file paths and data have been removed and/or renamed in this script.
# Variable names and/or descriptions are used instead.
# The reader will need to add in own data for this script to work. 
##########################################################################################################

# LIBRARIES, DIRECTORY, DATA

# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(readr)


# set working directory
dir <- "TYPE THE DIRECTORY PATH HERE"
setwd(dir)


##############################################################################
# read data per facility from individual csv files 
data_hospital1 <- read.csv("hospital1.csv")
data_hospital2 <- read.csv("hospital2.csv")
data_hospital3 <- read.csv("hospital3.csv")
data_hospital4 <- read.csv("hospital4.csv")


##############################################################################
# COMBINING DATASETS

# add column for "hospital" to label each dataset with facility
# convert hospital column into factor instead of character
data_hospital1$hospital <- as.factor("hospital1")
data_hospital2$hospital <- as.factor("hospital2")
data_hospital3$hospital <- as.factor("hospital3")


# combine data from all hospitals into one dataframe
# ****consider creating a function/for-loop if more dataframes to be joined
data_join_1 <- rbind(data_hospital1, data_hospital2)
data_join_2 <- rbind(data_join_1, data_hospital3)


# create dataframe of combined hospitals
data_all_hospitals <- data_join_2
# reference point
main <- data_all_hospitals



# DATA CHECK 1 ****************************************************************
# check that total number of observations for all joined dataframes match
nrow_all <- nrow(data_hospital1) + nrow(data_hospital2) + nrow(data_hospital3)
ifelse(nrow_all == nrow(data_all_hospitals), 
       "TEST PASSED: total rows in combined datasets correct", 
       "TEST FAILED: total rows in combined datasets incorrect")






##################################################################################
# DEFINE DATA FOR CLEANING HERE
data1 <- data_all_hospitals



##################################################################################

# EXPLORATORY DATA ANALYSIS (EDA)
summary(data1)
head(data1)
dim(data1)
typeof(data1)
str(data1)




##################################################################################
##################################################################################
# EDA AND DATA CLEANING for variables


# VARIABLE: AGE
summary(data1$AGE)
str(data1$AGE)


# create new column age_2 
# to convert age into numeric and remove "Y" character from age column
data1$age_2 <- as.numeric(gsub("Y", "", data1$AGE))


# visualise distribution of age in the dataset
hist(data1$age_2,
     main = "EDA: Age of ALL admission inpatient encounters from XXX to XXX",
     xlab= "age in years")
summary(data1$age_2)


# separate paediatric and adult patient encounters in dataset
age_paediatric <- data1 %>%
  filter(age_2<16)


# separate all values not within adult age range (age>16) OR not paediatric (<16) numerics
# this contain alls values not within age range
age_unk <- data1 %>%
  filter(is.na(data1$age_2)) 


# separate adult dataframe
age_adult <- data1 %>%
  filter(age_2>=16) # **** this is the adult dataframe


# visualise the adult ages in the dataset
hist(age_adult$age_2, 
     main = "Age of ADULT patients admitted from XXX to XXX (entire dataset)",
     xlab= "Age in years")


# DATA CHECK 2 *********************************************************************
# check that total number of observations for age dataframes match
nrow_age <- nrow(age_paediatric) + nrow(age_unk) + nrow(age_adult)
ifelse(nrow_age == nrow(data_all_hospitals), 
       "TEST PASSED: total rows in combined datasets (age) correct", 
       "TEST FAILED: total rows in combined datasets (age) incorrect")




##################################################################################
# DEFINE DATAFRAME FOR NEXT STAGE OF CLEANING
# this is dataframe of age-appropriate patient encounters for the study (age_adult)
data2 <- age_adult






#################################################################################
###################################################################################
# VARIABLE: FORM COMPLETION



# classify form completion status
# create column for "completion" in dataframe
# 1=yes/completed, verified
# 0=no/not completed, modified
data2$completion_2 <- if_else(data2$FORM_STATUS == "", 0, 1)




# separate dataframes for completed_verified, completed_modified, and noncompleted forms
# for descriptive statistics

# select dataframe with completed forms
# this is the dataframe of all modified and completed forms
completed_all <- filter(data2, FORM_STATUS != "")
# select dataframe of NONcompleted forms (no form completed for patient)
completed_none <-filter (data2, FORM_STATUS == "")
# this is the dataframe of all verified forms
completed_verified <- filter(data2, FORM_STATUS == "Auth (Verified)")
# this is the dataframe of all modified forms
completed_modified <- filter(data2, FORM_STATUS == "Modified")



# DATA CHECK *****************************************************************************
# check that df of completion matches total number of completed forms from adult dataset(age_adult)
nrow_completion_status <- nrow(completed_modified) + nrow(completed_verified) + nrow(completed_none)
ifelse(nrow_completion_status == nrow(age_adult), 
       "TEST PASSED: total rows in combined datasets (completion) correct", 
       "TEST FAILED: total rows in combined datasets (completion) incorrect")


# summarise
# frequency table of completion status
table_completion<- freq(data2$completion_2)
print("Frequency table of completion status")
table_completion
table(data2$completion_2)







##################################################################################
# DEFINE DATAFRAME FOR NEXT STAGE OF CLEANING
# this is the dataframe of adult patient encounters with form completion status
data <- data2









#################################################################################
#################################################################################
# TIME INTERVAL CALCULATIONS AND CONVERSIONS


# convert both date and time columns into POSIX object
# combine date and time column into one for accurate calculation of intervals
# for admission, form completion, and discharge timestamps
data$adm_date2 <- as.POSIXct(paste(data$ADM_DATE, data$ADM_TIME), format = "%d/%m/%Y %H:%M")
data$form_date2 <- as.POSIXct(paste(data$FORM_DATE, data$FORM_TIME), format = "%d/%m/%Y %H:%M")
data$dis_date2 <- as.POSIXct(paste(data$DIS_DATE, data$DIS_TIME), format = "%d/%m/%Y %H:%M")


# calculate INTERVALS
# round intervals to the nearest 2 digits
# interval 1(interval_los): length of stay (los) = discharge - admission
# **** NOTE: calculated interval unit in DAYS
data$interval_los <- round(difftime(data$dis_date, data$adm_date, units = "days"), digits = 2)
data$interval_los2 <- as.numeric(data$interval_los)

# interval 2(interval_completion): admission to form completion = form - admission
# **** NOTE: calculated interval unit in HOURS
data$interval_completion <- round(difftime(data$form_date, data$adm_date, units = "hours"), digits = 2)
data$interval_completion2 <- as.numeric(data$interval_completion)





#################################################################################
# EDA AND CLEANING OF VARIABLE: intervals 

# EDA of intervals
summary(data$interval_completion2)
summary(data$interval_los2)

# exclude values out of range
# 0 used as filter to determine negative values
interval_completion2_exc <- data %>%
  filter(data$interval_completion2 <= 0) 

# clean dataset with interval of form completion >0 (hours)
# this excludes values less than 0 (effectively negative values,as above)
interval_completion2_inc <- data %>%
  filter(data$interval_completion2 > 0) 

# duplicate current data (after exclusion of interval completion as above)
# for dataset checks/testing/comparison later
# this is the dataset with correct age, forms completed, form interval completion >0
data3 <- interval_completion2_inc




# DATA CHECK *****************************************************************************
# check that total number of observations for dataframes match(interval_completion2)
nrow_interval_completion <- nrow(interval_completion2_exc) + nrow(interval_completion2_inc)
nrow_completed_forms <- nrow(completed_verified) + nrow(completed_modified)
ifelse(nrow_interval_completion == nrow_completed_forms, 
       "TEST PASSED: total rows in combined datasets correct", 
       "TEST FAILED: total rows in combined datasets incorrect")



##################################################################################
# DEFINE DATAFRAME FOR NEXT STAGE OF CLEANING
# cleaned dataset excluding outliers/inappropriate values for 
# valid age, completed forms, interval between admission time to form time (interval_completion2)
data <- data3

##################################################################################



# END OF SECTION
###############################################################################
###############################################################################








########################################################################
########################################################################
# TIMELINESS OF COMPLETION
########################################################################
########################################################################


# classify time intervals
# set categories for timeliness
data$time_cat = ifelse(
  data$interval_completion2 <= 8, "8 hours", ifelse(
    data$interval_completion2 <16, "16 hours", ifelse(
      data$interval_completion2 <24, "24 hours", ifelse(
        data$interval_completion2 <48, "48 hours", ifelse(
          data$interval_completion2 <72, "72 hours", ifelse(
            data$interval_completion2 <=120, "120 hours", "more than 120 hours")
        )
      )
    )
  )
)


# summarise
# create frequency table of time categories for timeliness
# this table summarises how soon after admission time an AAA is completed
# this table summarises the timeliness of AAA completion according to length of \n
# interval/time elapsed from admission
library(questionr)
freq(data$time_cat, cum = TRUE, sort = "dec", total = TRUE)


# determine the earliest(shortest) and latest(longest) time intervals for form completion
earliest_AAA <- data %>%
  arrange(interval_completion2)

latest_AAA <- data %>%
  arrange(-interval_completion2)





# categorise patient admission encounters into shifts based on time of admission
# note that shift start times vary in units/hospitals, and overlaps exist 
# consider other factors in deciding clinical shift/time blocks

# categorise admission time by shift/time of day
data$adm_time2 <- format(data$adm_date2, format = "%H%M")
data$adm_shift <- with(data, ifelse(adm_time2 < "0700", "night",
                                    ifelse(adm_time2 < "1300", "morning",
                                           ifelse(adm_time2 < "2100", "evening",
                                                  "night"))))

# categorise form time by shift/time of day
data$form_time2 <- format(data$form_date2, format = "%H%M")
data$form_shift <- with(data, ifelse(form_time2 < "0700", "night",
                                     ifelse(form_time2 < "1300", "morning",
                                            ifelse(form_time2 < "2100", "evening",
                                                   "night"))))




# summarise
# create frequency table for shifts: admission
# this summarises the time of day patients are admitted
library(questionr)
freq(data$adm_shift, cum = TRUE, sort = "dec", total = TRUE)

# create frequency table for shifts: form
# this summarises the time of day forms are being completed
freq(data$form_shift, cum = TRUE, sort = "dec", total = TRUE)







########################################################################

# EDA VISUALISATION OF INTERVAL: LENGTH OF STAY
# this summarises the length of stay for patients in the dataset
#table_los <- freq(data$interval_los2, cum = TRUE, sort = "dec", total = TRUE)
# this shows the distribution of patients
hist(data$interval_los2, breaks = "FD", 
     #xlim = c(0,3), 
     main="EDA PLOT: Distribution of patient's length of stay (days)",
     xlab="days")

hist(data$interval_los2, breaks = "FD", 
     xlim = c(0,15), 
     main="EDA PLOT: Distribution of patient's length of stay (15 days)",
     xlab="days")

hist(data$interval_los2, breaks = "FD", 
     xlim = c(0,3), 
     main="EDA PLOT: Distribution of patient's length of stay (3 days)",
     xlab="days")



# this counts encounters with length of stay less than or equal to 1 in the dataset
# this could be compared with stay type overnight bed days/day stay
data %>%
  filter(interval_los2 <= 1)
summary_los <- data %>%
  count(interval_los2 <= 1)









# TABLE SUMMARIES **************************************************************************************************

library(table1)

# create labels for table
label(data$age_2) <- "Age (years)"
label(data$interval_los2)  <- "Length of stay (days)"
label(data$SEX) <- "Sex"
label(data$adm_shift) <- "Time of admission (shift)"
label(data$interval_completion2) <- "Interval to form completion (hours)"
label(data$form_shift) <- "Time of form completion (shift)"
label(data$time_cat) <- "Time category for interval of form completion"
label(data$completion_2) <- "Form completion"
label(data$hospital) <- "Hospital"
label(data$adm_shift) <- "Time of admission encounter (shift)"



# create sample table
# patient characteristics by completion
table1(~ age_2 + interval_los2 + SEX + adm_shift| completion_2, 
       data=data,
       caption = "Table 1: Patient characteristics and form completion") 


# patient characteristics by facility
table1(~ age_2 + interval_los2 + SEX + adm_shift| hospital, 
       data=data,
       caption = "Table 2: Patient characteristics by hospital") 

# patient characteristics and completion by facility
label(data$hospital)
table1(~ age_2 + interval_los2 + SEX + adm_shift| hospital*completion_2, 
       data=data,
       caption = "Table 3: Patient characteristics and form completion by hospital") 



# facility characteristics (ward, shift, interval) and completion patterns
# form completion patterns by facility
table1(~ form_shift + interval_completion2 | hospital, 
       data=data,
       caption = "Table 4: Form completion by hospital")

# form completion patterns by facility (excluding encounters with no form completed)
# using completed dataframe with valid values
summary_df_completed <- data %>%
  filter(completion_2 == 1) %>%
  filter(interval_completion2 > 0)
label(data$form_shift) <- "Time of form completion (shift)"
table1(~ form_shift + interval_completion2 | hospital, 
       data=summary_df_completed,
       caption = "Table 5: Forms completed by hospital")


# patient factors LOS and time admission by completion
table1(~ interval_los2 + adm_shift | completion_2, 
       data=data,
       caption = "Table 6: Patient's length of stay and time admission by form completion",
       footnote = "Length of stay to be grouped")






########################################################################
########################################################################
########################################################################
# STAGE 2
########################################################################
# CATEGORISING INPATIENT ENCOUNTERS
# by stay type: same-day or overnight encounter
# consider acute bed days, average length of stay for dataset
# LOS, Admission date, Discharge date

# convert dates into POSIXct
data$adm_date_only <- as.POSIXct(data$ADM_DATE, format = "%d/%m/%Y")
data$form_date_only <- as.POSIXct(data$FORM_DATE, format = "%d/%m/%Y")
data$dis_date_only <- as.POSIXct(data$DIS_DATE, format = "%d/%m/%Y")


# determine stay type: 
# 0 = same-day 
# 1 = overnight encounter
data$stay_type <- ifelse(data$adm_date_only==data$dis_date_only, 0, 1)
data$stay_type2 <- ifelse(data$adm_date_only==data$dis_date_only, "same_day", "overnight")

as.factor(data$stay_type2)


# create sample table
library(table1)
# stay type of inpatient encounters
table1(~ stay_type2, 
       data=data,
       caption = "Table x: Stay type of admission inpatient encounters (Month Year-Year)")
# stay type by hospital
table1(~ stay_type2 | hospital, 
       data=data,
       caption = "Table x: Stay type of admission inpatient encounters by hospital (Month Year-Year)")
# stay type by form completion
table1(~ stay_type2 | completion_2, 
       data=data,
       caption = "Table x: Stay type of admission inpatient encounters by form completion ")

# filter data with valid stay type
stay_type_valid <- data %>%
  filter(stay_type2 %in% c("overnight", "same_day"))

# subset rows with missing/NA stay type
# these are patients who have not been discharged yet
stay_type_invalid <- data[is.na(data$stay_type2),]









###################################################################################
###################################################################################
# FILTERING OF UNITS FOR ANALYSIS


# a separate CSV file contained information about the clinical units and 
# whether the units satisfy inclusion/exclusion criteria for the study



# load file of clinical units
main_clinical_units <- read.csv("NAME OF CSV FILE HERE")

# filter columns from csv file
clinical_units_details <- main_clinical_units[,1:6]

# filter units to be included from main clinical_units csv file
clinical_units_for_inclusion <- clinical_units_details %>%
  filter(study.criteria=="include")

# filter units to be joined/filtered in dataset
units_for_analysis <- clinical_units_for_inclusion['CLINICAL_UNIT']



# dataframe of filtered units
library(dplyr)
data_unit_filtered <- data %>%
  filter(CLINICAL_UNIT %in% clinical_units_for_inclusion$CLINICAL_UNIT)

# dataframe of filtered units AND columns for unit clusters
data_cluster_join <- inner_join(data, clinical_units_for_inclusion,
                                by=c("hospital", "CLINICAL_UNIT"))



# DATA CHECK ****************************************************************
# check that values match after filtering out units based on selection criteria (from file)
total_units_for_analysis <- length(unique(clinical_units_for_inclusion$CLINICAL_UNIT)) 
total_units_in_filtered_data <- length(unique(data_unit_filtered$CLINICAL_UNIT))
if_else(total_units_for_analysis == total_units_in_filtered_data,
        "TEST PASSED: total number of units in filtered dataframe correct",
        "TEST FAILED: total number of units in filtered dataframe incorrect")

total_units_for_analysis <- length(unique(clinical_units_for_inclusion$CLINICAL_UNIT)) 
total_units_in_data_cluster_join <- length(unique(data_cluster_join$CLINICAL_UNIT))
if_else(total_units_for_analysis == total_units_in_data_cluster_join,
        "TEST PASSED: total number of units in cluster join dataframe correct",
        "TEST FAILED: total number of units in cluster join dataframe incorrect")

# check that 2 dataframes for unit cleaning equal in length
# data_unit_filtered
ifelse(nrow(data_unit_filtered) == nrow(data_cluster_join),
       "TEST PASSED: total number of observations in unit cleaned dataframes correct",
       "TEST FAILED: total number of observations in unit cleaned dataframes incorrect")       






########################################################################
# STAGE 3
########################################################################

# THIS IS THE DATAFRAME WITH CLEANED/FILTERED UNITS
# THIS IS THE DATAFRAME FOR INITIAL ANALYSIS BASED ON STUDY SELECTION CRITERIA ***
summary(data_cluster_join)



# review and recheck data types and values for each variable 
# clean further as necessary 
# decide columns to retain
str(data_cluster_join)
names(data_cluster_join)



# REDUCE **********************************************
# reduce the dataframe and remove columns/variables not needed for analysis
# define all variables to be removed from the dataframe
variables_to_remove <- c("variable1",
                         "variable2",
                         "variable3",
                         "variable4")

# reduce data
data_reduced = data_cluster_join[,!(names(data_cluster_join) %in% variables_to_remove)]



# CLEAN & CHECK **********************************************
# clean and check values for the variables again 
# remove "indeterminate" values from SEX to ensure binary levels: male and female
data_reduced_cleaned <- data_reduced%>%
  filter("SPECIFY FURTHER CLEANING/FILTERING HERE AS NEEDED")



# CONVERT  **********************************************
# convert data types accordingly
convert_to_factors <- c("completion_2",
                        "time_cat",
                        "adm_shift",
                        "form_shift",
                        "stay_type2")
# convert columns of dataframe to factor type
data_reduced_cleaned[,convert_to_factors] <- lapply(data_reduced_cleaned[,convert_to_factors],factor)
# test and check if conversion successful
str(data_reduced_cleaned)
# alternatively, may save converted data in new dataframe (for double checking/testing and tracking of results)




# DEFINE FINAL CLEANED DATA  **********************************************

final_cleaned_dataset <- data_reduced_cleaned


# END OF DATA CLEANING AND FILTERING UNITS =================================================
# ==========================================================================================
# ==========================================================================================


# export final cleaned dataset to csv file
write.csv(final_cleaned_dataset,"final_cleaned_dataset.csv")






