

###############################################################

#title: AAA script analysis
#author: "Danielle Ritz Shala"
#date: 01032021


###############################################################



# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(lme4)





# LOAD DATA
main <- read.csv("TYPE FILENAME OF CLEANED DATASET FOR ANALYSIS HERE")
data <- main




# EXPLORATORY DATA ANALYSIS

library(summarytools)
view(dfSummary(data, plain.ascii = FALSE, style = "grid", tmp.img.dir = st_options("tmp.img.dir")))


# determine all variables/columns in dataset
names(data)


# check data structures
str(data)


# convert data types as necessary
#as.numeric(data$interval_completion_2)
#as.factor(data$completion_2)


# covert/ensure form completion status is of factor type
# this is the binary outcome for logistic regression (0,1)
data$completion <- as.factor(data$completion_2)






#{r grouping, include=FALSE}

# visualise distribution of age
hist(data$age_2, 
     breaks = "FD", 
     main = "Distribution of age for admission inpatient encounters for Month/Year-Year",
     xlab = "age (years)")



# categorise age into groups accordingly
data$age_group <- ifelse(
  data$age_2 <=30, "30 and below", ifelse(
    data$age_2 <45, "30-45", ifelse(
      data$age_2 <60, "45-60", "60 above"
    )
  )
)


# convert groupings into factors
data$age_groups <- as.factor(data$age_group)









# visualise distribution plot of length of stay
hist(data$interval_los2,
     breaks ="FD",
     main = "Distribution of length of stay for admitted inpatient encounters",
     xlab = "patient's length of stay in days")


# categorise patient's length of stay accordingly
# round off length of stay to nearest whole number
data$interval_los <- round(data$interval_los2, digits = 0)
data$interval_los_group <- ifelse(
  data$interval_los <=1, "1 day", ifelse(
    data$interval_los <= 2, "2 days", ifelse(
      data$interval_los <= 3, "3 days", ifelse(
        data$interval_los <= 4, "4 days", ifelse(
          data$interval_los <= 5, "5 days", ifelse(
            data$interval_los <= 6, "6 days", ifelse(
              data$interval_los <= 7, "7 days",ifelse(
                data$interval_los <= 14, "up to 2 weeks", ifelse(
                  data$interval_los <= 28, "up to 4 weeks", "more than 4 weeks")
              )
            )
          )
        )
      )
    )
  )
)

# convert groupings into factors
data$interval_los_groups <- as.factor(data$interval_los_group)












#### Simplify dataframe for analysis
data_analysis <- subset(data, select = c(age_groups, SEX, adm_shift, interval_los_groups, # patient characteristics
                                         hospital, CLINICAL_UNIT, NHPPD, unit_type, # facility characteristics
                                         completion)) # outcome

# check structure of data for analysis (ensure all are factors)
str(data_analysis)


# Save simplified dataframe for analysis as CSV 
# for future reference
write.csv(data_analysis, "analysis_data_for_modelling.csv")








#### Relevel factors in simplified dataset accordingly
#```{r relevel, include=FALSE}


data_analysis$age_groups <- relevel(data_analysis$age_groups, ref = "30 and below")
data_analysis$adm_shift <- relevel(data_analysis$adm_shift, ref = "morning")
data_analysis$interval_los_groups <- relevel(data_analysis$interval_los_groups, ref = "3 days")
data_analysis$NHPPD <- relevel(data_analysis$NHPPD, ref = "NHPPD 6")
data_analysis$hospital <- relevel(data_analysis$hospital, ref = "hospital 1")
data_analysis$unit_type <- relevel (data_analysis$unit_type, ref = "medical")
data_analysis$CLINICAL_UNIT <- relevel(data_analysis$CLINICAL_UNIT, ref = "clinical_unit_1")
















################################################################################################################################
#***
# Start of Single-Level Modelling
#***
################################################################################################################################  
#  ```{r, include=FALSE}
# load packages
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lme4)




## Patient characteristics (single level) -----------------------------------------------------------------------------------
```{r single_patient, include=FALSE}
# patient characteristics: age, sex, admission time/shift, length of stay
univar_pt_age_groups <- glm(completion ~ age_groups, family = binomial, data = data_analysis)
univar_pt_sex <- glm(completion ~ SEX, family = binomial, data = data_analysis)
univar_pt_adm_shift <- glm(completion ~ adm_shift, family = binomial, data=data_analysis) 
univar_pt_interval_los_groups <- glm(completion ~ interval_los_groups, family = binomial, data = data_analysis)







#### Display summary results of each univariate model for patient characteristics in a table 
#```{r summary_single_univar, include=FALSE}
tab_model(univar_pt_age_groups, univar_pt_sex, univar_pt_adm_shift, univar_pt_interval_los_groups,
          show.reflvl = TRUE, 
          title = "Form completion by patient characteristics (univariate analysis)", 
          show.aic = TRUE,
          show.loglik = TRUE,
          show.r2 = TRUE,
          #          show.icc = TRUE,
          show.dev = TRUE)



#Visualise multiple models
#```{r visualisation_patient_uni}

plot_models(univar_pt_age_groups, univar_pt_sex,
            univar_pt_interval_los_groups, univar_pt_adm_shift,
            title = "Form completion by patient characteristics (univariate analysis)",
            m.labels = c("age", "sex", "length of stay", "time of admission"),
            legend.title = "Patient Characteristics"
)














### Facility characteristics (single level) -------------------------------------------------------------------------------------------------------
#```{r single_facility, include=FALSE}
# facility characteristics: hospital, unit type, nursing hours per unit (NHPPD)
univar_fac_hospital <- glm(completion ~ hospital, family = binomial, data = data_analysis)
univar_fac_inpatient_area <- glm(completion ~ CLINICAL_UNIT, family = binomial, data = data_analysis) #inpatient clinical area
univar_fac_NHPPD <- glm(completion ~ NHPPD, family = binomial, data = data_analysis)
univar_fac_unit_type <- glm(completion ~ unit_type, family = binomial, data = data_analysis)





#### Display summary results of univariate models for facility characteristics in a table

#```{r summary_facility, include=FALSE}
tab_model(univar_fac_hospital, univar_fac_NHPPD, univar_fac_unit_type,
          show.reflvl = TRUE, 
          title = "Form completion by facility characteristics (univariate analysis)",
          show.aic = TRUE,
          show.loglik = TRUE,
          show.r2 = TRUE,
          #          show.icc = TRUE,
          show.dev = TRUE)




#### Display summary results of univariate model for clinical units in a table
#```{r summary_facility_uni, include=FALSE}
tab_model(univar_fac_inpatient_area , show.reflvl = TRUE, title = "Generalised Linear Model for inpatient clinical areas",
          show.aic = TRUE,
          show.loglik = TRUE,
          show.r2 = TRUE,
          show.icc = TRUE,
          show.dev = TRUE)




#Visualise models for clinical units
#unsorted
plot_model(univar_fac_inpatient_area, 
           title = "Form completion by clinical units (univariate model)"
)
#sorted
plot_model(univar_fac_inpatient_area, 
           title = "Form completion by clinical units (univariate model)", 
           sort.est = TRUE
)

















################################################################################################################################
#***
# START OF MULTI-LEVEL MODELLING
#***
################################################################################################################################  





#Summarise best models: 
#1. single-level model
#2. multilevel model with hospital as random factor
#3. multilevel model with clinical areas (CLINICAL_UNIT) as random factor










#### Check for correlation between NHPPD and unit_type
cor.test(data_analysis$NHPPD, data_analysis$unit_type)

library(MASS)
tbl=table(data_analysis$NHPPD, data_analysis$unit_type)

chisq.test(tbl)










#### Run multilevel models with NHPPD and unit_type as factors separately (since correlated)
m5_clinical_unit_random_all_NHPPD <- glmer(completion ~ (1 | CLINICAL_UNIT) + 
                                         SEX + adm_shift + interval_los_groups + age_groups +
                                         NHPPD, 
                                       family = binomial(link = logit), data = data_analysis)







# summarise result of final model in a table
tab_model(m5_clinical_unit_random_all_NHPPD, 
          show.reflvl = TRUE, 
          title = "Final model for RQ3: Multilevel model with clinical area as random factor, NHPPD as facility characteristic",
          show.aic = TRUE,
          show.loglik = TRUE,
          show.r2 = TRUE,
          show.icc = TRUE,
          show.dev = TRUE
)


#Visualise final model for project
#unsorted
plot_model(m5_clinical_unit_random_all_NHPPD,
           title = "Final model for RQ3: Multilevel model with clinical area as random factor",
)
#sorted
plot_model(m5_clinical_unit_random_all_NHPPD,
           title = "Final model for RQ3: Multilevel model with clinical area as random factor",
           sort.est = TRUE
)























################################################################################################################################
#***
# MODEL SUMMARIES
#***
################################################################################################################################  


tab_model(m5_clinical_unit_random_all_NHPPD, # final model
          univar_pt_sex, univar_pt_adm_shift, univar_pt_interval_los_groups, univar_pt_age_groups, # patient characteristics
          univar_fac_NHPPD, # facility characteristics
          show.reflvl = TRUE, 
          title = "Summary of final model, patient, and facility characteristic (NHPPD)",
          show.aic = TRUE,
          show.loglik = TRUE,
          show.r2 = TRUE,
          show.icc = TRUE,
          show.dev = TRUE
)






################################################################################################################################
# frequency summaries
################################################################################################################################
library(table1)
table1(~ SEX + age_2 + adm_shift + interval_los2 + NHPPD| completion_2, 
       data=data,
       caption = "Summary of form completion for admission inpatient encounters")
#footnote = "Length of stay=missing for inpatient admission encounters with discharge date outside of study period")


table1(~ hospital| completion_2, 
       data=data,
       caption = "Form completion by hospital")


table1(~ SEX + age_groups + adm_shift + interval_los_groups + NHPPD| completion, 
       data=data_analysis,
       caption = "Summary of form completion by characteristic (grouped)")


