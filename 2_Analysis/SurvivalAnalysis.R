# get data into format for survival analysis ---

#save all results so can come back to it later
# results file

participants_inc <- participants(result = inc)
saveRDS(participants_inc, 
        here(output.folder, "ParticipantsInc.rds")) # 1 gb of data
#save study results
save(study_results, 
        here(output.folder, "studyResults.rds"))

readRDS(here(output.folder, "ParticipantsInc.rds"))




#link to the incidence population using participants function in incidence prevalence
# the code below grabs all the analysis for each cancer for whole population without stratifications (all age groups and both genders)
participantsAnalysis <- study_results$incidence_estimates_CPRDAurum %>%
  filter(denominator_age_group == "18;150" & denominator_sex == "Both") %>% distinct(analysis_id, outcome_cohort_name) 

# eventually will create a loop which will loop over each 
# participants_inc <- participants(result = inc, analysisId = 32) %>%
#   filter(!is.na(outcome_start_date)) %>% collect()


participants_inc <- participants(result = inc, analysisId = 3)
#asdsd <- cdm$denominator %>% filter(cohort_definition_id == 1) %>% collect()

participants_inc <- participants(result = inc, analysisId = 3)

asdf <- participants_inc %>% filter(!is.na(outcome_start_date)) %>% collect()



#cancerincprev # these are the original cohorts
# get variables for analysis ---
Pop<-cdm$person %>% 
  inner_join(cdm$ehdenwp2cancerextrap,
             by = c("person_id" = "subject_id" )) %>%
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date,
         cohort_definition_id)  %>% 
  left_join(cdm$observation_period %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
            by = "person_id") %>% 
  left_join(cdm$death %>% 
              select("person_id",  "death_date") %>% 
              distinct(),
            by = "person_id") %>% 
  
  collect()


# only include people with a diagnosis that starts at or after 1st jan 2000 ---
Pop<-Pop %>% 
  filter(cohort_start_date >= '2000-01-01') 

# Only include people with a diagnosis at or before 1st jan 2019 to remove pandemic effects ---
Pop<-Pop %>% 
  filter(cohort_start_date <= '2019-01-01') 


# format data -----
# add age -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month 
  Pop<-Pop %>%
    mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else { 
  Pop<-Pop %>% 
    mutate(age= year(cohort_start_date)-year_of_birth)
}


# age age groups ----
Pop<-Pop %>% 
  mutate(age_gr=ifelse(age<30,  "<30",
                       ifelse(age>=30 &  age<=39,  "30-39",
                              ifelse(age>=40 & age<=49,  "40-49",
                                     ifelse(age>=50 & age<=59,  "50-59",
                                            ifelse(age>=60 & age<=69, "60-69", 
                                                   ifelse(age>=70 & age<=79, "70-79", 
                                                          
                                                          ifelse(age>=80 & age<=89, "80-89",      
                                                                 ifelse(age>=90, ">=90",
                                                                        NA))))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("<30","30-39","40-49", "50-59",
                                   "60-69", "70-79","80-89",">=90"))) 
table(Pop$age_gr, useNA = "always")

# wider age groups
Pop<-Pop %>% 
  mutate(age_gr2=ifelse(age<=50,  "<=50",
                        ifelse(age>50, ">50",
                               NA))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                         levels = c("<=50", ">50")))
table(Pop$age_gr2, useNA = "always")


# reformat gender
# add gender -----
#8507 male
#8532 female
Pop<-Pop %>% 
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>% 
  mutate(gender= factor(gender, 
                        levels = c("Male", "Female")))
table(Pop$gender, useNA = "always")

# if missing (or unreasonable) age or gender, drop ----
Pop<-Pop %>% 
  filter(!is.na(age)) %>% 
  filter(age>=18) %>% 
  filter(age<=110) %>%
  filter(!is.na(gender))

# create sex:agegp categorical variables
Pop <- Pop %>%
  unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
  mutate(genderAgegp= factor(genderAgegp, 
                             levels = c("Female_<30","Female_30-39","Female_40-49", "Female_50-59",
                                        "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                        "Male_<30","Male_30-39","Male_40-49", "Male_50-59",
                                        "Male_60-69", "Male_70-79","Male_80-89","Male_>=90"))) 

# drop if missing observation period end date ----
Pop<-Pop %>% 
  filter(!is.na(observation_period_end_date))

# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365)

# make sure all have year of prior history ---
Pop<-Pop %>%
  filter(prior_obs_years>=1)

# need to make new end of observation period to 1/1/2019 ----
Pop<-Pop %>% 
  mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-01-01', '2019-01-01', NA)) %>%
  mutate(observation_period_end_date_2019 = as.Date(observation_period_end_date_2019) ) %>%
  mutate(observation_period_end_date_2019 = coalesce(observation_period_end_date_2019, observation_period_end_date))


# binary death outcome (for survival) ---
# need to take into account follow up
# if death date is > 1/1/2019 set death to 0
Pop<-Pop %>% 
  mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
  mutate(status= ifelse(death_date > observation_period_end_date_2019 , 1, status )) %>% 
  mutate(status= ifelse(is.na(status), 1, status ))

# calculate follow up in years
Pop<-Pop %>%  
  mutate(time_days=as.numeric(difftime(observation_period_end_date_2019,
                                       cohort_start_date,
                                       units="days"))) %>% 
  #  mutate(time_years=time_days/365) 
  mutate(time_years=time_days/365) 


# remove people with end of observation end date == cohort entry
Pop<-Pop %>%
  filter(time_days != 0)

# capture output in list
observedkm <- list()
observedmedianKM <- list()
observedhazotKM <- list()
observedrisktableKM <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  #carry out km estimate
  observedkm[[j]] <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
    tidy() %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both") %>%
    filter(n.risk >= 5) #remove entries with less than 5 patients
  
  print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  # get the risk table ---
  grid <- seq(0,floor(max(data$time_years)),by=2)
  observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    mutate(Method = "Observed", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) %>%
    slice(1)
  
  print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  
  # KM median survival---
  modelKM <- survfit(Surv(time_years, status) ~ 1, data=data) %>%
    summary()
  
  observedmedianKM[[j]] <- modelKM$table %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%  
    pivot_longer(-rowname) %>% 
    pivot_wider(names_from=rowname, values_from=value) %>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j],
           Gender = "Both" ,
           Age = "All" ) %>%
    select(-name)
  
  
  print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  # hazard function over time ----
  # paper https://arxiv.org/pdf/1509.03253.pdf states bshazard good package
  
  observedhazotKM[[j]] <- as.data.frame.bshazard(bshazard(Surv(time_years, status) ~ 1, data=data, verbose=FALSE)) %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both")
  
  print(paste0("Hazard over time results for KM ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined <- dplyr::bind_rows(observedmedianKM) 

hotkmcombined <- dplyr::bind_rows(observedhazotKM) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci )

# generate the risk table and remove entries < 5 patients
risktableskm <- dplyr::bind_rows(observedrisktableKM)%>%
  mutate(across(everything(), ~replace(., . <=  5 , NA))) %>%
  replace(is.na(.), "<5") %>%
  relocate(Cancer)


# once all done can merge all the results together in a rdata file
# put all the results into a list
ResultsKM_ALL <- list("KM_observed_all" = observedkmcombined, 
                      "KM_MedianSur_all" = medkmcombined,
                      "KM_hazard_rate_all" = hotkmcombined,
                      "KM_risktable_all" = risktableskm)

#write to excel
openxlsx::write.xlsx(ResultsKM_ALL, file = here("Results", db.name ,"cancer_KM_observed_results_ALL.xlsx"))