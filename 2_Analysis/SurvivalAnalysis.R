# KM survival analysis ---

#extract settings for survival
settings_surv <- settings_inc %>%
  filter(analysis_interval == "overall" & denominator_cohort_id == 3)

pops <- list()

for (i in 1:length(settings_surv$analysis_id)){
#extract the participants for each cancer
  pops[[i]] <-cdm$person %>% 
  inner_join(participants(inc, analysisId = settings_surv$analysis_id[i]) %>% filter(!is.na(outcome_start_date)),
             by = c("person_id" = "subject_id" )) %>%
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date,
         cohort_end_date,
         outcome_start_date,
         analysis_id)  %>%
  left_join(cdm$observation_period %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
            by = "person_id") %>% 
  left_join(cdm$death %>% 
              select("person_id",  "death_date") %>% 
              distinct(),
            by = "person_id") %>% 
  collect()

pops[[i]] <- pops[[i]]  %>%
mutate(outcome_cohort_name = settings_surv$outcome_cohort_name[i]) %>%
  mutate(outcome_cohort_id = settings_surv$outcome_cohort_id[i]) 

}

Pop <- dplyr::bind_rows(pops)

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
  mutate(age_gr=ifelse(age<30,  "18-29",
                       ifelse(age>=30 &  age<=39,  "30-39",
                              ifelse(age>=40 & age<=49,  "40-49",
                                     ifelse(age>=50 & age<=59,  "50-59",
                                            ifelse(age>=60 & age<=69, "60-69", 
                                                   ifelse(age>=70 & age<=79, "70-79", 
                                                          
                                                          ifelse(age>=80 & age<=89, "80-89",      
                                                                 ifelse(age>=90, ">=90",
                                                                        NA))))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("18-29","30-39","40-49", "50-59",
                                   "60-69", "70-79","80-89",">=90"))) 
table(Pop$age_gr, useNA = "always")


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
                             levels = c("Female_18-29","Female_30-39","Female_40-49", "Female_50-59",
                                        "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                        "Male_18-29","Male_30-39","Male_40-49", "Male_50-59",
                                        "Male_60-69", "Male_70-79","Male_80-89","Male_>=90"))) 

# drop if missing observation period end date ----
Pop<-Pop %>% 
  filter(!is.na(observation_period_end_date))

# need to make new end of observation period to 31/12/2019 ----
Pop<-Pop %>% 
  mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-12-31', '2019-12-31', NA)) %>%
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
                                       outcome_start_date,
                                       units="days"))) %>% 
  mutate(time_years=time_days/365) 


# remove people with end of observation end date == cohort entry
Pop<-Pop %>%
  filter(time_days != 0)


### KAPLAIN MEIER CODE ####

# whole population
observedkm <- list()
observedmedianKM <- list()
observedrisktableKM <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(outcome_cohort_id == j)
  
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
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both" ) %>%
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
  
}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined <- dplyr::bind_rows(observedmedianKM) 

# generate the risk table and remove entries < 5 patients
risktableskm <- dplyr::bind_rows(observedrisktableKM)%>%
  mutate(across(everything(), ~replace(., . <=  5 , NA))) %>%
  replace(is.na(.), "<5") %>%
  relocate(Cancer) %>%
  mutate(across(everything(), as.character))

info(logger, 'KM analysis for whole population COMPLETE')


# GENDER STRATIFICATION-----

observedkm_gender <- list()
observedmedianKM_gender <- list()
observedrisktableKM_gender <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(outcome_cohort_id == j) 
  
  # add a filter than removes data with 75% missingness
  grid <- seq(0,floor(max(data$time_years)),by=2)
  filter4gender <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All") %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = c("Male", "Female"))
  
  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  # analysis wont run if only 1 gender present
  if(genderlevels == 2){
    
    # get the risk table ---
    observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All") %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = c("Male", "Female")) 
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    #carry out km estimate
    observedkm_gender[[j]] <- survfit (Surv(time_years, status) ~ gender, data=data) %>%
      tidy() %>%
      rename(Gender = strata) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female")) %>%
      filter(n.risk >= 5) #remove entries with less than 5 patients
    
    print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    # KM median survival ---
    modelKM <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
      summary()
    
    # median survival ---
    observedmedianKM_gender[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j], 
             Age = "All" ,
             Gender = c("Male", "Female"))
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
  } else{
    
    print(paste0("Gender stratification KM analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present " , Sys.time()))
    
  }
  
} # this closes the loop on the analysis containing both genders

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_gender <- dplyr::bind_rows(observedkm_gender) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_gender <- dplyr::bind_rows(observedmedianKM_gender) 

#generate the risk table and remove entries < 5 patients
risktableskm_gender <- dplyr::bind_rows(observedrisktableKM_gender) 
risktableskm_gender <- risktableskm_gender %>%
  mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.== 0, NA, .))) %>%  
  mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
  replace(is.na(.), 0) %>%
  relocate(Cancer) %>%
  mutate(across(everything(), as.character))

info(logger, 'KM analysis for gender stratification COMPLETE')

###########################
# AGE STRATIFICATION
##########################

observedkm_age <- list()
observedmedianKM_age <- list()
observedrisktableKM_age <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(outcome_cohort_id == j) 
  
  # get the risk table ---
  grid <- seq(0,floor(max(data$time_years)),by=2)
  observedrisktableKM_age[[j]] <- RiskSetCount(grid,data$time_years[data$age_gr == "18-29"]) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both", Age = c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90")) 
  
  #carry out km estimate
  observedkm_age[[j]] <- survfit (Surv(time_years, status) ~ age_gr, data=data) %>%
    tidy() %>%
    rename(Age = strata) %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], 
           Age = str_replace(Age, "age_gr=<30", "18-29"),
           Age = str_replace(Age, "age_gr=30-39", "30-39"),
           Age = str_replace(Age, "age_gr=40-49", "40-49"),
           Age = str_replace(Age, "age_gr=50-59", "50-59"),
           Age = str_replace(Age, "age_gr=60-69", "60-69"),
           Age = str_replace(Age, "age_gr=70-79", "70-79"),
           Age = str_replace(Age, "age_gr=80-89", "80-89"),
           Age = str_replace(Age, "age_gr=>=90", ">=90"),
           Gender = "Both")
  
  print(paste0("KM for observed data age strat ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  
  # KM median survival---
  modelKM <- survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
    summary()
  
  observedmedianKM_age[[j]] <- modelKM$table %>%
    as.data.frame() %>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j], 
           Gender = "Both" )
  
  print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
}

# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age) 

#generate the risk table and obscure entries < 5 patients
risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age)
risktableskm_age <- risktableskm_age %>%
  mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.== 0, NA, .))) %>%  
  mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
  replace(is.na(.), 0) %>%
  relocate(Cancer) %>%
  mutate(across(everything(), as.character))

info(logger, 'KM analysis for AGE stratification COMPLETE')

##################################################################
# AGE*GENDER STRATIFICATION
##########################

info(logger, 'KM analysis for age*gender stratification START')

# KM observed
observedkm_age_gender <- list()
observedmedianKM_age_gender <- list()
observedrisktableKM_age_gender <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(outcome_cohort_id == j) 

  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  if(genderlevels == 2){
    
    # get the risk table for data after filtering for missingness ---
    grid <- seq(0,floor(max(data$time_years)),by=2)
    observedrisktableKM_age_gender[[j]] <- RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_18-29"]) %>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_>=90"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_18-29"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_>=90"]))%>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j],
             Gender = rep(c("Female", "Male"), each = nlevels(data$age_gr)),
             Age = rep(c("18-29" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"), 2) ) %>%
      unite("GenderAge", c(Gender, Age), remove = FALSE) 
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    #carry out km estimate ---
    observedkm_age_gender[[j]] <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
      tidy() %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], 
             Age = strata ,
             Age = str_replace(Age, "genderAgegp=Female_18-29", "18-29"),
             Age = str_replace(Age, "genderAgegp=Female_30-39", "30-39"),
             Age = str_replace(Age, "genderAgegp=Female_40-49", "40-49"),
             Age = str_replace(Age, "genderAgegp=Female_50-59", "50-59"),
             Age = str_replace(Age, "genderAgegp=Female_60-69", "60-69"),
             Age = str_replace(Age, "genderAgegp=Female_70-79", "70-79"),
             Age = str_replace(Age, "genderAgegp=Female_80-89", "80-89"),
             Age = str_replace(Age, "genderAgegp=Female_>=90", ">=90"),
             Age = str_replace(Age, "genderAgegp=Male_18-29", "18-29"),
             Age = str_replace(Age, "genderAgegp=Male_30-39", "30-39"),
             Age = str_replace(Age, "genderAgegp=Male_40-49", "40-49"),
             Age = str_replace(Age, "genderAgegp=Male_50-59", "50-59"),
             Age = str_replace(Age, "genderAgegp=Male_60-69", "60-69"),
             Age = str_replace(Age, "genderAgegp=Male_70-79", "70-79"),
             Age = str_replace(Age, "genderAgegp=Male_80-89", "80-89"),
             Age = str_replace(Age, "genderAgegp=Male_>=90", ">=90"),       
             Gender = strata,
             Gender = str_replace(Gender, "genderAgegp=Female_18-29", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_30-39", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_40-49", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_50-59", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_60-69", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_70-79", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_80-89", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_>=90", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Male_18-29", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_30-39", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_40-49", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_50-59", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_60-69", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_70-79", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_80-89", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_>=90", "Male"),
             strata = str_replace(strata, "genderAgegp=", "") ) %>%
      rename("GenderAge" = "strata")
    
    print(paste0("KM for observed data age strat ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
    # KM median survival---
    modelKM <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
      summary()
    
    observedmedianKM_age_gender[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j],
             GenderAge = rownames(modelKM$table), 
             GenderAge = str_replace(GenderAge, "genderAgegp=", "")) %>%
      separate(col = "GenderAge", into = c("Gender", "Age"), sep = "_", remove = FALSE)
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
  } else {
    
    
    print(paste0("Gender*Age stratification KM analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present age stratification will have results " , Sys.time()))
    
  }
  
}

# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age_gender <- dplyr::bind_rows(observedkm_age_gender) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_age_gender <- dplyr::bind_rows(observedmedianKM_age_gender) 

#generate the risk table and remove entries < 5 patients
risktableskm_age_gender <- dplyr::bind_rows(observedrisktableKM_age_gender) 

risktableskm_age_gender <- risktableskm_age_gender %>%
  mutate_at(.vars = c(1:(ncol(risktableskm_age_gender)-5)), funs(ifelse(.<= 5, "<5", .))) %>%
  replace(is.na(.), 0) %>%
  relocate(Cancer) %>%
  mutate(across(everything(), as.character))

info(logger, 'KM analysis for AGE*GENDER stratification COMPLETE')


