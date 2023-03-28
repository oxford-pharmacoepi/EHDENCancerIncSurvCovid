# cohort characteristics

# This code extracts the participants from the incidence analysis overall and calculates their baseline characteristics (table 1 for papers)
# This code also generates the data that is inputted into the survival analysis
start <-Sys.time()

# INCIDENCE POPULATION
print(paste0("- Getting cohort characteristics: cancer populations"))
info(logger, "- Getting cohort characteristics: cancer populations")

#instantiate feature disease and medication cohorts
#instantiate feature cohorts (disease)
info(logger, "- getting feature for diseases definitions")

disease_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "ConditionFeatureCohorts"
))

info(logger, "- getting features: diseases")

cdm <- generateCohortSet(cdm, 
                         cohortSet = disease_cohorts,
                         name = feature_disease_table_name,
                         overwrite = TRUE
)

# to see numbers in features
# cdm$cancerincprev7_fdisease %>% 
# group_by(cohort_definition_id) %>%
# tally()

info(logger, "- got features for diseases")

# instantiate feature cohorts (medications)
info(logger, "- getting feature for medications definitions")

medication_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "MedicationFeatureCohorts"
))

info(logger, "- getting features: medications")

cdm <- generateCohortSet(cdm, 
                         cohortSet = medication_cohorts,
                         name = feature_medication_table_name,
                         overwrite = TRUE
)

info(logger, "- got features for medications")

# get denominator to get participants
cdm$denominatordemo <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date(studyStartDate),
  endDate = as.Date(studyEndDate),
  ageGroup =list(
    c(18, 150)),
  sex = c("Both"),
  daysPriorHistory = 365,
  verbose = TRUE
)

# get overall incidence for all cancers
inc_overall_participants <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatordemo",
  outcomeTable = outcome_table_name,
  denominatorCohortId = NULL,
  outcomeCohortId = outcome_cohorts$cohort_definition_id,
  outcomeCohortName = outcome_cohorts$cohort_name,
  interval = c("overall"),
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 5,
  returnParticipants = TRUE,
  tablePrefix = outcome_table_stem,
  verbose = TRUE
)


# #extract settings for survival from incidence results
settings_surv <- settings(inc_overall_participants) 

pops <- list()

for (i in 1:length(settings_surv$analysis_id)){
#extract the participants for each cancer
  
pops[[i]] <-cdm$person %>%
    inner_join(participants(inc_overall_participants, analysisId = as.numeric(settings_surv$analysis_id[i])) %>% filter(!is.na(outcome_start_date)),
               by = c("person_id" = "subject_id" ), copy = TRUE) %>%
    select(person_id,gender_concept_id,
           year_of_birth, month_of_birth, day_of_birth,
           cohort_start_date,
           cohort_end_date,
           outcome_start_date) %>%
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
#add age -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month
  Pop <-Pop %>%
    mutate(age=floor(as.numeric((ymd(outcome_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else {
  Pop <- Pop %>%
    mutate(age= lubridate::year(outcome_start_date)-year_of_birth)
}

# # age age groups ----
Pop <- Pop %>%
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

# # reformat gender
# # add gender -----
# #8507 male
# #8532 female
Pop <-Pop %>%
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>%
  mutate(gender= factor(gender,
                        levels = c("Male", "Female")))
table(Pop$gender, useNA = "always")

# # if missing (or unreasonable) age or gender, drop ----
Pop <-Pop %>%
  filter(!is.na(age)) %>%
  filter(age>=18) %>%
  filter(age<=110) %>%
  filter(!is.na(gender))
#
# # create sex:agegp categorical variables
Pop <- Pop %>%
  unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
  mutate(genderAgegp= factor(genderAgegp,
                             levels = c("Female_18-29","Female_30-39","Female_40-49", "Female_50-59",
                                        "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                        "Male_18-29","Male_30-39","Male_40-49", "Male_50-59",
                                        "Male_60-69", "Male_70-79","Male_80-89","Male_>=90")))

# # drop if missing observation period end date ----
Pop <-Pop %>%
  filter(!is.na(observation_period_end_date))

# get GP number of visits the year prior to diagnosis
# ip.codes<-c(9202)
# # add all descendents
# ip.codes.w.desc<-cdm$concept_ancestor %>%
#   filter(ancestor_concept_id  %in% ip.codes ) %>% 
#   collect() %>% 
#   select(descendant_concept_id) %>% 
#   distinct() %>% 
#   pull()
# 
# Pop <- Pop %>%
#   left_join(
#     Pop %>% 
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$visit_occurrence %>% 
#                    filter(visit_concept_id %in% ip.codes.w.desc) %>% 
#                    filter(visit_start_date > (as.Date(studyStartDate) - lubridate::days(365))) %>%
#                    select("person_id", "visit_start_date") %>% 
#                    compute(),
#                  by=c("person_id"), copy = TRUE, multiple = "all") %>% 
#       filter(visit_start_date < outcome_start_date &
#                visit_start_date >= (outcome_start_date- lubridate::days(365))) %>% 
#       select("person_id") %>% 
#       group_by(person_id) %>% 
#       tally(name = "outpatient_vist") %>% 
#       mutate(outpatient_vist = as.numeric(outpatient_vist)), 
#     by="person_id") %>% 
#   compute()
# Pop  <- Pop %>% 
#   mutate(outpatient_vist=ifelse(is.na(outpatient_vist), 0, outpatient_vist))

# Prior history from date of diagnosis from start of observation period
Pop  <- Pop %>% 
  mutate(Prior_history_days = as.numeric(outcome_start_date - observation_period_start_date ))

# Prior history from date of diagnosis (from 1 jan 2001)
Pop  <- Pop %>% 
  mutate(Prior_history_days_study_start = as.numeric(as.Date(cohort_start_date) - observation_period_start_date ))

# calculate Follow up - calculate end of observation period
Pop <- Pop %>%
  mutate(endOfObservation = ifelse(observation_period_end_date >= studyEndDate, studyEndDate, NA)) %>%
  mutate(endOfObservation = as.Date(endOfObservation) ) %>%
  mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date))

# calculate follow up in years and days
Pop <-Pop %>%
  mutate(time_days=as.numeric(difftime(endOfObservation,
                                       outcome_start_date,
                                       units="days"))) %>%
  mutate(time_years=time_days/365.25)

# binary death outcome (for survival) ---
# need to take into account follow up
# if death date is > database end data set death to 0
Pop <-Pop %>%
  mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
  mutate(status= ifelse(death_date > endOfObservation , 1, status )) %>%
  mutate(status= ifelse(is.na(status), 1, status )) %>%
  mutate(Death = recode(status, 
                        "1" = "Alive", 
                        "2" = "Dead"))

# medications (3 months before index date)
for(i in seq_along(medication_cohorts$cohort_name)){
  working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
  working_id <- medication_cohorts$cohort_definition_id[[i]]
  Pop <-
    Pop %>%
    left_join(
      Pop %>%
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm[[feature_medication_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     rename("feature_end_date"="cohort_end_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!cohort_definition_id),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(
          # overlapping
          (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
             feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
            # ending in window
            (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
               feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_name:=1),
      by="person_id") %>%
    compute()
}

# conditions (other cancers)
for(i in seq_along(outcome_cohorts$cohort_name)){
  working_name <- glue::glue("{outcome_cohorts$cohort_name[[i]]}")
  working_id <- outcome_cohorts$cohort_definition_id[[i]]
  Pop <- 
    Pop %>% 
    left_join(
      Pop %>% 
        select("person_id", "cohort_start_date") %>% 
        inner_join(cdm[[outcome_table_name]] %>% 
                     rename("feature_start_date"="cohort_start_date") %>% 
                     filter(cohort_definition_id== working_id ) %>% 
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(feature_start_date < cohort_start_date) %>% 
        select(person_id) %>% 
        distinct() %>% 
        mutate(!!working_name:=1),
      by="person_id")  %>% 
    compute()
  
}

# conditions (any time in history)
for(i in seq_along(disease_cohorts$cohort_definition_id)){
  
  working_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
  working_id <- disease_cohorts$cohort_definition_id[[i]]
  Pop <- 
    Pop %>% 
    left_join(
      Pop %>% 
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm[[feature_disease_table_name]] %>% 
                     rename("feature_start_date"="cohort_start_date") %>% 
                     filter(cohort_definition_id== working_id ) %>% 
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>% 
        filter(feature_start_date < outcome_start_date) %>% 
        select(person_id) %>% 
        distinct() %>% 
        mutate(!!working_name:=1),
      by="person_id")  %>% 
    compute()
  
}

# tidy up results for table 1
get_summary_characteristics<-function(data){
  
  summary_characteristics<- bind_rows(
    data %>% 
      count() %>% 
      mutate(var="N"),
    
    data %>% 
      summarise(mean=nice.num(mean(age)),
                standard_deviation = nice.num(sd(age)),
                median = nice.num(median(age)),
                interquartile_range=paste0(nice.num.count(quantile(age,probs=0.25)),  " to ",
                                           nice.num.count(quantile(age,probs=0.75)))) %>% 
      mutate(var="age"),
    
    data %>% 
      group_by(age_gr) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="age_gr") %>% 
      mutate(var=paste0("Age group: ", var)),
    
    data %>% 
      mutate(gender=factor(gender, levels=c("Male", "Female"))) %>% 
      group_by(gender) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="gender") %>% 
      mutate(var=paste0("Sex: ", var)),
    
    data %>% 
      mutate(Death=factor(Death, levels=c("Alive", "Dead"))) %>% 
      group_by(Death) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="Death") %>% 
      mutate(var=paste0("Death: ", var)),
    
    data %>% 
      summarise(mean=nice.num.count(mean(Prior_history_days)),
                standard_deviation = nice.num(sd(Prior_history_days)),
                median = nice.num(median(Prior_history_days)),
                interquartile_range=paste0(nice.num.count(quantile(Prior_history_days,probs=0.25)),  " to ",
                                           nice.num.count(quantile(Prior_history_days,probs=0.75)))) %>% 
      mutate(var="Prior_history_days"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((Prior_history_days/365.25))),
                standard_deviation = nice.num(sd((Prior_history_days/365.25))),
                median = nice.num(median((Prior_history_days/365.25))),
                interquartile_range=paste0(nice.num.count(quantile((Prior_history_days/365.25),probs=0.25)),  " to ",
                                           nice.num.count(quantile((Prior_history_days/365.25),probs=0.75)))) %>% 
      mutate(var="Prior_history_years"),
    
    data %>% 
      summarise(mean=nice.num.count(mean(Prior_history_days_study_start)),
                standard_deviation = nice.num(sd(Prior_history_days_study_start)),
                median = nice.num(median(Prior_history_days_study_start)),
                interquartile_range=paste0(nice.num.count(quantile(Prior_history_days_study_start,probs=0.25)),  " to ",
                                           nice.num.count(quantile(Prior_history_days_study_start,probs=0.75)))) %>% 
      mutate(var="Prior_history_days_study_start"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((Prior_history_days_study_start/365.25))),
                standard_deviation = nice.num(sd((Prior_history_days_study_start/365.25))),
                median = nice.num(median((Prior_history_days_study_start/365.25))),
                interquartile_range=paste0(nice.num.count(quantile((Prior_history_days_study_start/365.25),probs=0.25)),  " to ",
                                           nice.num.count(quantile((Prior_history_days_study_start/365.25),probs=0.75)))) %>% 
      mutate(var="Prior_history_years_start"),
    
    
    data %>% 
      summarise(mean=nice.num.count(mean(time_days)),
                standard_deviation = nice.num(sd(time_days)),
                median = nice.num(median(time_days)),
                interquartile_range=paste0(nice.num.count(quantile(time_days,probs=0.25)),  " to ",
                                           nice.num.count(quantile(time_days,probs=0.75)))) %>% 
      mutate(var="time_days"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((time_years))),
                standard_deviation = nice.num(sd((time_years))),
                median = nice.num(median((time_years))),
                interquartile_range=paste0(nice.num.count(quantile((time_years),probs=0.25)),  " to ",
                                           nice.num.count(quantile((time_years),probs=0.75)))) %>% 
      mutate(var="time_years"))
  
  for(i in seq_along(disease_cohorts$cohort_name)){
    working_id_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_id_name)
    )
  }
  
  for(i in seq_along(outcome_cohorts$cohort_name)){
    working_name <- glue::glue("{outcome_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_name)
    )
  }
  
  for(i in seq_along(medication_cohorts$cohort_name)){
    working_id_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_id_name)
    )
  }
  
  # filter any less than 5
  summary_characteristics <- summary_characteristics %>% 
    mutate(mean=ifelse(!is.na(n) & n<5, NA, mean)) %>% 
    mutate(percent=ifelse(!is.na(n) & n<5, NA, percent)) %>% 
    mutate(interquartile_range=ifelse(!is.na(n) & n<5, NA, interquartile_range)) %>% 
    mutate(standard_deviation=ifelse(!is.na(n) & n<5, NA, standard_deviation)) %>% 
    mutate(n=ifelse(!is.na(n) & n<5, "<5", n))
  
  return(summary_characteristics %>% 
           relocate(any_of(c("var", "n", "percent",
                             "mean", "standard_deviation",
                             "median", "interquartile_range"))))
  
}

# get a list to put results into
table1Characteristics <- list()

#create a loop that puts table 1 for each outcome
for(j in seq_along(outcome_cohorts$cohort_definition_id)){
  

table1Characteristics[[j]] <- get_summary_characteristics(Pop %>% 
                                                              filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
  mutate(Cancer = outcome_cohorts$cohort_name[[j]])

table1Characteristics[[j]]$n <- as.character(table1Characteristics[[j]]$n)

print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 

}

#bind all results together
table1Characteristics_all <- bind_rows(table1Characteristics) %>%
  mutate(Database = db.name, analysis = "Incidence")

#save the results
write_csv(table1Characteristics_all, here::here(paste0("Results/",db.name,"/Table1",db.name,".csv")))
  

# PREVALENCE POPULATION
# same code but for prev population (already obtained values just need participants)

prev_period_participants <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominatordemo",
  outcomeCohortId = prevalent_cohorts$cohort_definition_id,
  outcomeCohortName = prevalent_cohorts$cohort_name,
  outcomeLookbackDays = 0,
  outcomeTable = prevalent_table_name,
  interval = "years" ,
  completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
  fullContribution = FALSE , # individuals only required to be present for one day in interval
  minCellCount = 5,
  returnParticipants = TRUE,
  tablePrefix = prevalent_table_name,
  verbose = TRUE
)


# the numbers are not the same as achieved in annual prev
settings_surv_prev <- settings(prev_period_participants)

popsprev <- list()

for (i in 1:length(settings_surv_prev$analysis_id)){
  #extract the participants for each cancer

  popsprev[[i]] <-cdm$person %>%
    inner_join(participants(prev_period_participants, analysisId = as.numeric(settings_surv_prev$analysis_id[i])) %>% filter(!is.na(outcome_start_date)),
               by = c("person_id" = "subject_id" ), copy = TRUE) %>%
    select(person_id,gender_concept_id,
           year_of_birth, month_of_birth, day_of_birth,
           cohort_start_date,
           cohort_end_date,
           outcome_start_date) %>%
    left_join(cdm$observation_period %>%
                select("person_id",  "observation_period_start_date", "observation_period_end_date") %>%
                distinct(),
              by = "person_id") %>%
    left_join(cdm$death %>%
                select("person_id",  "death_date") %>%
                distinct(),
              by = "person_id") %>%
    collect()

  popsprev[[i]] <- popsprev[[i]]  %>%
    mutate(outcome_cohort_name = settings_surv_prev$outcome_cohort_name[i]) %>%
    mutate(outcome_cohort_id = settings_surv_prev$outcome_cohort_id[i])

}

PopPrev <- dplyr::bind_rows(popsprev)

# format data -----
#add age -----
PopPrev$age<- NA
if(sum(is.na(PopPrev$day_of_birth))==0 & sum(is.na(PopPrev$month_of_birth))==0){
  # if we have day and month
  PopPrev <-PopPrev %>%
    mutate(age=floor(as.numeric((ymd(outcome_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else {
  PopPrev <- PopPrev %>%
    mutate(age= lubridate::year(outcome_start_date)-year_of_birth)
}

# # age age groups ----
PopPrev <- PopPrev %>%
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
table(PopPrev$age_gr, useNA = "always")

# # reformat gender
# # add gender -----
# #8507 male
# #8532 female
PopPrev <-PopPrev %>%
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>%
  mutate(gender= factor(gender,
                        levels = c("Male", "Female")))
table(PopPrev$gender, useNA = "always")

# # if missing (or unreasonable) age or gender, drop ----
PopPrev <-PopPrev %>%
  filter(!is.na(age)) %>%
  filter(age>=18) %>%
  filter(age<=110) %>%
  filter(!is.na(gender))
#
# # create sex:agegp categorical variables
PopPrev <- PopPrev %>%
  unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
  mutate(genderAgegp= factor(genderAgegp,
                             levels = c("Female_18-29","Female_30-39","Female_40-49", "Female_50-59",
                                        "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                        "Male_18-29","Male_30-39","Male_40-49", "Male_50-59",
                                        "Male_60-69", "Male_70-79","Male_80-89","Male_>=90")))

# # drop if missing observation period end date ----
PopPrev <-PopPrev %>%
  filter(!is.na(observation_period_end_date))

# Prior history from date of diagnosis from start of observation period
PopPrev <- PopPrev %>%
  mutate(Prior_history_days = as.numeric(outcome_start_date - observation_period_start_date ))

# Prior history from date of diagnosis (from 1 jan 2001)
PopPrev <- PopPrev %>%
  mutate(Prior_history_days_study_start = as.numeric(as.Date(cohort_start_date) - observation_period_start_date ))

# calculate Follow up - calculate end of observation period
PopPrev <- PopPrev %>%
  mutate(endOfObservation = ifelse(observation_period_end_date >= studyEndDate, studyEndDate, NA)) %>%
  mutate(endOfObservation = as.Date(endOfObservation) ) %>%
  mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date))

# calculate follow up in years and days
PopPrev <-PopPrev %>%
  mutate(time_days=as.numeric(difftime(endOfObservation,
                                       outcome_start_date,
                                       units="days"))) %>%
  mutate(time_years=time_days/365.25)

# binary death outcome (for survival) ---
# need to take into account follow up
# if death date is > database end data set death to 0
PopPrev <-PopPrev %>%
  mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
  mutate(status= ifelse(death_date > endOfObservation , 1, status )) %>%
  mutate(status= ifelse(is.na(status), 1, status )) %>%
  mutate(Death = recode(status,
                        "1" = "Alive",
                        "2" = "Dead"))

# medications (3 months before index date)
for(i in seq_along(medication_cohorts$cohort_name)){
  working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
  working_id <- medication_cohorts$cohort_definition_id[[i]]
  PopPrev <-
    PopPrev %>%
    left_join(
      PopPrev %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm[[feature_medication_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     rename("feature_end_date"="cohort_end_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!cohort_definition_id),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>%
        filter(
          # overlapping
          (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
             feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
            # ending in window
            (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
               feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_name:=1),
      by="person_id") %>%
    compute()
}

# conditions (other cancers)
for(i in seq_along(prevalent_cohorts$cohort_name)){
  working_name <- glue::glue("{prevalent_cohorts$cohort_name[[i]]}")
  working_id <- prevalent_cohorts$cohort_definition_id[[i]]
  PopPrev <-
    PopPrev %>%
    left_join(
      PopPrev %>%
        select("person_id", "cohort_start_date") %>%
        inner_join(cdm[[prevalent_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>%
        filter(feature_start_date < cohort_start_date) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_name:=1),
      by="person_id")  %>%
    compute()

}

# conditions (any time in history)
for(i in seq_along(disease_cohorts$cohort_definition_id)){

  working_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
  working_id <- disease_cohorts$cohort_definition_id[[i]]
  PopPrev <-
    PopPrev %>%
    left_join(
      PopPrev %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm[[feature_disease_table_name]] %>%
                     rename("feature_start_date"="cohort_start_date") %>%
                     filter(cohort_definition_id== working_id ) %>%
                     select(!c(cohort_definition_id,
                               cohort_end_date)),
                   by=c("person_id" = "subject_id"), copy = TRUE) %>%
        filter(feature_start_date < outcome_start_date) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_name:=1),
      by="person_id")  %>%
    compute()

}

# get a list to put results into
table1CharacteristicsPrev <- list()

get_summary_characteristics_prev<-function(data){
  
  summary_characteristics<- bind_rows(
    data %>% 
      count() %>% 
      mutate(var="N"),
    
    data %>% 
      summarise(mean=nice.num(mean(age)),
                standard_deviation = nice.num(sd(age)),
                median = nice.num(median(age)),
                interquartile_range=paste0(nice.num.count(quantile(age,probs=0.25)),  " to ",
                                           nice.num.count(quantile(age,probs=0.75)))) %>% 
      mutate(var="age"),
    
    data %>% 
      group_by(age_gr) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="age_gr") %>% 
      mutate(var=paste0("Age group: ", var)),
    
    data %>% 
      mutate(gender=factor(gender, levels=c("Male", "Female"))) %>% 
      group_by(gender) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="gender") %>% 
      mutate(var=paste0("Sex: ", var)),
    
    data %>% 
      mutate(Death=factor(Death, levels=c("Alive", "Dead"))) %>% 
      group_by(Death) %>% 
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%   
      rename("var"="Death") %>% 
      mutate(var=paste0("Death: ", var)),
    
    data %>% 
      summarise(mean=nice.num.count(mean(Prior_history_days)),
                standard_deviation = nice.num(sd(Prior_history_days)),
                median = nice.num(median(Prior_history_days)),
                interquartile_range=paste0(nice.num.count(quantile(Prior_history_days,probs=0.25)),  " to ",
                                           nice.num.count(quantile(Prior_history_days,probs=0.75)))) %>% 
      mutate(var="Prior_history_days"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((Prior_history_days/365.25))),
                standard_deviation = nice.num(sd((Prior_history_days/365.25))),
                median = nice.num(median((Prior_history_days/365.25))),
                interquartile_range=paste0(nice.num.count(quantile((Prior_history_days/365.25),probs=0.25)),  " to ",
                                           nice.num.count(quantile((Prior_history_days/365.25),probs=0.75)))) %>% 
      mutate(var="Prior_history_years"),
    
    data %>% 
      summarise(mean=nice.num.count(mean(Prior_history_days_study_start)),
                standard_deviation = nice.num(sd(Prior_history_days_study_start)),
                median = nice.num(median(Prior_history_days_study_start)),
                interquartile_range=paste0(nice.num.count(quantile(Prior_history_days_study_start,probs=0.25)),  " to ",
                                           nice.num.count(quantile(Prior_history_days_study_start,probs=0.75)))) %>% 
      mutate(var="Prior_history_days_study_start"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((Prior_history_days_study_start/365.25))),
                standard_deviation = nice.num(sd((Prior_history_days_study_start/365.25))),
                median = nice.num(median((Prior_history_days_study_start/365.25))),
                interquartile_range=paste0(nice.num.count(quantile((Prior_history_days_study_start/365.25),probs=0.25)),  " to ",
                                           nice.num.count(quantile((Prior_history_days_study_start/365.25),probs=0.75)))) %>% 
      mutate(var="Prior_history_years_start"),
    
    
    data %>% 
      summarise(mean=nice.num.count(mean(time_days)),
                standard_deviation = nice.num(sd(time_days)),
                median = nice.num(median(time_days)),
                interquartile_range=paste0(nice.num.count(quantile(time_days,probs=0.25)),  " to ",
                                           nice.num.count(quantile(time_days,probs=0.75)))) %>% 
      mutate(var="time_days"),
    
    data %>% 
      summarise(mean=nice.num.count(mean((time_years))),
                standard_deviation = nice.num(sd((time_years))),
                median = nice.num(median((time_years))),
                interquartile_range=paste0(nice.num.count(quantile((time_years),probs=0.25)),  " to ",
                                           nice.num.count(quantile((time_years),probs=0.75)))) %>% 
      mutate(var="time_years"))
  
  for(i in seq_along(disease_cohorts$cohort_name)){
    working_id_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_id_name)
    )
  }
  
  for(i in seq_along(prevalent_cohorts$cohort_name)){
    working_name <- glue::glue("{prevalent_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_name)
    )
  }
  
  for(i in seq_along(medication_cohorts$cohort_name)){
    working_id_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>% 
                                           summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
                                           mutate(var=working_id_name)
    )
  }
  
  # filter any less than 5
  summary_characteristics <- summary_characteristics %>% 
    mutate(mean=ifelse(!is.na(n) & n<5, NA, mean)) %>% 
    mutate(percent=ifelse(!is.na(n) & n<5, NA, percent)) %>% 
    mutate(interquartile_range=ifelse(!is.na(n) & n<5, NA, interquartile_range)) %>% 
    mutate(standard_deviation=ifelse(!is.na(n) & n<5, NA, standard_deviation)) %>% 
    mutate(n=ifelse(!is.na(n) & n<5, "<5", n))
  
  return(summary_characteristics %>% 
           relocate(any_of(c("var", "n", "percent",
                             "mean", "standard_deviation",
                             "median", "interquartile_range"))))
  
}

# #create a loop that puts table 1 for each outcome
for(j in seq_along(prevalent_cohorts$cohort_definition_id)){

  table1CharacteristicsPrev[[j]] <- get_summary_characteristics_prev(PopPrev %>%
                                                              filter(outcome_cohort_name==prevalent_cohorts$cohort_name[[j]])) %>%
    mutate(Cancer = prevalent_cohorts$cohort_name[[j]])

  table1CharacteristicsPrev[[j]]$n <- as.character(table1CharacteristicsPrev[[j]]$n)

  print(paste0("Getting table 1 for ", prevalent_cohorts$cohort_name[[j]], " (" , j , " of ", length(prevalent_cohorts$cohort_name), ")"))

}

#bind all results together
table1Characteristics_all_prev <- bind_rows(table1CharacteristicsPrev) %>%
  mutate(Database = db.name, analysis = "Prevalence")

# #save the results
write_csv(table1Characteristics_all_prev, here::here(paste0("Results/",db.name,"/Table1Prev",db.name,".csv")))


# function to extract dataset based on head and neck cancer subtypes
if (grepl("CPRD", db.name) == TRUE){
  
  # get the participants for overall
  inc_han_overall_participants <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominatordemo",
    outcomeTable = outcome_table_name_han,
    denominatorCohortId = NULL,
    outcomeCohortId = outcome_cohorts_han$cohort_definition_id,
    outcomeCohortName = outcome_cohorts_han$cohort_name,
    interval = c("overall"), 
    outcomeWashout = NULL,
    repeatedEvents = FALSE,
    completeDatabaseIntervals = TRUE,
    minCellCount = 5,
    returnParticipants = TRUE,
    tablePrefix = outcome_table_name_han,
    verbose = TRUE
  )
  
  settings_surv <- settings(inc_han_overall_participants)
  
  pops <- list()
  
  for (i in 1:length(settings_surv$analysis_id)){
    #extract the participants for each cancer
    pops[[i]] <-cdm$person %>%
      inner_join(participants(inc_han_overall_participants, analysisId = as.numeric(settings_surv$analysis_id[i])) %>% filter(!is.na(outcome_start_date)),
                 by = c("person_id" = "subject_id" ), copy = TRUE) %>%
      select(person_id,gender_concept_id,
             year_of_birth, month_of_birth, day_of_birth,
             cohort_start_date,
             cohort_end_date,
             outcome_start_date)  %>%
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
  
  Pophan <- dplyr::bind_rows(pops)
  
  # format data -----
  #add age -----
  Pophan$age<- NA
  if(sum(is.na(Pophan$day_of_birth))==0 & sum(is.na(Pophan$month_of_birth))==0){
    # if we have day and month
    Pophan <-Pophan %>%
      mutate(age=floor(as.numeric((ymd(outcome_start_date)-
                                     ymd(paste(year_of_birth,
                                               month_of_birth,
                                               day_of_birth, sep="-"))))/365.25))
  } else {
    Pophan <- Pophan %>%
      mutate(age= lubridate::year(outcome_start_date)-year_of_birth)
  }
  
  # # age age groups ----
  Pophan <- Pophan %>%
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
  table(Pophan$age_gr, useNA = "always")
  
  # # reformat gender
  # # add gender -----
  # #8507 male
  # #8532 female
  Pophan <-Pophan %>%
    mutate(gender= ifelse(gender_concept_id==8507, "Male",
                          ifelse(gender_concept_id==8532, "Female", NA ))) %>%
    mutate(gender= factor(gender,
                          levels = c("Male", "Female")))
  table(Pophan$gender, useNA = "always")
  
  # # if missing (or unreasonable) age or gender, drop ----
  Pophan <-Pophan %>%
    filter(!is.na(age)) %>%
    filter(age>=18) %>%
    filter(age<=110) %>%
    filter(!is.na(gender))
  #
  # # create sex:agegp categorical variables
  Pophan <- Pophan %>%
    unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
    mutate(genderAgegp= factor(genderAgegp,
                               levels = c("Female_18-29","Female_30-39","Female_40-49", "Female_50-59",
                                          "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                          "Male_18-29","Male_30-39","Male_40-49", "Male_50-59",
                                          "Male_60-69", "Male_70-79","Male_80-89","Male_>=90")))
  
  # # drop if missing observation period end date ----
  Pophan <-Pophan %>%
    filter(!is.na(observation_period_end_date)) 
  
  Pophan  <- Pophan %>% 
    mutate(Prior_history_days = as.numeric(outcome_start_date - observation_period_start_date ))
  
  # Prior history from date of diagnosis (from 1 jan 2001)
  Pophan  <- Pophan %>% 
    mutate(Prior_history_days_study_start = as.numeric(as.Date(cohort_start_date) - observation_period_start_date ))
  
  # calculate Follow up - calculate end of observation period
  Pophan <- Pophan %>%
    mutate(endOfObservation = ifelse(observation_period_end_date >= studyEndDate, studyEndDate, NA)) %>%
    mutate(endOfObservation = as.Date(endOfObservation) ) %>%
    mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date))
  
  # calculate follow up in years and days
  Pophan <-Pophan %>%
    mutate(time_days=as.numeric(difftime(endOfObservation,
                                         outcome_start_date,
                                         units="days"))) %>%
    mutate(time_years=time_days/365.25)
  
  # binary death outcome (for survival) ---
  # need to take into account follow up
  # if death date is > database end data set death to 0
  Pophan <- Pophan %>%
    mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
    mutate(status= ifelse(death_date > endOfObservation , 1, status )) %>%
    mutate(status= ifelse(is.na(status), 1, status )) %>%
    mutate(Death = recode(status, 
                          "1" = "Alive", 
                          "2" = "Dead"))
  
  # get the co morbidites and medication usage for subset of cohort ----
  # medications (3 months before index date)
  for(i in seq_along(medication_cohorts$cohort_definition_id)){
    working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
    working_id <- medication_cohorts$cohort_definition_id[[i]]
    Pophan <-
      Pophan %>%
      left_join(
        Pophan %>%
          select("person_id", "outcome_start_date") %>% 
          inner_join(cdm[[feature_medication_table_name]] %>%
                       rename("feature_start_date"="cohort_start_date") %>%
                       rename("feature_end_date"="cohort_end_date") %>%
                       filter(cohort_definition_id== working_id ) %>%
                       select(!cohort_definition_id),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>% 
          filter(
            # overlapping
            (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
               feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
              # ending in window
              (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
                 feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
          select(person_id) %>%
          distinct() %>%
          mutate(!!working_name:=1),
        by="person_id") %>%
      compute()
  }
  
  # conditions (other cancers)
  for(i in seq_along(outcome_cohorts$cohort_name)){
    working_name <- glue::glue("{outcome_cohorts$cohort_name[[i]]}")
    working_id <- outcome_cohorts$cohort_definition_id[[i]]
    Pophan <- 
      Pophan %>% 
      left_join(
        Pophan %>% 
          select("person_id", "cohort_start_date") %>% 
          inner_join(cdm[[outcome_table_name]] %>% 
                       rename("feature_start_date"="cohort_start_date") %>% 
                       filter(cohort_definition_id== working_id ) %>% 
                       select(!c(cohort_definition_id,
                                 cohort_end_date)),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>% 
          filter(feature_start_date < cohort_start_date) %>% 
          select(person_id) %>% 
          distinct() %>% 
          mutate(!!working_name:=1),
        by="person_id")  %>% 
      compute()
    
  }
  
  # conditions (any time in history)
  for(i in seq_along(disease_cohorts$cohort_definition_id)){
    
    working_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
    working_id <- disease_cohorts$cohort_definition_id[[i]]
    Pophan <- 
      Pophan %>% 
      left_join(
        Pophan %>% 
          select("person_id", "outcome_start_date") %>% 
          inner_join(cdm[[feature_disease_table_name]] %>% 
                       rename("feature_start_date"="cohort_start_date") %>% 
                       filter(cohort_definition_id== working_id ) %>% 
                       select(!c(cohort_definition_id,
                                 cohort_end_date)),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>% 
          filter(feature_start_date < outcome_start_date) %>% 
          select(person_id) %>% 
          distinct() %>% 
          mutate(!!working_name:=1),
        by="person_id")  %>% 
      compute()
    
  }

  
  # tidy up results for table 1
  # get a list to put results into
  table1Characteristics_han <- list()
  
  #create a loop that puts table 1 for each outcome
  for(j in seq_along(outcome_cohorts_han$cohort_definition_id)){
    
    table1Characteristics_han[[j]] <- get_summary_characteristics(Pophan %>% 
                                                                filter(outcome_cohort_name==outcome_cohorts_han$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts_han$cohort_name[[j]])
    
    table1Characteristics_han[[j]]$n <- as.character(table1Characteristics_han[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts_han$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts_han$cohort_name), ")")) 
    
  }
  
  #bind all results together
  table1Characteristics_all_han <- bind_rows(table1Characteristics_han) %>%
    mutate(Database = db.name, analysis = "Incidence")
  
  #save the results
  write_csv(table1Characteristics_all_han, here::here(paste0("Results/",db.name,"/Table1HanMed",db.name,".csv")))

# PREVALENCE FOR HAN
  prev_period_han_participants <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominatordemo",
    outcomeCohortId = prevalent_cohorts_han$cohort_definition_id,
    outcomeCohortName = prevalent_cohorts_han$cohort_name,
    outcomeLookbackDays = 0,
    outcomeTable = prevalent_table_name_han,
    interval = "years" ,
    completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
    fullContribution = FALSE , # individuals only required to be present for one day in interval
    minCellCount = 5,
    returnParticipants = TRUE,
    tablePrefix = prevalent_table_name_han,
    verbose = TRUE
  )
  
  # the numbers are not the same as achieved in annual prev
  settings_surv_prev <- settings(prev_period_han_participants)
  
  popsprev <- list()
  
  for (i in 1:length(settings_surv_prev$analysis_id)){
    #extract the participants for each cancer
    
    popsprev[[i]] <-cdm$person %>%
      inner_join(participants(prev_period_han_participants, analysisId = as.numeric(settings_surv_prev$analysis_id[i])) %>% filter(!is.na(outcome_start_date)),
                 by = c("person_id" = "subject_id" ), copy = TRUE) %>%
      select(person_id,gender_concept_id,
             year_of_birth, month_of_birth, day_of_birth,
             cohort_start_date,
             cohort_end_date,
             outcome_start_date) %>%
      left_join(cdm$observation_period %>%
                  select("person_id",  "observation_period_start_date", "observation_period_end_date") %>%
                  distinct(),
                by = "person_id") %>%
      left_join(cdm$death %>%
                  select("person_id",  "death_date") %>%
                  distinct(),
                by = "person_id") %>%
      collect()
    
    popsprev[[i]] <- popsprev[[i]]  %>%
      mutate(outcome_cohort_name = settings_surv_prev$outcome_cohort_name[i]) %>%
      mutate(outcome_cohort_id = settings_surv_prev$outcome_cohort_id[i])
    
  }
  
  PopPrevHan <- dplyr::bind_rows(popsprev)
  
  # format data -----
  #add age -----
  PopPrevHan$age<- NA
  if(sum(is.na(PopPrevHan$day_of_birth))==0 & sum(is.na(PopPrevHan$month_of_birth))==0){
    # if we have day and month
    PopPrevHan <-PopPrevHan %>%
      mutate(age=floor(as.numeric((ymd(outcome_start_date)-
                                     ymd(paste(year_of_birth,
                                               month_of_birth,
                                               day_of_birth, sep="-"))))/365.25))
  } else {
    PopPrevHan <- PopPrevHan %>%
      mutate(age= lubridate::year(outcome_start_date)-year_of_birth)
  }
  
  # # age age groups ----
  PopPrevHan <- PopPrevHan %>%
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
  table(PopPrevHan$age_gr, useNA = "always")
  
  # # reformat gender
  # # add gender -----
  # #8507 male
  # #8532 female
  PopPrevHan <-PopPrevHan %>%
    mutate(gender= ifelse(gender_concept_id==8507, "Male",
                          ifelse(gender_concept_id==8532, "Female", NA ))) %>%
    mutate(gender= factor(gender,
                          levels = c("Male", "Female")))
  table(PopPrevHan$gender, useNA = "always")
  
  # # if missing (or unreasonable) age or gender, drop ----
  PopPrevHan <-PopPrevHan %>%
    filter(!is.na(age)) %>%
    filter(age>=18) %>%
    filter(age<=110) %>%
    filter(!is.na(gender))
  #
  # # create sex:agegp categorical variables
  PopPrevHan <- PopPrevHan %>%
    unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
    mutate(genderAgegp= factor(genderAgegp,
                               levels = c("Female_18-29","Female_30-39","Female_40-49", "Female_50-59",
                                          "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                          "Male_18-29","Male_30-39","Male_40-49", "Male_50-59",
                                          "Male_60-69", "Male_70-79","Male_80-89","Male_>=90")))
  
  # # drop if missing observation period end date ----
  PopPrevHan <-PopPrevHan %>%
    filter(!is.na(observation_period_end_date))
  
  # Prior history from date of diagnosis from start of observation period
  PopPrevHan <- PopPrevHan %>%
    mutate(Prior_history_days = as.numeric(outcome_start_date - observation_period_start_date ))
  
  # Prior history from date of diagnosis (from 1 jan 2001)
  PopPrevHan <- PopPrevHan %>%
    mutate(Prior_history_days_study_start = as.numeric(as.Date(cohort_start_date) - observation_period_start_date ))
  
  # calculate Follow up - calculate end of observation period
  PopPrevHan <- PopPrevHan %>%
    mutate(endOfObservation = ifelse(observation_period_end_date >= studyEndDate, studyEndDate, NA)) %>%
    mutate(endOfObservation = as.Date(endOfObservation) ) %>%
    mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date))
  
  # calculate follow up in years and days
  PopPrevHan <-PopPrevHan %>%
    mutate(time_days=as.numeric(difftime(endOfObservation,
                                         outcome_start_date,
                                         units="days"))) %>%
    mutate(time_years=time_days/365.25)
  
  # binary death outcome (for survival) ---
  # need to take into account follow up
  # if death date is > database end data set death to 0
  PopPrevHan <-PopPrevHan %>%
    mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
    mutate(status= ifelse(death_date > endOfObservation , 1, status )) %>%
    mutate(status= ifelse(is.na(status), 1, status )) %>%
    mutate(Death = recode(status,
                          "1" = "Alive",
                          "2" = "Dead"))
  
  # medications (3 months before index date)
  for(i in seq_along(medication_cohorts$cohort_name)){
    working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
    working_id <- medication_cohorts$cohort_definition_id[[i]]
    PopPrevHan <-
      PopPrevHan %>%
      left_join(
        PopPrevHan %>%
          select("person_id", "outcome_start_date") %>%
          inner_join(cdm[[feature_medication_table_name]] %>%
                       rename("feature_start_date"="cohort_start_date") %>%
                       rename("feature_end_date"="cohort_end_date") %>%
                       filter(cohort_definition_id== working_id ) %>%
                       select(!cohort_definition_id),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>%
          filter(
            # overlapping
            (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
               feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
              # ending in window
              (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
                 feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
          select(person_id) %>%
          distinct() %>%
          mutate(!!working_name:=1),
        by="person_id") %>%
      compute()
  }
  
  # conditions (other cancers)
  for(i in seq_along(prevalent_cohorts$cohort_name)){
    working_name <- glue::glue("{prevalent_cohorts$cohort_name[[i]]}")
    working_id <- prevalent_cohorts$cohort_definition_id[[i]]
    PopPrevHan <-
      PopPrevHan %>%
      left_join(
        PopPrevHan %>%
          select("person_id", "cohort_start_date") %>%
          inner_join(cdm[[prevalent_table_name]] %>%
                       rename("feature_start_date"="cohort_start_date") %>%
                       filter(cohort_definition_id== working_id ) %>%
                       select(!c(cohort_definition_id,
                                 cohort_end_date)),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>%
          filter(feature_start_date < cohort_start_date) %>%
          select(person_id) %>%
          distinct() %>%
          mutate(!!working_name:=1),
        by="person_id")  %>%
      compute()
    
  }
  
  # conditions (any time in history)
  for(i in seq_along(disease_cohorts$cohort_definition_id)){
    
    working_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
    working_id <- disease_cohorts$cohort_definition_id[[i]]
    PopPrevHan <-
      PopPrevHan %>%
      left_join(
        PopPrevHan %>%
          select("person_id", "outcome_start_date") %>%
          inner_join(cdm[[feature_disease_table_name]] %>%
                       rename("feature_start_date"="cohort_start_date") %>%
                       filter(cohort_definition_id== working_id ) %>%
                       select(!c(cohort_definition_id,
                                 cohort_end_date)),
                     by=c("person_id" = "subject_id"), copy = TRUE) %>%
          filter(feature_start_date < outcome_start_date) %>%
          select(person_id) %>%
          distinct() %>%
          mutate(!!working_name:=1),
        by="person_id")  %>%
      compute()
    
  }
  
  # get a list to put results into
  table1CharacteristicsPrev <- list()
  
  # #create a loop that puts table 1 for each outcome
  for(j in seq_along(prevalent_cohorts_han$cohort_definition_id)){
    
    table1CharacteristicsPrev[[j]] <- get_summary_characteristics_prev(PopPrevHan %>%
                                                                         filter(outcome_cohort_name==prevalent_cohorts_han$cohort_name[[j]])) %>%
      mutate(Cancer = prevalent_cohorts_han$cohort_name[[j]])
    
    table1CharacteristicsPrev[[j]]$n <- as.character(table1CharacteristicsPrev[[j]]$n)
    
    print(paste0("Getting table 1 for ", prevalent_cohorts_han$cohort_name[[j]], " (" , j , " of ", length(prevalent_cohorts$cohort_name), ")"))
    
  }
  
  #bind all results together
  table1Characteristics_han_prev <- bind_rows(table1CharacteristicsPrev) %>%
    mutate(Database = db.name, analysis = "Prevalence")
  
  # #save the results
  write_csv(table1Characteristics_han_prev, here::here(paste0("Results/",db.name,"/Table1HanPrev",db.name,".csv")))
  
  
}
  
Sys.time()-start
