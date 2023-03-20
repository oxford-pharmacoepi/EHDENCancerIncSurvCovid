# cohort characteristics

# This code extracts the participants from the incidence analysis overall and calculates their baseline characteristics (table 1 for papers)
# This code also generates the data that is inputted into the survival analysis

print(paste0("- Getting cohort characteristics: cancer populations"))
info(logger, "- Getting cohort characteristics: cancer populations")

# get denominator
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


# get the co morbidites and medication usage
# read in table containing list to create
table1features <- read_csv(
  here::here(
    "1_InstantiateCohorts",
    "Table1Features.csv"))

# split into drugs and conditions
table1features_drugs <- table1features %>% filter(table1features$Class == "Drug")

table1features_conditions <- table1features %>% filter(table1features$Class == "Condition")


# conditions
for(i in seq_along(table1features_conditions$Name)){
  
  working_id_name <- glue::glue("{table1features_conditions$Name[[i]]}")
  working_concept_id <- table1features_conditions$Concept_ID[i]
  
  #get the feature and its descendants
  feature.codes<-tbl(db, sql("SELECT * FROM concept_ancestor")) %>% 
    filter(ancestor_concept_id == working_concept_id) %>% 
    collect()
  
  Pop <- 
    Pop %>% 
    left_join(Pop %>% 
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm$condition_occurrence %>% 
                  select("person_id","condition_concept_id", "condition_start_date") %>%
                  filter(condition_concept_id %in% !!feature.codes$descendant_concept_id),
                  by=c("person_id"), copy = TRUE) %>% 
        filter(condition_start_date < outcome_start_date) %>% 
        select(person_id) %>% 
        distinct() %>% 
        mutate(!!working_id_name:=1),
      by="person_id")  %>% 
    compute()
  
  print(paste0("Getting features for ", table1features_conditions$Description[i], " (" , i , " of ", length(table1features_conditions$Name), ")")) 
  
}

# medications

# medications
for(i in seq_along(table1features_drugs$Name)){
  
  working_id_name <- glue::glue("{table1features_drugs$Name[[i]]}")
  working_concept_id <- table1features_drugs$Concept_ID[i]
  
  #get the feature and its descendants
  feature.codes<-tbl(db, sql("SELECT * FROM concept_ancestor")) %>% 
    filter(ancestor_concept_id == working_concept_id) %>% 
    collect()
  
  Pop <-
    Pop %>%
    left_join(
      Pop %>%
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm$drug_exposure %>% 
                     select("person_id","drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date") %>%
                     filter(drug_concept_id %in% !!feature.codes$descendant_concept_id),
                   by=c("person_id"), copy = TRUE) %>% 
        filter(
          # overlapping
          (drug_exposure_start_date <= (outcome_start_date-lubridate::days(-1)) &
             drug_exposure_end_date >= (outcome_start_date-lubridate::days(-1))) |
            # ending in window
            (drug_exposure_start_date >= (outcome_start_date-lubridate::days(90)) &
               drug_exposure_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
        select(person_id) %>%
        distinct() %>%
        mutate(!!working_id_name:=1),
      by="person_id") %>%
    compute()
  
  print(paste0("Getting features for ", table1features_drugs$Description[i], " (" , i , " of ", length(table1features_drugs$Name), ")")) 
  
}



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
  
  
  
}
  
  




# co morbidities

# # this works
# Dementia.codes<-tbl(db, sql("SELECT * FROM concept_ancestor")) %>% 
#   filter(ancestor_concept_id ==4182210) %>% 
#   collect()
# 
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>% 
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$condition_occurrence %>% 
#                    filter(condition_concept_id %in% !!Dementia.codes$descendant_concept_id), 
#                  by=c("person_id"), copy = TRUE) %>% 
#       filter(condition_start_date <= outcome_start_date) %>% 
#       select(person_id) %>% 
#       distinct() %>% 
#       mutate(Dementia=1),
#     by="person_id")  %>% 
#   compute()
# 
# 
# 
# 
# 
# 
# 
# 
# # medications
# 
# 
# 
# 
# 
# 
# 
# 
# # medications
# for(i in seq_along(medication_cohorts$cohortId)){
#   working_name <- glue::glue("{medication_cohorts$cohortName[[i]]}")
#   working_id <- medication_cohorts$cohortId[[i]]
#   Pop <-
#     Pop %>%
#     left_join(
#       Pop %>%
#         select("person_id", "outcome_start_date") %>% 
#         inner_join(cdm[[feature_medication_table_name]] %>%
#                      rename("feature_start_date"="cohort_start_date") %>%
#                      rename("feature_end_date"="cohort_end_date") %>%
#                      filter(cohort_definition_id== working_id ) %>%
#                      select(!cohort_definition_id),
#                    by=c("person_id" = "subject_id"), copy = TRUE) %>% 
#         filter(
#           # overlapping
#           (feature_start_date <= (outcome_start_date-days(-1)) &
#              feature_end_date >= (outcome_start_date-days(-1))) |
#             # ending in window
#             (feature_end_date >= (outcome_start_date-days(180)) &
#                feature_end_date <= (outcome_start_date-days(-1)))) %>%
#         select(person_id) %>%
#         distinct() %>%
#         mutate(!!working_name:=1),
#       by="person_id") %>%
#     compute()
# }
# 
# 
# # conditions
# 
# for(i in seq_along(disease_cohorts$cohortId)){
#   
#   working_name <- glue::glue("{disease_cohorts$cohortName[[i]]}")
#   working_id <- disease_cohorts$cohortId[[i]]
#   Pop <- 
#     Pop %>% 
#     left_join(
#       Pop %>% 
#         select("person_id", "outcome_start_date") %>% 
#         inner_join(cdm[[feature_disease_table_name]] %>% 
#                      rename("feature_start_date"="cohort_start_date") %>% 
#                      filter(cohort_definition_id== working_id ) %>% 
#                      select(!c(cohort_definition_id,
#                                cohort_end_date)),
#                    by=c("person_id" = "subject_id"), copy = TRUE) %>% 
#         filter(feature_start_date < outcome_start_date) %>% 
#         select(person_id) %>% 
#         distinct() %>% 
#         mutate(!!working_name:=1),
#       by="person_id")  %>% 
#     compute()
#   
# }
# 
# 
# # Non smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>%
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$observation %>%
#                    filter( observation_concept_id == 4222303 ) , 
#                  by=c("person_id"), copy = TRUE)  %>% 
#       filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#       filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#       select(c(person_id, observation_date, outcome_start_date)) %>% 
#       distinct() %>%
#       group_by(person_id, outcome_start_date) %>%
#       filter(observation_date == max(observation_date)) %>%
#       rename("non_smoker_date"="observation_date"),
#     by= c("person_id", "outcome_start_date")) %>% 
#   compute()

# # Smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>%
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$observation %>%
#                    filter( 
#                      observation_concept_id == 4141787 | # Smoking started
#                        observation_concept_id == 44789712 | # want to stop smoking
#                        observation_concept_id == 40486518 | # failed attempt to stop smoking
#                        observation_concept_id == 	4046886 | # smoking reduced 
#                        observation_concept_id == 4058137 | # tried giving up smoking
#                        observation_concept_id == 4216174 | # not interested in giving up smoking
#                        observation_concept_id == 4215409 | # Ready to stop smoking
#                        observation_concept_id == 4190573 | # Thinking about stopping smoking
#                        observation_concept_id == 4052948 | # Keeps trying to stop smoking
#                        observation_concept_id ==  4144271 | # Tobacco smoking consumption
#                        observation_concept_id == 4298794
#                    ) , 
#                  by=c("person_id"), copy = TRUE)  %>% 
#       filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#       filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#       select(c(person_id, observation_date, outcome_start_date)) %>% 
#       distinct() %>%
#       group_by(person_id, outcome_start_date) %>%
#       filter(observation_date == max(observation_date)) %>%
#       rename("Smoker_date"="observation_date"),
#     by= c("person_id", "outcome_start_date")) %>% 
#   compute()
# 
# # previous smokers
# Pop <- 
#   Pop %>% 
#   left_join(
#     Pop %>%
#       select("person_id", "outcome_start_date") %>% 
#       inner_join(cdm$observation %>%
#                    filter( 
#                      observation_concept_id == 4052032 | # stopped smoking
#                        observation_concept_id == 4052466 # date ceased smoking
#                    ) , 
#                  by=c("person_id"), copy = TRUE)  %>% 
#       filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
#       filter(observation_date > outcome_start_date - days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
#       select(c(person_id, observation_date, outcome_start_date)) %>% 
#       distinct() %>%
#       group_by(person_id, outcome_start_date) %>%
#       filter(observation_date == max(observation_date)) %>%
#       rename("PrevSmoker_date"="observation_date"),
#     by= c("person_id", "outcome_start_date")) %>% 
#   compute()
# 
# 
# # remove smokers who are now previous smokers
# Pop <- Pop %>%
#   mutate(Smoker_date = replace(Smoker_date, Smoker_date < PrevSmoker_date, NA))
# 
# # replace the dates with text and remove the dates
# Pop <- Pop %>%
#   mutate(SmokingStatus = NA,
#          SmokingStatus = replace(SmokingStatus, !is.na(Smoker_date), "Smoker"),
#          SmokingStatus = replace(SmokingStatus, !is.na(PrevSmoker_date), "Previous Smoker"),
#          SmokingStatus = replace(SmokingStatus, !is.na(non_smoker_date), "Non Smoker")) %>%
#   select(!c(Smoker_date, PrevSmoker_date, non_smoker_date))


# things to add:
#   
# Years of prior observation time
# co morbidities
# medication use
