# cohort characteristics

# This code extracts the participants from the incidence analysis overall and calculates their baseline characteristics (table 1 for papers)
# This code also generates the data that is inputted into the survival analysis

# INCIDENCE POPULATION
print(paste0("- Getting cohort characteristics: cancer populations"))
info(logger, "- Getting cohort characteristics: cancer populations")

# get denominator to get participants
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominatordemo" ,
  cohortDateRange = as.Date(c(studyStartDate,studyEndDate)),
  requirementInteractions = TRUE,
  ageGroup =list(
    c(18, 150)),
  sex = c("Both"),
  daysPriorObservation = 365,
  overwrite = TRUE
)

# get overall incidence for all cancers
inc_overall_participants <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominatordemo",
  outcomeTable = incidence_table_name,
  denominatorCohortId = NULL,
  interval = "overall",
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 5,
  temporary = FALSE,
  returnParticipants = TRUE
)

cancer_participants <- list()

# get participants from incidence for table one and survival
for (i in 1:length(outcome_cohorts$cohort_definition_id)){

#extract settings for survival from incidence results
cancer_participants[[i]] <- participants(result = inc_overall_participants, analysisId = i) %>% 
  collect() %>% 
  mutate(cohort_definition_id = i) %>% 
  filter(!(is.na(outcome_start_date)))

}

cancer_table_one <- bind_rows(cancer_participants)

# 
# #https://darwin-eu.github.io/CDMConnector/articles/a02_Working_with_cohorts.html
# DBI::dbWriteTable(con, inSchema("results", "cancer_table_one", 
#                                 dbms = dbms(con)), 
#                   value = cancer_table_one, 
#                   overwrite = TRUE)
# 
# cdm$cancer_table_one <- tbl(con, inSchema("results", "cancer_table_one", dbms = dbms(con))) 
# 
# 
# cdm$cancer_table_one <- new_generated_cohort_set(cdm$cancer_table_one)


# cdm$cancer_table_one <- cdm$incidence %>% 
#   filter(subject_id %in% cancer_table_one$subject_id) %>% 
#   # this section uses patient profiles to add in age and age groups as well as
#   # sex and prior history
#   addDemographics(
#     age = TRUE,
#     ageName = "age",
#     ageGroup =  list(
#       "age_gr" =
#         list(
#           "18 to 39" = c(18, 39),
#           "40 to 49" = c(40, 49),
#           "50 to 59" = c(50, 59),
#           "60 to 69" = c(60, 69),
#           "70 to 79" = c(70, 79),
#           "80 +" = c(80, 150)
#         )
#     )
#   )

cdm$working_participants <- participants(result = inc_overall_participants, i) %>% 
  filter(!(is.na(outcome_start_date)))

# drugs_names <- IncTxBreast_overall$outcome_cohort_name
# characteristics <- list()
# 
# for(i in seq_along(IncTxBreast_overall$outcome_cohort_id) ){
#   cdm$working_participants <- participants(result = IncTxBreast_overall, i) %>% #participants(IncTxBreast_overall, i) %>% 
#     select("subject_id", "outcome_start_date") %>% 
#     filter(!is.na(outcome_start_date)) %>% 
#     rename("cohort_start_date" = "outcome_start_date")
#   cdm$working_participants <- cdm$working_participants %>% 
#     addDemographics(cdm) %>% 
#     addCohortIntersectFlag(cdm = cdm,
#                            targetCohortTable = feature_disease_table_name_1,
#                            window = c(-Inf, 0), 
#                            nameStyle = "{cohort_name}") %>% 
#     addCohortIntersectFlag(cdm = cdm,
#                            targetCohortTable = feature_medication_table_name_1,
#                            window = c(-90, 0), 
#                            nameStyle = "{cohort_name}") 
#   
# }


inc_attr <- incidenceAttrition(inc_overall_participants)

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








#instantiate feature disease and medication cohorts

info(logger, "INSTANTIATE MEDICATIONS")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistMedications, 
                                              name = "medications",
                                              overwrite = TRUE)

info(logger, "INSTANTIATED MEDICATIONS")

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

cdm <- generateConceptCohortSet(cdm = cdm, 
                                conceptSet = codelistConditions,
                                name = "conditions",
                                overwrite = TRUE)

info(logger, "INSTANTIATED CONDITIONS")



















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

# Prior history from date of diagnosis
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

# add in smoking status # former, non or smoker in anytime prior history
# If smoking status was not recorded in year, the methods of last observation carried forward (LOCF)
# Records of non-smoking after those for current or previous smoking were amended to former-smoking. 
# As it was not possible to ascertain smoking status prior to CPRD records, we refer to non- rather than never-smoking.

# non smoking 5 years
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter( observation_concept_id == 4222303 |
                             observation_concept_id ==  4144272 
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("non_smoker_date5y"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()

# non smoking 10 years
Pop <-
  Pop %>%
  left_join(
 Pop %>%
  select("person_id", "outcome_start_date") %>%
  inner_join(cdm$observation %>%
               filter( observation_concept_id == 4222303 |
                         observation_concept_id ==  4144272 
                         ) ,
             by=c("person_id"), copy = TRUE)  %>%
  filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
  filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
  select(c(person_id, observation_date, outcome_start_date)) %>%
  distinct() %>%
  group_by(person_id, outcome_start_date) %>%
  filter(observation_date == max(observation_date)) %>%
  rename("non_smoker_date10y"="observation_date"),
 by= c("person_id", "outcome_start_date")) %>%
  compute()


# current non smoking 5 yr
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(observation_concept_id == 4052464
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("current_non_smoker_date5yr"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()

# current non smoking 10 yr
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(observation_concept_id == 4052464
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("current_non_smoker_date10yr"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()

# Smoker any time 5 yr
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(
                     observation_concept_id == 4005823 |
                       observation_concept_id == 4298794 | 
                       observation_concept_id == 762499 |
                       observation_concept_id == 762498 |
                       observation_concept_id == 37395605 |
                       observation_concept_id == 4246415 |
                       observation_concept_id ==  4218917 |
                       observation_concept_id == 4209006 |
                       observation_concept_id ==  4276526 |
                       observation_concept_id == 4209585 |
                       observation_concept_id == 764104 |
                       observation_concept_id == 764103 |
                       observation_concept_id == 4058138 |
                       observation_concept_id == 4042037 |
                       observation_concept_id == 4044777 |
                       observation_concept_id == 4044776 |
                       observation_concept_id == 4044775 |
                       observation_concept_id == 4041511 |
                       observation_concept_id == 4052029 |
                       observation_concept_id == 4058136 |
                       observation_concept_id == 4044778 |
                       observation_concept_id == 4052030 |
                       observation_concept_id == 4144273 |
                       observation_concept_id == 4052947 |
                       observation_concept_id == 4209006 |
                       observation_concept_id == 4204653 |
                       observation_concept_id == 44789712 |
                       observation_concept_id == 40486518 |
                       observation_concept_id ==4046886 |
                       observation_concept_id == 4058137 |
                       observation_concept_id == 4216174 |
                       observation_concept_id == 4215409 |
                       observation_concept_id == 4190573 |
                       observation_concept_id == 4052948 
                     ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("Smoker_date_5yr"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()


# Smokers current or code within 10 years
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(
                     observation_concept_id == 4005823 |
                       observation_concept_id == 4298794 | 
                       observation_concept_id == 762499 |
                       observation_concept_id == 762498 |
                       observation_concept_id == 37395605 |
                       observation_concept_id == 4246415 |
                       observation_concept_id ==  4218917 |
                       observation_concept_id == 4209006 |
                       observation_concept_id ==  4276526 |
                       observation_concept_id == 4209585 |
                       observation_concept_id == 764104 |
                       observation_concept_id == 764103 |
                       observation_concept_id == 4058138 |
                       observation_concept_id == 4042037 |
                       observation_concept_id == 4044777 |
                       observation_concept_id == 4044776 |
                       observation_concept_id == 4044775 |
                       observation_concept_id == 4041511 |
                       observation_concept_id == 4052029 |
                       observation_concept_id == 4058136 |
                       observation_concept_id == 4044778 |
                       observation_concept_id == 4052030 |
                       observation_concept_id == 4144273 |
                       observation_concept_id == 4052947 |
                       observation_concept_id == 4209006 |
                       observation_concept_id == 4204653 |
                       observation_concept_id == 44789712 |
                       observation_concept_id == 40486518 |
                       observation_concept_id ==4046886 |
                       observation_concept_id == 4058137 |
                       observation_concept_id == 4216174 |
                       observation_concept_id == 4215409 |
                       observation_concept_id == 4190573 |
                       observation_concept_id == 4052948 
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("Smoker_date_10yr"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()

# former smokers 5 year
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(
                     observation_concept_id == 4052032 | # stopped smoking
                       observation_concept_id == 44802805 # recently stopped smoking
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("PrevSmoker_date5y"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()


# former smokers 10 year
Pop <-
  Pop %>%
  left_join(
    Pop %>%
      select("person_id", "outcome_start_date") %>%
      inner_join(cdm$observation %>%
                   filter(
                     observation_concept_id == 4052032 | # stopped smoking
                       observation_concept_id == 44802805 # recently stopped smoking
                   ) ,
                 by=c("person_id"), copy = TRUE)  %>%
      filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
      filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
      select(c(person_id, observation_date, outcome_start_date)) %>%
      distinct() %>%
      group_by(person_id, outcome_start_date) %>%
      filter(observation_date == max(observation_date)) %>%
      rename("PrevSmoker_date10y"="observation_date"),
    by= c("person_id", "outcome_start_date")) %>%
  compute()




# rules for updating smoking status
# if only smoking codes == SMOKER, non smoker == NON SMOKER
# Never-smokers were re-assigned as former-smokers if there were contradictory preceding smoking codes
# previous smokers were assigned smokers if date of smoking was after previous smoking code 

# Smoking status using 5 years worth of previous data
# create a new variable for previous and if those who say non smoking convert to former/previous smoker
Pop <- Pop %>% mutate(PrevSmoker_date5y1 = ifelse(!is.na(PrevSmoker_date5y), non_smoker_date5y, PrevSmoker_date5y))
Pop <- Pop %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, !is.na(PrevSmoker_date5y1), "2Former"))
Pop <- Pop %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, !is.na(PrevSmoker_date5y), "2Former"))

# remove previous entries if the have a smoking date after previous
Pop <- Pop %>% mutate(Smokernow = Smoker_date_5yr > PrevSmoker_date5y )
Pop <- Pop %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, Smokernow == TRUE, NA))

# convert smoking into a character variable
Pop <- Pop %>% mutate(Smoker_date_5yr1 = as.character(Smoker_date_5yr))
Pop <- Pop %>% mutate(Smoker_date_5yr1 = replace(Smoker_date_5yr1, !is.na(Smoker_date_5yr1), "3Smoker"))

# convert non smokers into non smoker
Pop <- Pop %>% mutate(non_smoker_date5y1 = as.character(non_smoker_date5y))
Pop <- Pop %>% mutate(non_smoker_date5y1 = replace(non_smoker_date5y1, !is.na(non_smoker_date5y1), "1Non Smoker"))

Pop <- Pop %>% mutate(current_non_smoker_date5yr1 = as.character(current_non_smoker_date5yr))
Pop <- Pop %>% mutate(current_non_smoker_date5yr1 = replace(current_non_smoker_date5yr1, !is.na(current_non_smoker_date5yr1), "1Non Smoker"))

# merge all to get smoking status in past 5 years
Pop <- Pop %>% mutate(smoking_status5yr = coalesce(PrevSmoker_date5y1, Smoker_date_5yr1))
Pop <- Pop %>% mutate(smoking_status5yr = coalesce(smoking_status5yr, non_smoker_date5y1))
Pop <- Pop %>% mutate(smoking_status5yr = coalesce(smoking_status5yr, current_non_smoker_date5yr1))
# replace those with missing observations with "Missing"
Pop <- Pop %>% mutate(smoking_status5yr = replace(smoking_status5yr, is.na(smoking_status5yr), "4Missing"))



# Smoking status 10 years previous
Pop <- Pop %>% mutate(PrevSmoker_date10y1 = ifelse(!is.na(PrevSmoker_date10y), non_smoker_date10y, PrevSmoker_date10y))
Pop <- Pop %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, !is.na(PrevSmoker_date10y1), "2Former"))
Pop <- Pop %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, !is.na(PrevSmoker_date10y), "2Former"))

# remove previous entries if the have a smoking date after previous
Pop <- Pop %>% mutate(Smokernow = Smoker_date_10yr > PrevSmoker_date10y )
Pop <- Pop %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, Smokernow == TRUE, NA))

# convert smoking into a character variable
Pop <- Pop %>% mutate(Smoker_date_10yr1 = as.character(Smoker_date_10yr))
Pop <- Pop %>% mutate(Smoker_date_10yr1 = replace(Smoker_date_10yr1, !is.na(Smoker_date_10yr1), "3Smoker"))

# convert non smokers into non smoker
Pop <- Pop %>% mutate(non_smoker_date10y1 = as.character(non_smoker_date10y))
Pop <- Pop %>% mutate(non_smoker_date10y1 = replace(non_smoker_date10y1, !is.na(non_smoker_date10y1), "1Non Smoker"))

Pop <- Pop %>% mutate(current_non_smoker_date10yr1 = as.character(current_non_smoker_date10yr))
Pop <- Pop %>% mutate(current_non_smoker_date10yr1 = replace(current_non_smoker_date10yr1, !is.na(current_non_smoker_date10yr1), "1Non Smoker"))

# merge all to get smoking status in past 10 years
Pop <- Pop %>% mutate(smoking_status10yr = coalesce(PrevSmoker_date10y1, Smoker_date_10yr1))
Pop <- Pop %>% mutate(smoking_status10yr = coalesce(smoking_status10yr, non_smoker_date10y1))
Pop <- Pop %>% mutate(smoking_status10yr = coalesce(smoking_status10yr, current_non_smoker_date10yr1))
# replace those with missing observations with "Missing"
Pop <- Pop %>% mutate(smoking_status10yr = replace(smoking_status10yr, is.na(smoking_status10yr), "4Missing"))


# medications (3 months before index date)
# for(i in seq_along(medication_cohorts$cohort_name)){
#   working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
#   working_id <- medication_cohorts$cohort_definition_id[[i]]
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
#           (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
#              feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
#             # ending in window
#             (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
#                feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
#         select(person_id) %>%
#         distinct() %>%
#         mutate(!!working_name:=1),
#       by="person_id") %>%
#     compute()
# }

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

# conditions (liver related)
for(i in seq_along(disease_cohorts_liver$cohort_definition_id)){
  
  working_name <- glue::glue("{disease_cohorts_liver$cohort_name[[i]]}")
  working_id <- disease_cohorts_liver$cohort_definition_id[[i]]
  Pop <- 
    Pop %>% 
    left_join(
      Pop %>% 
        select("person_id", "outcome_start_date") %>% 
        inner_join(cdm[[feature_disease_liver_table_name]] %>% 
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

# gender splits for characterisation
PopFemale <- Pop %>% 
  filter(gender == "Female")
PopMale <- Pop %>% 
  filter(gender == "Male")



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
      mutate(smoking_status5yr=factor(smoking_status5yr, levels=c("1Non Smoker", "2Former", "3Smoker", "4Missing"))) %>%
      group_by(smoking_status5yr) %>%
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%
      rename("var"="smoking_status5yr") %>%
      mutate(var=paste0("smoking_status5yr: ", var)),
    
    data %>%
      mutate(smoking_status10yr=factor(smoking_status10yr, levels=c("1Non Smoker", "2Former", "3Smoker", "4Missing"))) %>%
      group_by(smoking_status10yr) %>%
      summarise(n=n(),
                percent=paste0(nice.num((n/nrow(data))*100),  "%")) %>%
      rename("var"="smoking_status10yr") %>%
      mutate(var=paste0("smoking_status10yr: ", var)),
    
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
  
  #disease features
  for(i in seq_along(disease_cohorts$cohort_name)){
    working_id_name <- glue::glue("{disease_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>%
                                           summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>%
                                           mutate(var=working_id_name)
    )
  }
  #other cancers
  for(i in seq_along(outcome_cohorts$cohort_name)){
    working_name <- glue::glue("{outcome_cohorts$cohort_name[[i]]}")
    summary_characteristics <- bind_rows(summary_characteristics,
                                         data %>%
                                           summarise(n=sum(!is.na(!!rlang::sym(working_name))),
                                                     percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>%
                                           mutate(var=working_name)
    )
  }
  #medications
  # for(i in seq_along(medication_cohorts$cohort_name)){
  #   working_id_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
  #   summary_characteristics <- bind_rows(summary_characteristics,
  #                                        data %>% 
  #                                          summarise(n=sum(!is.na(!!rlang::sym(working_id_name))),
  #                                                    percent=paste0(nice.num((n/nrow(data))*100),  "%"))%>% 
  #                                          mutate(var=working_id_name)
  #   )
  # }
  #liver cancer features
  for(i in seq_along(disease_cohorts_liver$cohort_name)){
    working_id_name <- glue::glue("{disease_cohorts_liver$cohort_name[[i]]}")
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
  
  # add in smoking status # former, non or smoker in anytime prior history
  # If smoking status was not recorded in year, the methods of last observation carried forward (LOCF)
  # Records of non-smoking after those for current or previous smoking were amended to former-smoking. 
  # As it was not possible to ascertain smoking status prior to CPRD records, we refer to non- rather than never-smoking.
  
  # non smoking 5 years
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter( observation_concept_id == 4222303 |
                               observation_concept_id ==  4144272 
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("non_smoker_date5y"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  # non smoking 10 years
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter( observation_concept_id == 4222303 |
                               observation_concept_id ==  4144272 
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("non_smoker_date10y"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  
  # current non smoking 5 yr
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(observation_concept_id == 4052464
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("current_non_smoker_date5yr"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  # current non smoking 10 yr
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(observation_concept_id == 4052464
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("current_non_smoker_date10yr"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  # Smoker any time 5 yr
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(
                       observation_concept_id == 4005823 |
                         observation_concept_id == 4298794 | 
                         observation_concept_id == 762499 |
                         observation_concept_id == 762498 |
                         observation_concept_id == 37395605 |
                         observation_concept_id == 4246415 |
                         observation_concept_id ==  4218917 |
                         observation_concept_id == 4209006 |
                         observation_concept_id ==  4276526 |
                         observation_concept_id == 4209585 |
                         observation_concept_id == 764104 |
                         observation_concept_id == 764103 |
                         observation_concept_id == 4058138 |
                         observation_concept_id == 4042037 |
                         observation_concept_id == 4044777 |
                         observation_concept_id == 4044776 |
                         observation_concept_id == 4044775 |
                         observation_concept_id == 4041511 |
                         observation_concept_id == 4052029 |
                         observation_concept_id == 4058136 |
                         observation_concept_id == 4044778 |
                         observation_concept_id == 4052030 |
                         observation_concept_id == 4144273 |
                         observation_concept_id == 4052947 |
                         observation_concept_id == 4209006 |
                         observation_concept_id == 4204653 |
                         observation_concept_id == 44789712 |
                         observation_concept_id == 40486518 |
                         observation_concept_id ==4046886 |
                         observation_concept_id == 4058137 |
                         observation_concept_id == 4216174 |
                         observation_concept_id == 4215409 |
                         observation_concept_id == 4190573 |
                         observation_concept_id == 4052948 
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("Smoker_date_5yr"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  
  # Smokers current or code within 10 years
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(
                       observation_concept_id == 4005823 |
                         observation_concept_id == 4298794 | 
                         observation_concept_id == 762499 |
                         observation_concept_id == 762498 |
                         observation_concept_id == 37395605 |
                         observation_concept_id == 4246415 |
                         observation_concept_id ==  4218917 |
                         observation_concept_id == 4209006 |
                         observation_concept_id ==  4276526 |
                         observation_concept_id == 4209585 |
                         observation_concept_id == 764104 |
                         observation_concept_id == 764103 |
                         observation_concept_id == 4058138 |
                         observation_concept_id == 4042037 |
                         observation_concept_id == 4044777 |
                         observation_concept_id == 4044776 |
                         observation_concept_id == 4044775 |
                         observation_concept_id == 4041511 |
                         observation_concept_id == 4052029 |
                         observation_concept_id == 4058136 |
                         observation_concept_id == 4044778 |
                         observation_concept_id == 4052030 |
                         observation_concept_id == 4144273 |
                         observation_concept_id == 4052947 |
                         observation_concept_id == 4209006 |
                         observation_concept_id == 4204653 |
                         observation_concept_id == 44789712 |
                         observation_concept_id == 40486518 |
                         observation_concept_id ==4046886 |
                         observation_concept_id == 4058137 |
                         observation_concept_id == 4216174 |
                         observation_concept_id == 4215409 |
                         observation_concept_id == 4190573 |
                         observation_concept_id == 4052948 
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("Smoker_date_10yr"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  # former smokers 5 year
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(
                       observation_concept_id == 4052032 | # stopped smoking
                         observation_concept_id == 44802805 # recently stopped smoking
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(1826) ) %>% # removes anyone with observation more than 5 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("PrevSmoker_date5y"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  
  # former smokers 10 year
  Pophan <-
    Pophan %>%
    left_join(
      Pophan %>%
        select("person_id", "outcome_start_date") %>%
        inner_join(cdm$observation %>%
                     filter(
                       observation_concept_id == 4052032 | # stopped smoking
                         observation_concept_id == 44802805 # recently stopped smoking
                     ) ,
                   by=c("person_id"), copy = TRUE)  %>%
        filter(observation_date < outcome_start_date) %>% # removes anyone with a observation after outcome
        filter(observation_date > outcome_start_date - lubridate::days(3650) ) %>% # removes anyone with observation more than 10 years before the outcome
        select(c(person_id, observation_date, outcome_start_date)) %>%
        distinct() %>%
        group_by(person_id, outcome_start_date) %>%
        filter(observation_date == max(observation_date)) %>%
        rename("PrevSmoker_date10y"="observation_date"),
      by= c("person_id", "outcome_start_date")) %>%
    compute()
  
  
  
  
  # rules for updating smoking status
  # if only smoking codes == SMOKER, non smoker == NON SMOKER
  # Never-smokers were re-assigned as former-smokers if there were contradictory preceding smoking codes
  # previous smokers were assigned smokers if date of smoking was after previous smoking code 
  
  # Smoking status using 5 years worth of previous data
  # create a new variable for previous and if those who say non smoking convert to former/previous smoker
  Pophan <- Pophan %>% mutate(PrevSmoker_date5y1 = ifelse(!is.na(PrevSmoker_date5y), non_smoker_date5y, PrevSmoker_date5y))
  Pophan <- Pophan %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, !is.na(PrevSmoker_date5y1), "2Former"))
  Pophan <- Pophan %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, !is.na(PrevSmoker_date5y), "2Former"))
  
  # remove previous entries if the have a smoking date after previous
  Pophan <- Pophan %>% mutate(Smokernow = Smoker_date_5yr > PrevSmoker_date5y )
  Pophan <- Pophan %>% mutate(PrevSmoker_date5y1 = replace(PrevSmoker_date5y1, Smokernow == TRUE, NA))
  
  # convert smoking into a character variable
  Pophan <- Pophan %>% mutate(Smoker_date_5yr1 = as.character(Smoker_date_5yr))
  Pophan <- Pophan %>% mutate(Smoker_date_5yr1 = replace(Smoker_date_5yr1, !is.na(Smoker_date_5yr1), "3Smoker"))
  
  # convert non smokers into non smoker
  Pophan <- Pophan %>% mutate(non_smoker_date5y1 = as.character(non_smoker_date5y))
  Pophan <- Pophan %>% mutate(non_smoker_date5y1 = replace(non_smoker_date5y1, !is.na(non_smoker_date5y1), "1Non Smoker"))
  
  Pophan <- Pophan %>% mutate(current_non_smoker_date5yr1 = as.character(current_non_smoker_date5yr))
  Pophan <- Pophan %>% mutate(current_non_smoker_date5yr1 = replace(current_non_smoker_date5yr1, !is.na(current_non_smoker_date5yr1), "1Non Smoker"))
  
  # merge all to get smoking status in past 5 years
  Pophan <- Pophan %>% mutate(smoking_status5yr = coalesce(PrevSmoker_date5y1, Smoker_date_5yr1))
  Pophan <- Pophan %>% mutate(smoking_status5yr = coalesce(smoking_status5yr, non_smoker_date5y1))
  Pophan <- Pophan %>% mutate(smoking_status5yr = coalesce(smoking_status5yr, current_non_smoker_date5yr1))
  # replace those with missing observations with "Missing"
  Pophan <- Pophan %>% mutate(smoking_status5yr = replace(smoking_status5yr, is.na(smoking_status5yr), "4Missing"))
  
  
  
  # Smoking status 10 years previous
  Pophan <- Pophan %>% mutate(PrevSmoker_date10y1 = ifelse(!is.na(PrevSmoker_date10y), non_smoker_date10y, PrevSmoker_date10y))
  Pophan <- Pophan %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, !is.na(PrevSmoker_date10y1), "2Former"))
  Pophan <- Pophan %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, !is.na(PrevSmoker_date10y), "2Former"))
  
  # remove previous entries if the have a smoking date after previous
  Pophan <- Pophan %>% mutate(Smokernow = Smoker_date_10yr > PrevSmoker_date10y )
  Pophan <- Pophan %>% mutate(PrevSmoker_date10y1 = replace(PrevSmoker_date10y1, Smokernow == TRUE, NA))
  
  # convert smoking into a character variable
  Pophan <- Pophan %>% mutate(Smoker_date_10yr1 = as.character(Smoker_date_10yr))
  Pophan <- Pophan %>% mutate(Smoker_date_10yr1 = replace(Smoker_date_10yr1, !is.na(Smoker_date_10yr1), "3Smoker"))
  
  # convert non smokers into non smoker
  Pophan <- Pophan %>% mutate(non_smoker_date10y1 = as.character(non_smoker_date10y))
  Pophan <- Pophan %>% mutate(non_smoker_date10y1 = replace(non_smoker_date10y1, !is.na(non_smoker_date10y1), "1Non Smoker"))
  
  Pophan <- Pophan %>% mutate(current_non_smoker_date10yr1 = as.character(current_non_smoker_date10yr))
  Pophan <- Pophan %>% mutate(current_non_smoker_date10yr1 = replace(current_non_smoker_date10yr1, !is.na(current_non_smoker_date10yr1), "1Non Smoker"))
  
  # merge all to get smoking status in past 10 years
  Pophan <- Pophan %>% mutate(smoking_status10yr = coalesce(PrevSmoker_date10y1, Smoker_date_10yr1))
  Pophan <- Pophan %>% mutate(smoking_status10yr = coalesce(smoking_status10yr, non_smoker_date10y1))
  Pophan <- Pophan %>% mutate(smoking_status10yr = coalesce(smoking_status10yr, current_non_smoker_date10yr1))
  # replace those with missing observations with "Missing"
  Pophan <- Pophan %>% mutate(smoking_status10yr = replace(smoking_status10yr, is.na(smoking_status10yr), "4Missing"))
  
  
  # get the co morbidites and medication usage for subset of cohort ----
  # medications (3 months before index date)
  # for(i in seq_along(medication_cohorts$cohort_definition_id)){
  #   working_name <- glue::glue("{medication_cohorts$cohort_name[[i]]}")
  #   working_id <- medication_cohorts$cohort_definition_id[[i]]
  #   Pophan <-
  #     Pophan %>%
  #     left_join(
  #       Pophan %>%
  #         select("person_id", "outcome_start_date") %>% 
  #         inner_join(cdm[[feature_medication_table_name]] %>%
  #                      rename("feature_start_date"="cohort_start_date") %>%
  #                      rename("feature_end_date"="cohort_end_date") %>%
  #                      filter(cohort_definition_id== working_id ) %>%
  #                      select(!cohort_definition_id),
  #                    by=c("person_id" = "subject_id"), copy = TRUE) %>% 
  #         filter(
  #           # overlapping
  #           (feature_start_date <= (outcome_start_date-lubridate::days(-1)) &
  #              feature_end_date >= (outcome_start_date-lubridate::days(-1))) |
  #             # ending in window
  #             (feature_end_date >= (outcome_start_date-lubridate::days(90)) &
  #                feature_end_date <= (outcome_start_date-lubridate::days(-1)))) %>%
  #         select(person_id) %>%
  #         distinct() %>%
  #         mutate(!!working_name:=1),
  #       by="person_id") %>%
  #     compute()
  # }
  
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
  
  # conditions (liver related)
  for(i in seq_along(disease_cohorts_liver$cohort_definition_id)){
    
    working_name <- glue::glue("{disease_cohorts_liver$cohort_name[[i]]}")
    working_id <- disease_cohorts_liver$cohort_definition_id[[i]]
    Pophan <- 
      Pophan %>% 
      left_join(
        Pop %>% 
          select("person_id", "outcome_start_date") %>% 
          inner_join(cdm[[feature_disease_liver_table_name]] %>% 
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
  write_csv(table1Characteristics_all_han, here::here(paste0("Results/",db.name,"/Table1Han",db.name,".csv")))
  
}

# function to extract table one characteristics per gender and by calendar year diagnosis
if (agestandardization == TRUE) {
  
#for females
  table1CharacteristicsF <- list()
  
  #create a loop that puts table 1 for each outcome
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1CharacteristicsF[[j]] <- get_summary_characteristics(PopFemale %>% 
                                                                filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1CharacteristicsF[[j]]$n <- as.character(table1CharacteristicsF[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
  
  #bind all results together
  table1Characteristics_female <- bind_rows(table1CharacteristicsF) %>%
    mutate(Database = db.name, analysis = "Incidence", Gender = "Female")
  
  #save the results
  write_csv(table1Characteristics_female, here::here(paste0("Results/",db.name,"/Table1Females",db.name,".csv")))
  
# for males
  
  table1CharacteristicsM <- list()
  
  #create a loop that puts table 1 for each outcome
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1CharacteristicsM[[j]] <- get_summary_characteristics(PopMale %>% 
                                                                filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1CharacteristicsM[[j]]$n <- as.character(table1CharacteristicsM[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
  
  #bind all results together
  table1Characteristics_male <- bind_rows(table1CharacteristicsM) %>%
    mutate(Database = db.name, analysis = "Incidence", Gender = "Male")
  
  #save the results
  write_csv(table1Characteristics_male, here::here(paste0("Results/",db.name,"/Table1Males",db.name,".csv")))  
  

# for calender year
  Popcy <- DataExtraction(dataset = Pop)  
  
  table1Characteristics2000 <- list()
  table1Characteristics2005 <- list()
  table1Characteristics2010 <- list()
  table1Characteristics2015 <- list()
  table1Characteristics2020 <- list()
  
  #create a loop that puts table 1 for each outcome 2000-2004
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1Characteristics2000[[j]] <- get_summary_characteristics(Popcy[[2]] %>% 
                                                                 filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1Characteristics2000[[j]]$n <- as.character(table1Characteristics2000[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
 
  #create a loop that puts table 1 for each outcome 2005-2009
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1Characteristics2005[[j]] <- get_summary_characteristics(Popcy[[3]] %>% 
                                                                    filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1Characteristics2005[[j]]$n <- as.character(table1Characteristics2005[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
  
  #create a loop that puts table 1 for each outcome 2010-2014
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1Characteristics2010[[j]] <- get_summary_characteristics(Popcy[[4]] %>% 
                                                                    filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1Characteristics2010[[j]]$n <- as.character(table1Characteristics2010[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }  
  
  #create a loop that puts table 1 for each outcome 2015-2019
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1Characteristics2015[[j]] <- get_summary_characteristics(Popcy[[5]] %>% 
                                                                    filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1Characteristics2015[[j]]$n <- as.character(table1Characteristics2015[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
  
  #create a loop that puts table 1 for each outcome 2020-2021
  for(j in seq_along(outcome_cohorts$cohort_definition_id)){
    
    
    table1Characteristics2020[[j]] <- get_summary_characteristics(Popcy[[6]] %>% 
                                                                    filter(outcome_cohort_name==outcome_cohorts$cohort_name[[j]])) %>%
      mutate(Cancer = outcome_cohorts$cohort_name[[j]])
    
    table1Characteristics2020[[j]]$n <- as.character(table1Characteristics2020[[j]]$n)
    
    print(paste0("Getting table 1 for ", outcome_cohorts$cohort_name[[j]], " (" , j , " of ", length(outcome_cohorts$cohort_name), ")")) 
    
  }
  
  #bind all results together
  table1Characteristics_cy2000 <- bind_rows(table1Characteristics2000) %>%
    mutate(Database = db.name, analysis = "Incidence", Calendar_year = "2000 to 2004") 
  
  table1Characteristics_cy2005 <- bind_rows(table1Characteristics2005) %>%
    mutate(Database = db.name, analysis = "Incidence", Calendar_year = "2005 to 2009") 
  
  table1Characteristics_cy2010 <- bind_rows(table1Characteristics2010) %>%
    mutate(Database = db.name, analysis = "Incidence", Calendar_year = "2010 to 2014") 
  
  table1Characteristics_cy2015 <- bind_rows(table1Characteristics2015) %>%
    mutate(Database = db.name, analysis = "Incidence", Calendar_year = "2015 to 2019") 
  
  table1Characteristics_cy2020 <- bind_rows(table1Characteristics2020) %>%
    mutate(Database = db.name, analysis = "Incidence", Calendar_year = "2020 to 2021") 
  
#merge all together and remove results for breast cancer as this contains both males and females
  table1Characteristics_cy<- rbind(
    
    table1Characteristics_cy2000,
    table1Characteristics_cy2005,
    table1Characteristics_cy2010,
    table1Characteristics_cy2015,
    table1Characteristics_cy2020
    
  ) %>% 
    filter(Cancer != "IncidentBreastCancer")
  

  
# for calender year females breast cancer only
  
  PopFemaleBreast <- PopFemale %>% 
    filter(outcome_cohort_name == "IncidentBreastCancer")
  
  PopcyF <- DataExtraction(dataset = PopFemaleBreast) 
  
  #get results set up
  table1Characteristics2000f <- get_summary_characteristics(PopcyF[[2]]) %>% 
    mutate(Cancer = "Breast", Database = db.name, analysis = "Incidence", Calendar_year = "2000 to 2004") 
  table1Characteristics2000f$n <- as.character(table1Characteristics2000f$n)  
  
  table1Characteristics2005f <- get_summary_characteristics(PopcyF[[3]]) %>% 
    mutate(Cancer = "Breast", Database = db.name, analysis = "Incidence", Calendar_year = "2005 to 2009") 
  table1Characteristics2005f$n <- as.character(table1Characteristics2005f$n) 
  
  table1Characteristics2010f <- get_summary_characteristics(PopcyF[[4]]) %>% 
    mutate(Cancer = "Breast", Database = db.name, analysis = "Incidence", Calendar_year = "2010 to 2014") 
  table1Characteristics2010f$n <- as.character(table1Characteristics2010f$n) 
  
  table1Characteristics2015f <- get_summary_characteristics(PopcyF[[5]]) %>% 
    mutate(Cancer = "Breast", Database = db.name, analysis = "Incidence", Calendar_year = "2015 to 2019") 
  table1Characteristics2015f$n <- as.character(table1Characteristics2015f$n) 
  
  table1Characteristics2020f <- get_summary_characteristics(PopcyF[[6]]) %>% 
    mutate(Cancer = "Breast", Database = db.name, analysis = "Incidence", Calendar_year = "2020 to 2021") 
  table1Characteristics2020f$n <- as.character(table1Characteristics2020f$n) 
  
  table1Characteristics_cy_f <- rbind(
    
    table1Characteristics2000f ,
    table1Characteristics2005f ,
    table1Characteristics2010f ,
    table1Characteristics2015f ,
    table1Characteristics2020f
  )
  
  
  
  #merge results together
  table1Characteristics_cy_final <- rbind(table1Characteristics_cy, table1Characteristics_cy_f)
  
  #save results
  write_csv(table1Characteristics_cy_final, here::here(paste0("Results/",db.name,"/Table1CalenderYrStrat",db.name,".csv")))   
}







