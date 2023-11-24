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
  minCellCount = 0,
  temporary = FALSE,
  returnParticipants = TRUE
)


# create a loop which pulls out the participants used for the incidence analysis
cancer_participants <- list()

# get participants from incidence for table one and survival
for (i in 1:length(outcome_cohorts$cohort_definition_id)){

#extract settings for survival from incidence results
cancer_participants[[i]] <- participants(result = inc_overall_participants, analysisId = i) %>% 
  mutate(cohort_definition_id = i) %>% 
  filter(!(is.na(outcome_start_date))) %>% 
  mutate(cohort_start_date = outcome_start_date) %>% 
  relocate(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)

}


# stick all the participants from all outcomes into one
cdm$cancer_table_one <- Reduce(union_all, cancer_participants) %>%
  computeQuery(
    name = "cancer_table_one", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  ) 


# set the cohort back into the cdm
cdm$cancer_table_one <- new_generated_cohort_set(
  cohort_ref = cdm$cancer_table_one,
  overwrite = TRUE
)

cdm$cancer_table_one <- cdm$cancer_table_one %>% 
  
  PatientProfiles::addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup =  list(
      "age_gr" =
        list(
          "18 to 29" = c(18, 29),
          "30 to 39" = c(30, 39),
          "40 to 49" = c(40, 49),
          "50 to 59" = c(50, 59),
          "60 to 69" = c(60, 69),
          "70 to 79" = c(70, 79),
          "80 to 89" = c(80, 89),
          "90 +" = c(90, 150)
          
        )
    )
  )

# bug with date for cohort_end_date so fix this
cdm$cancer_table_one <- cdm$cancer_table_one %>% 
  mutate(cohort_end_date = as.Date(cohort_end_date))

# add grouping for stratification of year of diagnosis
# extract year
cdm$cancer_table_one <- cdm$cancer_table_one %>% 
  mutate(calendar_yr_diag = lubridate::year(cohort_start_date))

# create groupings
cdm$cancer_table_one <- cdm$cancer_table_one %>% 
  mutate(
    calendar_yr_gp = case_when(
      between(lubridate::year(cohort_start_date), 2000, 2004) ~ "2000 to 2004",
      between(lubridate::year(cohort_start_date), 2005, 2009) ~ "2005 to 2009",
      between(lubridate::year(cohort_start_date), 2010, 2014) ~ "2010 to 2014",
      between(lubridate::year(cohort_start_date), 2015, 2019) ~ "2015 to 2019",
      between(lubridate::year(cohort_start_date), 2020, 2022) ~ "2020 to 2022",
      TRUE ~ NA_character_  # Default case if none of the conditions are met
    )
  )


# subset the CDM for analysis table to make code run quicker
info(logger, "SUBSETTING CDM")
cdm <- CDMConnector::cdmSubsetCohort(cdm, "cancer_table_one")
info(logger, "SUBSETTED CDM")

  # instantiate medications
  info(logger, "INSTANTIATE MEDICATIONS")
  codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)


  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                           conceptSet = codelistMedications, 
                                                           name = "medications")

  info(logger, "INSTANTIATED MEDICATIONS")

  # instantiate conditions
  info(logger, "INSTANTIATE CONDITIONS")
  codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = codelistConditions,
                                                name = "conditions",
                                                overwrite = TRUE)

  info(logger, "INSTANTIATED CONDITIONS")


  info(logger, "CREATE TABLE ONE SUMMARY")

  suppressWarnings(
    tableone <- cdm$cancer_table_one %>%
      PatientProfiles::summariseCharacteristics(
        strata = list(c("sex"),
                      c("calendar_yr_gp"), c("sex", "calendar_yr_gp")),
        minCellCount = 5,
        ageGroup = list(c(18, 29),
                            c(30, 39),
                            c(40, 49),
                            c(50, 59),
                            c(60, 69),
                            c(70, 79),
                            c(80, 89),
                            c(90, 150)),
        tableIntersect = list(
          "Visits" = list(
            tableName = "visit_occurrence", value = "count", window = c(-365, -1)
          )
        ),
        cohortIntersect = list(
          "Medications" = list(
            targetCohortTable = "medications", value = "flag", window = c(-365, -1)
          ),
          "Conditions" = list(
            targetCohortTable = "conditions", value = "flag", window = c(-Inf, -1)
          )
        )
      )
  )


  info(logger, "CREATED TABLE ONE SUMMARY")


info(logger, "CREATING TABLE ONE")

# rename cancers with better formats
tableone <- tableone %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 1", "Breast")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 2", "Colorectal")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 3", "Esophageal")) %>% 
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 4", "Head and Neck")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 5", "Liver")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 6", "Lung")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 7", "Pancreatic")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 8", "Prostate")) %>%
  dplyr::mutate(group_level = replace(group_level, group_level == "Cohort 9", "Stomach")) 

#save table one for characterization of cancers
readr::write_csv(tableone, paste0(here::here(output.folder),"/", cdm_name(cdm), "_tableone_summary.csv"))

# # tidy up the table ones
# # overall
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
# 
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>%
#     dplyr::filter(strata_name == "Overall")
# 
#   tb1_temp <- reformat_table_one(tabledata) %>%
#     dplyr::mutate(Cancer = unique(tabledata$group_level),
#                   Stratification = "none",
#                   Sex = "Both" ,
#                   Age = "All" ,
#                   Database = db.name)
# 
# 
#   tableone_clean_temp[[tableonecancer]] <- tb1_temp
#   rm(tb1_temp)
# 
# }
# tableone_overall <- dplyr::bind_rows(tableone_clean_temp)
# 
# # by sex
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
# 
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>%
#     dplyr::filter(strata_name == "sex")
# 
#   if(unique(tableone$group_level)[tableonecancer] != "Prostate") {
# 
#     tb1_tempF <- tabledata %>%
#       dplyr::filter(strata_level == "Female") %>%
#       reformat_table_one() %>%
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Female" ,
#                     Age = "All" ,
#                     Database = db.name)
# 
#     tb1_tempM <- tabledata %>%
#       dplyr::filter(strata_level == "Male") %>%
#       reformat_table_one() %>%
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Male" ,
#                     Age = "All" ,
#                     Database = db.name)
# 
#     #combine sexes together
#     tb1_temp <- dplyr::bind_rows(tb1_tempF , tb1_tempM)
# 
#     rm(tb1_tempF, tb1_tempM)
# 
#   } else {
# 
#     tb1_tempM <- tabledata %>%
#       dplyr::filter(strata_level == "Male") %>%
#       reformat_table_one() %>%
#       dplyr::mutate(Cancer = unique(tabledata$group_level),
#                     Stratification = "Sex",
#                     Sex = "Male" ,
#                     Age = "All" ,
#                     Database = db.name)
# 
#     tb1_temp <- tb1_tempM
#   }
# 
#   tableone_clean_temp[[tableonecancer]] <- tb1_temp
# 
#   rm(tb1_temp )
# 
# }
# tableone_sex <- dplyr::bind_rows(tableone_clean_temp)
# 
# # by age
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
# 
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>%
#     dplyr::filter(strata_name == "age_gr")
# 
#   tb1_temp_age <- list()
#   for(z in 1:length(unique(tabledata$strata_level))) {
# 
#     # because some age groups do not have data need to have try catches to make sure loop still continues even if data not available
#     tryCatch(
#       {
#         tb1_temp_age[[z]] <- tabledata %>%
#           dplyr::filter(strata_level == unique(tabledata$strata_level)[z]) %>%
#           reformat_table_one() %>%
#           dplyr::mutate(Cancer = unique(tabledata$group_level),
#                         Stratification = "Age",
#                         Sex = "Both",
#                         Age =  unique(tabledata$strata_level)[z] ,
#                         Database = db.name) %>%
#           dplyr::filter(!stringr::str_detect(Description, 'Age Group:'))
# 
#       },
#       error = function(e) {
#         cat(conditionMessage(e), "Table one not carried out for ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(" Table one not carried out for  ",unique(tableone$group_level)[tableonecancer], " ", e))
# 
#       },
#       warning = function(w){
#         cat(conditionMessage(e), "Warning problem with table one ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(unique(tableone$group_level)[tableonecancer], ": ", w))}
#     )
#   }
# 
#   tableone_clean_temp[[tableonecancer]] <- dplyr::bind_rows(tb1_temp_age)
# 
#   rm(tb1_temp_age)
# }
# tableone_age <- dplyr::bind_rows(tableone_clean_temp)
# 
# # by age and sex
# tableone_clean_temp <- list()
# for(tableonecancer in 1:length(unique(tableone$group_level))) {
# 
#   tabledata <- tableone %>%
#     dplyr::filter(group_level == unique(tableone$group_level)[tableonecancer]) %>%
#     dplyr::filter(strata_name == "sex and age_gr")
# 
#   tb1_temp_age_sex <- list()
# 
#   for(z in 1:length(unique(tabledata$strata_level))) {
# 
#     # because some age groups do not have data need to have try catches to make sure loop still continues even if data not available
#     tryCatch(
#       {
#         tb1_temp_age_sex[[z]] <- tabledata %>%
#           dplyr::filter(strata_level == unique(tabledata$strata_level)[z]) %>%
#           reformat_table_one() %>%
#           dplyr::mutate(Cancer = unique(tabledata$group_level),
#                         Stratification = "agesex",
#                         Sex = "Both",
#                         agesex =  unique(tabledata$strata_level)[z] ,
#                         Database = db.name) %>%
#           dplyr::mutate(agesex = str_replace(agesex, " and ", "_")) %>%
#           separate(col = "agesex",
#                    into = c("Sex", "Age"),
#                    sep = "_") %>%
#           dplyr::filter(!stringr::str_detect(Description, 'Age Group:'))
# 
#       },
#       error = function(e) {
#         cat(conditionMessage(e), "Table one not carried out for ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(" Table one not carried out for  ",unique(tableone$group_level)[tableonecancer], " ", e))
# 
#       },
#       warning = function(w){
#         cat(conditionMessage(e), "Warning problem with table one ", unique(tableone$group_level)[tableonecancer], "see log for more information")
#         info(logger, paste0(unique(tableone$group_level)[tableonecancer], ": ", w))}
#     )
#   }
# 
#   tableone_clean_temp[[tableonecancer]] <- dplyr::bind_rows(tb1_temp_age_sex)
# 
#   rm(tb1_temp_age_sex)
# }
# tableone_age_sex <- dplyr::bind_rows(tableone_clean_temp)
# 
# 
# # combine all tableone outputs
# tableone_final <- dplyr::bind_rows(tableone_overall, tableone_sex, tableone_age, tableone_age_sex)


info(logger, "CREATED TABLE ONE")



