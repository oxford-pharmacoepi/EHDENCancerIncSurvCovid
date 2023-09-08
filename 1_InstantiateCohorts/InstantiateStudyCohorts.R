# instantiate outcome cohorts
info(logger, "- getting incident outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

prevalent_cohorts <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "PrevalentCohorts"))

outcome_cohorts_han <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "HeadNeckSubtypes" ,
  "OutcomeCohorts"
))


prevalent_cohorts_han <- CDMConnector::readCohortSet(here::here("1_InstantiateCohorts",
                                                                "HeadNeckSubtypes" ,
                                                                "PrevalentCohorts"))

# only instanstiate cohorts if this argument is TRUE
if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = cdm_database_schema,
                                    write_schema = results_database_schema,
                                    cohort_tables = c(outcome_table_name,
                                                      prevalent_table_name,
                                                      outcome_table_name_han,
                                                      prevalent_table_name_han))
  
  
} else {

print("Instantiating cancer cohorts")
  
info(logger, "- getting incident outcomes")

cdm <- CDMConnector::generateCohortSet(cdm, 
                                       cohortSet = outcome_cohorts,
                                       name = outcome_table_name,
                                       overwrite = TRUE
)

info(logger, "- got outcomes")


# instantiate prevalent outcome cohorts
info(logger, "- getting prevalent outcomes")

cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                         cohortSet = prevalent_cohorts,
                         name = prevalent_table_name,
                         overwrite = TRUE
                         
)

info(logger, "- got prevalent outcomes")


# if running CPRD data also run the subsets of head and neck cancers for inc/prev
if (grepl("CPRD", db.name) == TRUE) {
  
  print("Instantiating head and neck subtypes")
  
  # instantiate outcome cohorts
  info(logger, "- getting incident outcome definitions head and neck")
  
  
  info(logger, "- getting incident han outcomes")
  
  cdm <- CDMConnector::generateCohortSet(cdm, 
                                         cohortSet = outcome_cohorts_han,
                                         name = outcome_table_name_han,
                                         overwrite = TRUE
  )
  
  info(logger, "- got outcomes")
  
  
  # instantiate prevalent outcome cohorts
  info(logger, "- getting prevalent han outcomes")

  
  cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                           cohortSet = prevalent_cohorts_han,
                           name = prevalent_table_name_han,
                           overwrite = TRUE
  )
  
  info(logger, "- got prevalent han outcomes")
  
}

}
