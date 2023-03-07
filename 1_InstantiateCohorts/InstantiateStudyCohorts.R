# instantiate outcome cohorts
info(logger, "- getting incident outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

info(logger, "- getting incident outcomes")

cdm <- CDMConnector::generateCohortSet(cdm, 
                                       cohortSet = outcome_cohorts,
                                       name = outcome_table_name,
                                       overwrite = TRUE
)

info(logger, "- got outcomes")


# instantiate prevalent outcome cohorts
info(logger, "- getting prevalent outcomes")

prevalent_cohorts <- readCohortSet(here::here("1_InstantiateCohorts","PrevalentCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
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
  
  outcome_cohorts_han <- CDMConnector::readCohortSet(here(
    "1_InstantiateCohorts",
    "HeadNeckSubtypes" ,
    "OutcomeCohorts"
  ))
  
  info(logger, "- getting incident han outcomes")
  
  cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts_han,
                                         name = outcome_table_name_han,
                                         overwrite = TRUE
  )
  
  info(logger, "- got outcomes")
  
  
  # instantiate prevalent outcome cohorts
  info(logger, "- getting prevalent han outcomes")
  
  prevalent_cohorts_han <- readCohortSet(here::here("1_InstantiateCohorts",
                                                "HeadNeckSubtypes" ,
                                                "PrevalentCohorts"))
  
  cdm <- generateCohortSet(cdm = cdm, 
                           cohortSet = prevalent_cohorts_han,
                           name = prevalent_table_name_han,
                           overwrite = TRUE
  )
  
  info(logger, "- got prevalent han outcomes")
  
}