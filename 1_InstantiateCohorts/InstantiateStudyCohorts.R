# instantiate outcome cohorts
info(logger, "- getting incident outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

info(logger, "- getting incident outcomes")


cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts,
                         cohortTableName = outcome_table_name,
                         overwrite = TRUE
)


info(logger, "- got outcomes")


# instantiate prevalent outcome cohorts
info(logger, "- getting prevalent outcomes")

prevalent_cohorts <- readCohortSet(here::here("1_InstantiateCohorts","PrevalentCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = prevalent_cohorts,
                         cohortTableName = prevalent_table_name,
                         overwrite = TRUE
)

info(logger, "- got prevalent outcomes")