# instantiate outcome cohorts
info(logger, "- getting outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

info(logger, "- getting outcomes")


cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts,
                         cohortTableName = outcome_table_name,
                         overwrite = TRUE
)


info(logger, "- got outcomes")

# instantiate strata cohorts
info(logger, "- getting strata definitions")
strata_cohorts <- readCohortSet(here(
  "1_InstantiateCohorts",
  "StrataCohorts"
))
info(logger, "- getting strata")
cdm <- generateCohortSet(cdm, strata_cohorts,
                         cohortTableName = strata_table_name,
                         overwrite = TRUE
)


info(logger, "- got strata")