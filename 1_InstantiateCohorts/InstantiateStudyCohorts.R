# instantiate outcome cohorts
info(logger, "- getting incident outcome definitions")

outcome_cohorts <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "OutcomeCohorts"
))

# prevalent_cohorts <- CDMConnector::readCohortSet(here::here(
#   "1_InstantiateCohorts",
#   "PrevalentCohorts"))

# only instanstiate cohorts if this argument is TRUE
instantiatedCohorts <- FALSE
if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = cdm_database_schema,
                                    write_schema = results_database_schema,
                                    cohort_tables = c(outcome_table_name))
  
  
} else {

print("Instantiating cancer cohorts")
  
info(logger, "- getting incidence cohorts")

cdm <- CDMConnector::generateCohortSet(cdm, 
                                       cohortSet = outcome_cohorts,
                                       computeAttrition = TRUE,
                                       name = incidence_table_name,
                                       overwrite = TRUE
)

info(logger, "- got incidence cohorts")


# instantiate prevalent outcome cohorts
# info(logger, "- getting prevalent outcomes")
# 
# cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
#                          cohortSet = prevalent_cohorts,
#                          computeAttrition = TRUE,
#                          name = prevalent_table_name,
#                          overwrite = TRUE
#                          
# )
# 
# info(logger, "- got prevalent outcomes")

}
