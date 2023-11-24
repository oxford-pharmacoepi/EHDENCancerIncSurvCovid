# code for getting denominator and estimating incidence below

# cancer populations  -----
print(paste0("- Getting denominator: cancer populations"))
info(logger, "- Getting denominator: cancer populations")

#get denominator ------
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator" ,
  cohortDateRange = as.Date(c(studyStartDate,studyEndDate)),
  requirementInteractions = TRUE,
  ageGroup =list(
    c(18, 150),
    c(18, 29),
    c(30, 39),
    c(40, 49),
    c(50, 59),
    c(60, 69),
    c(70, 79),
    c(80, 89),
    c(90, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 365,
  overwrite = TRUE
)

print(paste0("- Got denominator: cancer populations"))
info(logger, "- Got denominator: cancer populations")

# Estimate incidence -------
print(paste0("- Getting incidence and period prevalence: cancer populations"))
info(logger, "- Getting incidence and period prevalence: cancer populations")

print(paste0("- Getting incidence: cancer populations"))
info(logger, "- Getting incidence: cancer populations")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = incidence_table_name,
  denominatorCohortId = NULL,
  interval = c("quarters", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

attrition_inc <- IncidencePrevalence::incidenceAttrition(inc)
write.csv(attrition_inc, here::here(paste0(output.folder,"/", db.name, "_incidence_attrition.csv")), row.names = FALSE)

print(paste0("- Got incidence: cancer populations"))
info(logger, "- Got incidence: cancer populations")

# print(paste0("- Getting period prevalence: cancer populations"))
# info(logger, "- Getting period prevalence: cancer populations")
# 
# # Estimate period prevalence ---------
# prev_period <- estimatePeriodPrevalence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = prevalent_table_name,
#   outcomeLookbackDays = 0, 
#   interval = "quarters" ,
#   completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
#   fullContribution = FALSE , # individuals only required to be present for one day in interval
#   minCellCount = 5,
#   temporary = TRUE,
#   returnParticipants = FALSE
# )
# 
# attrition_prev <- IncidencePrevalence::prevalenceAttrition(prev_period)
# write.csv(attrition_prev, here::here(paste0(output.folder,"/", db.name, "_prevalence_attrition.csv")), row.names = FALSE)
# 
# print(paste0("- Got period prevalence: cancer populations"))
# info(logger, "- Got period prevalence: cancer populations")

print(paste0("- Got incidence: cancer population"))
info(logger, "- Got incidence: cancer population")

# Export the results -----
print(paste0("- Exporting incidence: cancer populations"))
info(logger, "- Exporting incidence: cancer populations")

# exportIncidencePrevalenceResults(resultList = list(
#   "incidence" = inc,
#   "period_prevalence" = prev_period),
#   zipName= paste0(db.name, "IPResults"),
#   outputFolder=here::here("Results", db.name))

exportIncidencePrevalenceResults(resultList = list(
  "incidence" = inc),
  zipName= paste0(db.name, "IPResults"),
  outputFolder=here::here("Results", db.name))

print(paste0("- Exported incidence results: cancer populations"))
info(logger, "- Exported incidence results: cancer populations")

print(paste0("- Plotting incidence results: cancer populations"))
info(logger, "- Plotting incidence results: cancer populations")

# for age standardization we need results without obscuring therefore if
# age standardization required it will run this code and save the results
agestandardization <- TRUE
if (agestandardization == TRUE) {
  
inc_agestand <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = incidence_table_name,
  denominatorCohortId = NULL,
  interval = c("quarters", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 0,
  temporary = TRUE,
  returnParticipants = FALSE)

# # Estimate period prevalence ---------
# prev_period_agestand <- estimatePeriodPrevalence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = prevalent_table_name,
#   outcomeLookbackDays = 0, 
#   interval = "quarters" ,
#   completeDatabaseIntervals = TRUE, # prev only estimate for intervals where db captures all of the interval
#   fullContribution = FALSE , # individuals only required to be present for one day in interval
#   minCellCount = 0,
#   temporary = TRUE,
#   returnParticipants = FALSE
# )

# Get the results ----------------
exportIncidencePrevalenceResults(resultList = list(
  "incidence" = inc_agestand),
  zipName= paste0(db.name, "IPResultsAgeStandardization"),
  outputFolder=here::here("Results", db.name))

}
