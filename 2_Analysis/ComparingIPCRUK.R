# This code calculates incidence from 2016-2018 to compare to 
# incidence rates to that on the CRUK website
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type
# 

########################################
#incidence for time period from 2016-2018 (CRUK use these dates)

# cdm$denominator <- generateDenominatorCohortSet(
#   cdm = cdm,
#   startDate = as.Date("2016-01-01"),
#   endDate = as.Date("2018-12-31"),
#   ageGroup =list(
#     c(25, 150),
#     c(25, 29),
#     c(30, 34),
#     c(35, 39),  
#     c(40, 44),
#     c(45, 49),  
#     c(50, 54),
#     c(55, 59),  
#     c(60, 64),
#     c(65, 69),  
#     c(70, 74),
#     c(75, 79),  
#     c(80, 84),
#     c(85, 89), 
#     c(90, 150)
#   ),
#   sex = c("Female"),
#   daysPriorHistory = 365,
#   verbose = TRUE
# )


# inc <- estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeCohortId = outcome_cohorts$cohortId,
#   outcomeCohortName = outcome_cohorts$cohortName,
#   outcomeTable = outcome_table_name,
#   interval = "overall",
#   outcomeWashout = NULL,
#   repeatedEvents = FALSE,
#   completeDatabaseIntervals = TRUE,
#   minCellCount = 5
# )
# 
# 
# study_results2<- gatherIncidencePrevalenceResults(cdm = cdm,
#                                                   list(inc),
#                                                   databaseName = db.name)
# 
# asdf <- study_results2$incidence_estimates
# 
# exportIncidencePrevalenceResults(result=study_results2,
#                                  zipName= paste0(db.name, "IPResults2016_18"),
#                                  outputFolder=here::here("Results", db.name))
