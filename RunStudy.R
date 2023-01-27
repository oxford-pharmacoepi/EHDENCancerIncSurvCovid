# table names----
outcome_table_name<-paste0(outcome_table_stem,"_o")

# QC plot folder ----
if (!file.exists(here("3_QC"))){
  dir.create("3_QC", recursive = TRUE)}

# output files ---- 
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

# add functions ----
# risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

#exporting the survival results
exportSurvivalResults <- function(result, zipName, outputFolder) {
  
  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }
  
  # write results to disk
  lapply(names(result), FUN = function(checkResultName) {
    checkResult <- result[[checkResultName]]
    utils::write.csv(checkResult,
                     file = file.path(
                       tempDir,
                       paste0(checkResultName, ".csv")
                     ),
                     row.names = FALSE
    )
  })
  zip::zip(zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
           files = list.files(tempDir, full.names = TRUE))
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }
  
  invisible(result)
}


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence rate analysis ----
info(logger, 'RUNNING INCIDENCE RATE ANALYSIS')
source(here("2_Analysis","IncidenceAnalysis.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

# Run survival analysis -----
info(logger, 'RUNNING SURVIVAL ANALYSIS')
source(here("2_Analysis","SurvivalAnalysis.R"))
info(logger, 'SURVIVAL ANALYSIS RAN')

# combine all the results -----
survivalResults <- bind_rows(
  observedkmcombined , # all 
  observedkmcombined_gender , # gender strat 
  observedkmcombined_age , # age strat
  observedkmcombined_age_gender # age gender strat
) %>%
  mutate(Database = db.name)

#risk table # error with characters and double formats
riskTableResults <- bind_rows(
  risktableskm , # all
  risktableskm_gender , # gender strat
  risktableskm_age , # age strat
  risktableskm_age_gender # age*gender strat 
) %>%
  mutate(Database = db.name)

#median results
medianKMResults <- bind_rows( 
  medkmcombined , # all
  medkmcombined_gender , # gender
  medkmcombined_age , # age strat
  medkmcombined_age_gender # age*gender strat 
) %>%
  mutate(Database = db.name)


# put results all together in a list
survival_study_results <- list(survivalResults ,
                         riskTableResults,
                         medianKMResults )

names(survival_study_results) <- c(paste0("survival_estimates_", db.name),
                                 paste0("risk_table_results_", db.name),
                                 paste0("median_survival_results_", db.name) )
                                 
                                 
# zip results
print("Zipping results to output folder")
exportSurvivalResults(result=survival_study_results,
                      zipName= paste0(db.name, "SurvivalResults"),
                      outputFolder=here::here("Results", db.name))


print("Done!")
print("-- If all has worked, there should now be two zip folders with the incidence/prevalence and survival results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)











