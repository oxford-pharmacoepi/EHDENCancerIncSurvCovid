# table names----
outcome_table_name<-paste0(outcome_table_stem,"_o") # for incidence
prevalent_table_name<-paste0(outcome_table_stem,"_p") # for prevalence

if (grepl("CPRD", db.name) == TRUE) {
outcome_table_name_han<-paste0(outcome_table_stem,"_o_han") # for incidence
prevalent_table_name_han<-paste0(outcome_table_stem,"_p_han") # for prevalence
}

# QC plot folders ----
qcfolder <- here::here("3_QC",db.name)
if (!file.exists(qcfolder)){
  dir.create(qcfolder, recursive = TRUE)}

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
source(here("2_Analysis","IncidenceAnalysis1.R"))
info(logger, 'INCIDENCE RATE ANALYSIS RAN')

# Run cohort characterisation analysis ----
info(logger, 'RUNNING COHORT CHARACTERISATION ANALYSIS')
#source(here("2_Analysis","CohortCharacteristics.R"))
info(logger, 'COHORT CHARACTERISATION ANALYSIS RAN')

if(runSurvial == TRUE){
# Run survival analysis -----
info(logger, 'RUNNING SURVIVAL ANALYSIS')
source(here("2_Analysis","SurvivalAnalysis1.R"))
info(logger, 'SURVIVAL ANALYSIS RAN') 
}

print("Done!")
print("-- If all has worked, there should now be three zip folders with the incidence/prevalence and survival results for whole datasets and calender years in the output folder to share")
print("-- Thank you for running the study! :)")
Sys.time()-start
readLines(log_file)
