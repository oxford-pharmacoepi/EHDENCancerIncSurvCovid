# table names----
incidence_table_name <- "incidence" 
prevalent_table_name <- "prevalence"
feature_disease_table_name <- "conditions"
feature_medication_table_name <- "medications"

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

#formatting for table 1
nice.num1<-function(x) {
  base::trimws(format(round(x,1),
                      big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}

nice.num2<-function(x) {
  base::trimws(format(round(x,2),
                      big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

reformat_table_one <- function(table_one_summary){
  
  reformatted_table1 <- data.frame(x = character(),  y = character())
  
  n1 <- table_one_summary %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
                                         data.frame(x = paste0("n"),
                                                    y = paste0(n1)))
  
  # variables assembled by min/max etc
  cat_var <- table_one_summary %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- dplyr::bind_rows(reformatted_table1,
                                           data.frame(x = paste0(cat_var[[i]], ": median (IQR)"),
                                                      y = paste0(table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                                 " (",
                                                                 table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                                 " - ",
                                                                 table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                                 ")"))
    )
  }
  
  # age group variables
  age_var <- table_one_summary %>%
    dplyr::filter(variable == "Age group") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  for (i in (1:length(age_var))){
    reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0("Age Group: ", age_var[[i]], " n (%)"),
                                                                          y = paste0(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                     " (",
                                                                                     round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                     ")")) )
  }
  
  
  #condition variables
  condition_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Conditions flag -inf')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  if(length(condition_var) != 0) {
    for (i in (1:length(condition_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(condition_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")")))
    }
  }
  
  #medication variables
  medication_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Medications flag -365')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  
  if(length(medication_var) != 0) {
    for (i in (1:length(medication_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(medication_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")")))
    } 
  }
  
  #cancer outcomes  
  outcome_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Outcome flag 0 to 0')) %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)
  
  
  if(length(outcome_var) != 0) {
    for (i in (1:length(outcome_var))){
      reformatted_table1 <- dplyr::bind_rows(reformatted_table1, data.frame(x = paste0(outcome_var[[i]], " n (%)"),
                                                                            y = paste0(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                       " (",
                                                                                       round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == outcome_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                       ")"))) 
      
    } 
  }
  
  
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()
  
  ###rename columns
  colnames(reformatted_table1) <- c("Description", "Value") 
  
  return(reformatted_table1)
  
}

#FUNCTION to extract the data and calculate the correct observation time and event (death) for different calender strata
DataExtraction <- function(dataset){
  data <- dataset
  
  #make new end of observation period to studyEndDate parameter ----
  grid <- rev(seq(2019, min(lubridate::year(lubridate::ymd(dataset$cohort_start_date))),by=-5))
  
  # create a tool which creates 5 year age gaps and follows up for 5 years
  # now need to create the start and end dates for each one
  startYear <- paste0(grid-4,"-01-01") # first days
  endYear <- paste0(grid,"-12-31") # end days (plus 4 to create 5 year bands)
  
  #add on extra times for 2020-22 (3 years)
  startYear <- c(startYear , "2020-01-01")
  endYear <- c(endYear , "2022-12-31")
  
  # split data into groups of calender year and put it into a list. This will create 4 groups split by calender year of diagnosis with 5 years follow up
  calenderSplitData <- list()
  
  for(w in 1:length(endYear)){
    
    calenderdata <- dataset %>%
      filter( outcome_start_date >= startYear[w] &  
                outcome_start_date <= endYear[w] )
    
    if (startYear[w] == "2020-01-01"){
      
      calenderdata <- calenderdata %>%
        mutate(endOfObservation = outcome_start_date + 1095.75) %>%
        mutate(endOfObservation = dplyr::if_else(observation_period_end_date >= endOfObservation, endOfObservation, NA)) %>%
        mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date)) %>% 
        mutate(endOfObservation = dplyr::if_else(endOfObservation > lubridate::as_date(endYear[w]), lubridate::as_date(endYear[w]), endOfObservation))
      
      
    } else {
      
      calenderdata <- calenderdata %>%
        mutate(endOfObservation = outcome_start_date + 1826.25) %>%
        mutate(endOfObservation = dplyr::if_else(observation_period_end_date >= endOfObservation, endOfObservation, NA)) %>%
        mutate(endOfObservation = coalesce(endOfObservation, observation_period_end_date))
      
    }
    
    # binary death outcome (for survival) ---
    # need to take into account follow up
    # if death date is > database end data set death to 1
    calenderdata <- calenderdata %>%
      mutate(status = dplyr::if_else(!is.na(death_date), 2, 1 )) %>%
      mutate(status = dplyr::if_else(death_date > endOfObservation , 1, status )) %>%
      mutate(status = dplyr::if_else(is.na(status), 1, status ))
    
    
    # calculate follow up in years
    calenderdata <- calenderdata %>%
      mutate(time_days=as.numeric(difftime(endOfObservation,
                                           outcome_start_date,
                                           units="days"))) %>%
      mutate(time_years=time_days/365.25)
    
    
    calenderSplitData[[w]] <- calenderdata
    
  }
  
  
  return(c(list(data),calenderSplitData))
  
  
}

agestandardization <- TRUE

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
source(here("2_Analysis","CohortCharacteristics3.R"))
info(logger, 'COHORT CHARACTERISATION ANALYSIS RAN')

# Run survival analysis -----
info(logger, 'RUNNING SURVIVAL ANALYSIS')
source(here("2_Analysis","SurvivalAnalysis1.R"))
info(logger, 'SURVIVAL ANALYSIS RAN')


print("Done!")
print("-- If all has worked, there should now be zip folders with the incidence/prevalence and survival results in the output folder to share")
print("-- Thank you for running the study! :)")
Sys.time()-start
readLines(log_file)
