# KM survival analysis ---

cdm$analysis <- cdm$cancer_table_one %>% 
dplyr::left_join(cdm$death %>% 
                   select("person_id",  "death_date") %>% 
                   distinct(),
                 by = c("subject_id"= "person_id")) %>% 
  dplyr::left_join(cdm$observation_period %>% 
                     select("person_id",  "observation_period_end_date") %>% 
                     distinct(),
                   by = c("subject_id"= "person_id")) %>% 
  CDMConnector::computeQuery() %>% 
  dplyr::filter(cohort_start_date >= as.Date(studyStartDate)) %>% 
  dplyr::filter(cohort_start_date <= '2022-12-15') %>% 
  dplyr::mutate(observation_period_end_date_2022 = ifelse(observation_period_end_date >= '2022-12-15', '2022-12-15', NA)) %>%
  dplyr::mutate(observation_period_end_date_2022 = as.Date(observation_period_end_date_2022) ) %>%
  dplyr::mutate(observation_period_end_date_2022 = coalesce(observation_period_end_date_2022, observation_period_end_date)) %>% 
  dplyr::mutate(status = death_date) %>% 
  dplyr::mutate(status = ifelse(death_date > '2022-12-15', NA, status)) %>% 
  dplyr::mutate(status = ifelse(death_date > observation_period_end_date_2022, NA, status)) %>% 
  dplyr::mutate(status = ifelse(is.na(status), 1, 2 )) %>% 
  dplyr::mutate(time_days = observation_period_end_date_2022 - cohort_start_date ) %>% 
  dplyr::mutate(time_years=time_days/365) %>% 
  dplyr::filter(age_gr != "None") %>% 
  dplyr::mutate(sex_age_gp = str_c(age_gr, sex, sep = "_"),
                future_observation = time_days) %>%
  CDMConnector::computeQuery()

# see if there is prostate cancer in database then run this code and put in both if statements
# remove females from prostate cancer cohort (misdiagnosis)
# get cohort definition id for prostate cancer
if( "IncidentProstateCancer" %in% outcome_cohorts$cohort_name == TRUE){
  
  prostateID <- outcome_cohorts %>% 
    dplyr::filter(outcome_cohorts$cohort_name == "IncidentProstateCancer") %>% 
    dplyr::select(cohort_definition_id) %>% 
    as.numeric()
  
  # remove females from prostate cancer cohort (misdiagnosis)
  cdm$analysis <- cdm$analysis %>% 
    dplyr::filter(!(sex == "Female" & cohort_definition_id == prostateID))
}

cdm$analysis <- cdm$analysis %>% 
  filter(time_days > 0)

#update the attrition
cdm$analysis <- CDMConnector::recordCohortAttrition(cohort = cdm$analysis,
                                                    reason="Excluding those with date of death and cancer diagnosis on same date" )


# record the attrition
attritioncdm <- CDMConnector::cohort_attrition(cdm$analysis) %>% 
  dplyr::left_join(outcome_cohorts, 
                   by = join_by(cohort_definition_id),
                   relationship = "many-to-many",
                   keep = FALSE
  ) %>% 
  dplyr::select(!c(cohort, json)) %>% 
  dplyr::relocate(cohort_name) %>% 
  dplyr::mutate(Database = cdm_name(cdm)) %>% 
  dplyr::rename(Cancer = cohort_name)

readr::write_csv(attritioncdm, paste0(here::here(output.folder),"/", cdm_name(cdm), "_survival_attrition.csv"))


# collect the dataset
Pop <- cdm$analysis %>% collect()

#OUTPUT data for whole dataset and strata based on calender year
PopAll <- DataExtraction(dataset = Pop)

### KAPLAIN MEIER CODE ####
# INPUT
# dataset is a dataframe containing the data as processed above
# outcome cohort is a dataframe containing information about cancers
# OUTPUT
# list containing 4 data frames 1) survival estimates 2) risktables 3) median survival 4) survival probabilities

tic("KM analysis for whole population")
info(logger, 'KM analysis for whole population START')

SurAnalysis <- function(dataset, outcomeCohort) {
# capture output in list
observedkm <- list()
observedmedianKM <- list()
observedrisktableKM <- list()
observedkm_sex <- list()
observedmedianKM_sex <- list()
observedrisktableKM_sex <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
  #subset the data by cancer type
  data <- dataset %>%
    dplyr::filter(cohort_definition_id == j)
  
  #carry out km estimate ---
  #take every other row from results
  observedkm[[j]] <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>%
    tidy() %>%
    dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both")
  
  print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
  
  # get risk table for specific times ---
  grid <- seq(0,(max(data$time_years)),by=0.5)  
  sprob <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>% 
    summary(times = grid, extend = TRUE)
  cols <- lapply(c(2:15) , function(x) sprob[x])
  
  observedrisktableKM[[j]] <- do.call(data.frame, cols) %>%
    dplyr::select(c(n.risk, n.event, n.censor)) %>% 
    t() %>% 
    as_tibble() %>% 
    `colnames<-`(grid) %>% 
    dplyr::mutate(Method = "Kaplan-Meier", 
                  Cancer = outcome_cohorts$cohort_name[j],
                  Sex = "Both" ,
                  Age = "All",
                  details = c("n.risk", "n.event", "n.censor")) %>% 
    dplyr::relocate(details)
  
  print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
  
  
  # get surv probs for specific times ---
  surprobsKM <- do.call(data.frame, cols) %>%
    dplyr::select(c(time, surv, lower, upper)) %>% 
    dplyr::filter(time == 0.5 |
                    time == 1 |
                    time == 1.5 |
                    time == 2 |
                    time == 2.5 |
                    time == 3 |
                    time == 5  ) %>% 
    dplyr::mutate(surv = round((surv*100),4),
                  lower = round((lower*100),4),
                  upper = round((upper*100),4),
                  "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                     paste0(paste0(nice.num1(surv)), " (",
                                                            paste0(nice.num1(lower)),"-",
                                                            paste0(nice.num1(upper)), ")"),
                                                     NA)) %>% 
    dplyr::select(-c(lower, upper)) %>% 
    tidyr::pivot_wider(names_from = time, 
                       values_from = c(`Survival Rate % (95% CI)`, surv),
                       names_prefix = " year ",
                       names_sep = "")
  
  # KM median survival---
  modelKM <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>%
    summary()
  
  medianKM <- modelKM$table %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%  
    tidyr::pivot_longer(-rowname) %>% 
    tidyr::pivot_wider(names_from=rowname, values_from=value) %>% 
    dplyr::rename(n = records, se =`se(rmean)`) %>% 
    dplyr::mutate(rmean = round(rmean, 4),
                  median = round(median, 4),
                  `0.95LCL` = round(`0.95LCL`, 4),
                  `0.95UCL` = round(`0.95UCL`, 4),
                  "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                paste0(paste0(nice.num2(rmean)), " (",
                                                       paste0(nice.num2(se)), ")"),
                                                NA),
                  "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                              paste0(paste0(nice.num2(median)), " (",
                                                                     paste0(nice.num2(`0.95LCL`)),"-",
                                                                     paste0(nice.num2(`0.95UCL`)), ")"),
                                                              NA)) %>% 
    dplyr::select(-c(`0.95LCL`,`0.95UCL`, name, n.max, n.start)) %>% 
    dplyr::mutate(n  = replace(n, n ==  0 , NA),
                  events = replace(events, events ==  0 , NA)) %>%
    dplyr::mutate(n  = replace(n, n <=  5 , "<5"),
                  events  = replace(events, events <=  5 , "<5"))  %>%
    dplyr::mutate(n  = replace_na(n, "0"),
                  events  = replace_na(events, "0")) %>% 
    dplyr::mutate(n = as.character(n),
                  events = as.character(events))
  
  
  observedmedianKM[[j]] <- dplyr::bind_cols(medianKM, surprobsKM)
  observedmedianKM[[j]] <- observedmedianKM[[j]] %>% 
    dplyr::mutate(Method = "Kaplan-Meier", 
                  Cancer = outcome_cohorts$cohort_name[j] ,
                  Age = "All", 
                  Sex = "Both" )
  
  rm(surprobsKM,medianKM,modelKM)
  
  print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
  
  
  sexlevels <- data %>%
    dplyr::group_by(sex) %>% dplyr::summarise(count = n()) %>% dplyr::tally()
  
  if(sexlevels == 2){
    
    #carry out km estimate
    observedkm_sex[[j]] <- survival::survfit(Surv(time_years, status) ~ sex, data=data) %>%
      tidy() %>%
      dplyr::rename(Sex = strata) %>%
      dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = str_replace(Sex, "sex=Male", "Male"), 
                    Sex = str_replace(Sex,"sex=Female", "Female")) 
    
    print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    
    # get risk table for specific times for each sex then combine again ---
    grid <- seq(0,floor(max(data$time_years)),by=0.5) #get the number of years
    sprob <- survival::survfit(Surv(time_years, status) ~ sex, data=data) %>% 
      summary(times = grid, extend = TRUE)
    cols <- lapply(c(2:16) , function(x) sprob[x])
    
    kmsex <- do.call(data.frame, cols) %>%
      dplyr::select(c(n.risk, n.event, n.censor, strata)) %>% 
      dplyr::rename(Sex = strata) %>% 
      dplyr::mutate(Sex = str_replace(Sex, "sex=", ""))
    
    # risk table for females       
    kmsex_f <- kmsex %>% 
      dplyr::filter(Sex == "Female") %>% 
      dplyr::select(!c(Sex)) %>% 
      t() %>% 
      as_tibble() %>% 
      `colnames<-`(grid) %>%
      dplyr::mutate(Method = "Kaplan-Meier", 
                    Cancer = outcome_cohorts$cohort_name[j],
                    Age = "All",
                    Sex = "Female" ,
                    details = c("n.risk", "n.event", "n.censor")) %>% 
      dplyr::relocate(details)
    
    # risk table for males
    kmsex_m <- kmsex %>% 
      dplyr::filter(Sex == "Male") %>% 
      dplyr::select(!c(Sex)) %>% 
      t() %>% 
      as_tibble() %>% 
      `colnames<-`(grid) %>%
      dplyr::mutate(Method = "Kaplan-Meier", 
                    Cancer = outcome_cohorts$cohort_name[j],
                    Age = "All",
                    Sex = "Male" ,
                    details = c("n.risk", "n.event", "n.censor")) %>% 
      dplyr::relocate(details)       
    
    # bind results for both sexes
    observedrisktableKM_sex[[j]] <- bind_rows(kmsex_f, kmsex_m )
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    # survival probabilities ----
    surprobsKM <- do.call(data.frame, cols) %>%
      dplyr::select(c(time, surv, lower, upper, strata)) %>% 
      dplyr::rename(Sex = strata) %>% 
      dplyr::filter(time == 0.5 |
                      time == 1 |
                      time == 1.5 |
                      time == 2 |
                      time == 2.5 |
                      time == 3 |
                      time == 5  ) %>% 
      dplyr::mutate(Sex = str_replace(Sex, "sex=", "")) %>% 
      dplyr::mutate(surv = round((surv*100),4),
                    lower = round((lower*100),4),
                    upper = round((upper*100),4),
                    "Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                                       paste0(paste0(nice.num1(surv)), " (",
                                                              paste0(nice.num1(lower)),"-",
                                                              paste0(nice.num1(upper)), ")"),
                                                       NA)) %>% 
      dplyr::select(-c(lower, upper)) %>% 
      tidyr::pivot_wider(names_from = time, 
                         values_from = c(`Survival Rate % (95% CI)`, surv),
                         names_prefix = " year ",
                         names_sep = "")
    
    # KM median survival ---
    modelKM <- survival::survfit(Surv(time_years, status) ~ sex, data=data) %>%
      summary()
    
    medianKM <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      dplyr::rename(Sex = rowname, n = records, se =`se(rmean)`) %>% 
      dplyr::mutate(rmean = round(rmean, 4),
                    median = round(median, 4),
                    Sex = str_replace(Sex, "sex=", ""),
                    `0.95LCL` = round(`0.95LCL`, 4),
                    `0.95UCL` = round(`0.95UCL`, 4),
                    "rmean in years (SE)"= ifelse(!is.na(rmean),
                                                  paste0(paste0(nice.num2(rmean)), " (",
                                                         paste0(nice.num2(se)), ")"),
                                                  NA),
                    "Median Survival in Years (95% CI)"= ifelse(!is.na(median),
                                                                paste0(paste0(nice.num2(median)), " (",
                                                                       paste0(nice.num2(`0.95LCL`)),"-",
                                                                       paste0(nice.num2(`0.95UCL`)), ")"),
                                                                NA)) %>% 
      dplyr::select(-c(`0.95LCL`,`0.95UCL`, n.max, n.start)) %>% 
      dplyr::mutate(n  = replace(n, n ==  0 , NA),
                    events = replace(events, events ==  0 , NA)) %>%
      dplyr::mutate(n  = replace(n, n <=  5 , "<5"),
                    events  = replace(events, events <=  5 , "<5"))  %>%
      dplyr::mutate(n  = replace_na(n, "0"),
                    events  = replace_na(events, "0")) %>% 
      dplyr::mutate(n = as.character(n),
                    events = as.character(events))
    
    observedmedianKM_sex[[j]] <- dplyr::inner_join(medianKM, surprobsKM, by = "Sex" ) %>% 
      dplyr::mutate(Method = "Kaplan-Meier", 
                    Cancer = outcome_cohorts$cohort_name[j] ,
                    Age = "All")
    
    rm(surprobsKM,medianKM,modelKM)
    
    print(paste0("Median and rmean survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
    
    
  } else {
    
    print(paste0("sex stratification KM analysis not carried out for ", outcome_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
  
  
}
  
# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low ) 

# median results
medkmcombined <- dplyr::bind_rows(observedmedianKM)

# generate results for risk table with those at risk and censor < 5 cases
risktableskm <- dplyr::bind_rows(observedrisktableKM) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0") 

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_sex <- dplyr::bind_rows(observedkm_sex) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low ) 

medkmcombined_sex <- dplyr::bind_rows(observedmedianKM_sex) 

#generate the risk table and remove entries < 10 patients
risktableskm_sex <- dplyr::bind_rows(observedrisktableKM_sex) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0")


survivalResults <- bind_rows(observedkmcombined, observedkmcombined_sex)  %>%
  mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                     max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))

#risk table # error with characters and double formats
riskTableResults <- bind_rows(risktableskm, risktableskm_sex)  %>%
  mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                     max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))

#median results and sur probs
medianKMResults <- bind_rows(medkmcombined, medkmcombined_sex)  %>%
  mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                     max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))

# put results all together in a list
survival_study_results <- list(survivalResults ,
                               riskTableResults,
                               medianKMResults)

names(survival_study_results) <- c(paste0("survival_estimates"),
                                   paste0("risk_table_results"),
                                   paste0("median_survival_results"))


print(paste0("Survival Analysis completed"))

return(survival_study_results)

}

toc(func.toc=toc_min)
info(logger, 'KM analysis for whole population COMPLETE')


# create a loop which carries the analysis out on the number of calender year groups (all data plus the calender time splits)
SurResults <- list()

for(l in 1:length(PopAll)) {
  SurResults[[l]] <- SurAnalysis(dataset = PopAll[[l]],
                       outcomeCohort = outcome_cohorts)
}

# extract results for the whole population
whole_pop_results <- list(
  SurResults[[1]]$survival_estimates ,
  SurResults[[1]]$risk_table_results ,
  SurResults[[1]]$median_survival_results
  )

names(whole_pop_results) <- c(paste0("survival_estimates", db.name),
                               paste0("risk_table_results", db.name),
                               paste0("median_survival_results", db.name))

# extract calender year results
surres <- list()
rtres <- list()
msres <- list()

# extract information for calender year (element 1 is whole population so start from 2:n)
for(q in 2:length(PopAll)) {
  
  surres[[q]] <-SurResults[[q]]$survival_estimates
  rtres[[q]] <-SurResults[[q]]$risk_table_results
  msres[[q]] <-SurResults[[q]]$median_survival_results
  
}

# bind the results for calender years
survival_results_cy <- bind_rows(surres)
risk_table_cy <- bind_rows(rtres)
med_surv_results_cy <- bind_rows(msres)

calenderyr_results <- list(
  survival_results_cy,
  risk_table_cy,
  med_surv_results_cy)

names(calenderyr_results) <- c(paste0("survival_estimates_cy", db.name),
                                   paste0("risk_table_results_cy", db.name),
                                   paste0("median_survival_results_cy", db.name))

# zip results
print("Zipping results to output folder")

#whole database
exportSurvivalResults(result=whole_pop_results,
                      zipName= paste0(db.name, "WholeSurvivalResults"),
                      outputFolder=here::here("Results", db.name))

#calender year stratification
exportSurvivalResults(result=calenderyr_results,
                      zipName= paste0(db.name, "CalenderYrSurvivalResults"),
                      outputFolder=here::here("Results", db.name))
