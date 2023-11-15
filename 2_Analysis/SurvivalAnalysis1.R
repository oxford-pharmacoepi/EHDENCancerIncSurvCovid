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
  dplyr::filter(cohort_start_date <= '2022-12-31') %>% 
  dplyr::mutate(observation_period_end_date_2022 = ifelse(observation_period_end_date >= '2022-12-31', '2022-12-31', NA)) %>%
  dplyr::mutate(observation_period_end_date_2022 = as.Date(observation_period_end_date_2022) ) %>%
  dplyr::mutate(observation_period_end_date_2022 = coalesce(observation_period_end_date_2022, observation_period_end_date)) %>% 
  dplyr::mutate(status = death_date) %>% 
  dplyr::mutate(status = ifelse(death_date > '2022-12-31', NA, status)) %>% 
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
  filter(time_days != 0)

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
observedhazotKM <- list()
observedrisktableKM <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) {
  
  #subset the data by cancer type
  data <- Pop %>%
    dplyr::filter(cohort_definition_id == j)
  
  #carry out km estimate ---
  #take every other row from results
  observedkm[[j]] <- survival::survfit(Surv(time_years, status) ~ 1, data=data) %>%
    tidy() %>%
    dplyr::mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both")
  
  print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohort_name[j], " completed"))
  
  # get risk table for specific times ---
  grid <- seq(0,floor(max(data$time_years)),by=0.5) #get the number of years every half year
  
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
  
}
  
# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined <- dplyr::bind_rows(observedkm) %>%
  dplyr::rename(est = estimate ,ucl = conf.high, lcl = conf.low ) 

medkmcombined <- dplyr::bind_rows(observedmedianKM)

# generate results for risk table with those at risk and censor < 5 cases
risktableskm <- dplyr::bind_rows(observedrisktableKM) %>% 
  dplyr::filter(details != "n.censor") %>% 
  dplyr::mutate(Stratification = "None", Adjustment = "None") %>% 
  dplyr::mutate(across(everything(), ~replace(., .==  0 , NA))) %>%
  dplyr::mutate(across(everything(), ~replace(., .<=  5 , "<5"))) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  replace(is.na(.), "0") 


survivalResults <- observedkmcombined  %>%
  mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                     max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))

#risk table # error with characters and double formats
riskTableResults <- risktableskm  %>%
  mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                     max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))

#median results and sur probs
medianKMResults <- medkmcombined %>%
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
  
  surres[[q]]<-SurResults[[q]]$survival_estimates
  rtres[[q]]<-SurResults[[q]]$risk_table_results
  msres[[q]]<-SurResults[[q]]$median_survival_results
  
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



#################################
# KM for 1 and 2 year (covid paper)
##################################

if (agestandardization == TRUE) {
  
  grid <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  prbtime <- c(0.5,1,1.5,2, 2.5, 3)
  
  SurAnalysis1 <- function(dataset, outcomeCohort) {
    
    # whole population
    observedsurprobsKM <- list()
    observedrisktableKM <- list()
    
    # loop to carry out for each cancer
    for(j in 1:nrow(outcomeCohort)) { 
      
      #subset the data by cancer type
      data <- dataset %>%
        filter(outcome_cohort_id == j)
      
      # get the risk table ---
      observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
        rbind(grid) %>% as.data.frame() %>%
        `colnames<-`(grid) %>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = "Both" ) %>%
        slice(1)
      
      print(paste0("Extract risk table ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      #grab survival probabilities 
      sprob <- survfit(Surv(time_years, status) ~ 1, data=data) %>% 
        summary(times = prbtime, extend = TRUE)
      
      cols <- lapply(c(2:15) , function(x) sprob[x])
      observedsurprobsKM[[j]] <- do.call(data.frame, cols) %>%
        mutate(Method = "Kaplan-Meier", 
               Cancer = outcomeCohort$cohort_name[j],
               Gender = "Both" ,
               Age = "All" )
      
      print(paste0("survival probabilites from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      
    }
    
    # generate the risk table and remove entries < 5 patients
    risktableskm <- dplyr::bind_rows(observedrisktableKM) %>%
      mutate(across(everything(), ~replace(., . ==  0 , NA))) %>%
      mutate(across(everything(), ~replace(., .  <=  5 , "<5"))) %>%
      replace(is.na(.), 0) %>%
      relocate(Cancer) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(Stratification = "None")
    
    #generate probabilities
    sprobkmcombined <- dplyr::bind_rows(observedsurprobsKM) %>%
      mutate(Stratification = "None")
    
    info(logger, 'KM analysis for whole population COMPLETE')
    
    # GENDER STRATIFICATION-----
    
    observedsurprobsKM_gender <- list()
    observedrisktableKM_gender <- list()
    
    # loop to carry out for each cancer
    for(j in 1:nrow(outcomeCohort)) { 
      
      #subset the data by cancer type
      data <- dataset %>%
        filter(outcome_cohort_id == j) 
      
      #creates a test that determines if both genders in the data
      genderlevels <- data %>%
        group_by(gender) %>% summarise(count = n()) %>% tally()
      
      # analysis wont run if only 1 gender present
      if(genderlevels == 2){
        
        # get the risk table ---
        
        observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
          rbind(grid) %>% as.data.frame() %>%
          `colnames<-`(grid) %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All") %>%
          slice(1) %>%
          rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
          mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = c("Male", "Female"))
        
        print(paste0("Extract risk table ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
        
        #grab survival probabilities
        sprob <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
          summary(times = prbtime, extend = TRUE)
        
        cols <- lapply(c(2:16) , function(x) sprob[x])
        observedsurprobsKM_gender[[j]] <- do.call(data.frame, cols) %>%
          rename(Gender = strata) %>%
          mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female"))
        
        print(paste0("survival probabilites from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
        
      } else {
        
        print(paste0("Gender stratification KM analysis not carried out for ", outcomeCohort$cohort_name[j], " due to only 1 gender present " , Sys.time()))
        
      }
      
    } # this closes the loop on the analysis containing both genders
    
    
    #generate the risk table and remove entries < 5 patients
    risktableskm_gender <- dplyr::bind_rows(observedrisktableKM_gender) 
    risktableskm_gender <- risktableskm_gender %>%
      mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.== 0, NA, .))) %>%  
      mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
      replace(is.na(.), 0) %>%
      relocate(Cancer) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(Stratification = "Gender")
    
    #generate probabilities
    sprobkmcombined_gender <- dplyr::bind_rows(observedsurprobsKM_gender) %>%
      mutate(Stratification = "Gender")
    
    info(logger, 'KM analysis for gender stratification COMPLETE')
    
    
    # combine all the survival results -----
    
    #risk table
    riskTableResults <- bind_rows(
      risktableskm , # all
      risktableskm_gender  
    ) %>%
      mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                         max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
    
    #1,5,10 survival probabilites results
    SurvProb1510KMResults <- bind_rows( 
      sprobkmcombined , # all
      sprobkmcombined_gender 
    ) %>%
      mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                         max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
    
    # put results all together in a list
    survival_study_results <- list(
      riskTableResults,
      
      SurvProb1510KMResults)
    
    names(survival_study_results) <- c(paste0("risk_table_results"),
                                       paste0("one_five_ten_survival_rates")
    )
    
    
    print(paste0("Survival Analysis completed"))
    
    return(survival_study_results)
    
  }  
  
  #SurResults1 <- list()
  
  for(l in 1:length(PopAll)) {
    SurResults1[[l]] <- SurAnalysis1(dataset = PopAll[[l]],
                                     outcomeCohort = outcome_cohorts)
  }
  
  # extract calender year results
  rtres <- list()
  oftsrres <- list()
  
  # extract information for calender year (element 1 is whole population so start from 2:n)
  for(q in 2:length(PopAll)) {
    
    rtres[[q]] <- SurResults1[[q]]$risk_table_results
    oftsrres[[q]] <- SurResults1[[q]]$one_five_ten_survival_rates
    
  }
  
  # bind the results for calender years
  risk_table_cy <- bind_rows(rtres)
  survival_prob_cy <- bind_rows(oftsrres)
  
  calenderyr_results <- list(
    risk_table_cy,
    survival_prob_cy)
  
  names(calenderyr_results) <- c(paste0("risk_table_results_cy", db.name),
                                 paste0("one_five_ten_survival_rates_cy", db.name))
  
  # zip results
  print("Zipping results to output folder")
  
  
  #calender year stratification for covid analysis
  exportSurvivalResults(result=calenderyr_results,
                        zipName= paste0(db.name, "CalenderYrSurvivalResults_covid"),
                        outputFolder=here::here("Results", db.name))
  
}




SurAnalysis <- function(dataset, outcomeCohort) {
  
  # whole population
  observedkm <- list()
  observedmedianKM <- list()
  observedsurprobsKM <- list()
  observedrisktableKM <- list()
  
  # loop to carry out for each cancer
  for(j in 1:nrow(outcomeCohort)) { 
    
    #subset the data by cancer type
    data <- dataset %>%
      filter(outcome_cohort_id == j)
    
    #carry out km estimate
    observedkm[[j]] <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
      tidy() %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = "Both") 
    
    print(paste0("KM for observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    # get the risk table ---
    if(ceiling(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")) <=5) {   
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=1) 
    } else {
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=2) 
    }
    
    observedrisktableKM[[j]] <- RiskSetCount(grid,data$time_years) %>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = "Both" ) %>%
      slice(1)
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    # KM median survival---
    modelKM <- survfit(Surv(time_years, status) ~ 1, data=data) %>%
      summary()
    
    observedmedianKM[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%  
      pivot_longer(-rowname) %>% 
      pivot_wider(names_from=rowname, values_from=value) %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcomeCohort$cohort_name[j],
             Gender = "Both" ,
             Age = "All" ) %>%
      select(-name)
    
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    #grab survival probabilities 1,5,10 years
    sprob <- survfit(Surv(time_years, status) ~ 1, data=data) %>% 
      summary(times = c(1,5,10), extend = TRUE)
    
    cols <- lapply(c(2:15) , function(x) sprob[x])
    observedsurprobsKM[[j]] <- do.call(data.frame, cols) %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcomeCohort$cohort_name[j],
             Gender = "Both" ,
             Age = "All" )
    
    print(paste0("survival probabilites for 1,5,10 years from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    
  }
  
  # take the results from a list (one element for each cancer) and put into dataframe for KM survival
  observedkmcombined <- dplyr::bind_rows(observedkm) %>%
    rename(est = estimate , ucl = conf.high, lcl = conf.low ) %>%
    mutate(Stratification = "None")
  
  medkmcombined <- dplyr::bind_rows(observedmedianKM) %>%
    mutate(Stratification = "None")
  
  # generate the risk table and remove entries < 5 patients
  risktableskm <- dplyr::bind_rows(observedrisktableKM) %>%
    mutate(across(everything(), ~replace(., . ==  0 , NA))) %>%
    mutate(across(everything(), ~replace(., .  <=  5 , "<5"))) %>%
    replace(is.na(.), 0) %>%
    relocate(Cancer) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(Stratification = "None")
  
  #generate probabilities
  sprobkmcombined <- dplyr::bind_rows(observedsurprobsKM) %>%
    mutate(Stratification = "None")
  
  info(logger, 'KM analysis for whole population COMPLETE')
  
  # GENDER STRATIFICATION-----
  
  observedkm_gender <- list()
  observedmedianKM_gender <- list()
  observedsurprobsKM_gender <- list()
  observedrisktableKM_gender <- list()
  
  # loop to carry out for each cancer
  for(j in 1:nrow(outcomeCohort)) { 
    
    #subset the data by cancer type
    data <- dataset %>%
      filter(outcome_cohort_id == j) 
    
    # get the risk table ---
    if(ceiling(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")) <=5) {   
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=1) 
    } else {
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=2) 
    }
    
    #creates a test that determines if both genders in the data
    genderlevels <- data %>%
      group_by(gender) %>% summarise(count = n()) %>% tally()
    
    # analysis wont run if only 1 gender present
    if(genderlevels == 2){
      
      # get the risk table ---
      
      observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
        rbind(grid) %>% as.data.frame() %>%
        `colnames<-`(grid) %>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All") %>%
        slice(1) %>%
        rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = c("Male", "Female"))
      
      print(paste0("Extract risk table ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      #carry out km estimate
      observedkm_gender[[j]] <- survfit (Surv(time_years, status) ~ gender, data=data) %>%
        tidy() %>%
        rename(Gender = strata) %>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female")) %>%
        filter(n.risk >= 5) #remove entries with less than 5 patients
      
      print(paste0("KM for observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      # KM median survival ---
      modelKM <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
        summary()
      
      # median survival ---
      observedmedianKM_gender[[j]] <- modelKM$table %>%
        as.data.frame() %>%
        mutate(Method = "Kaplan-Meier", 
               Cancer = outcomeCohort$cohort_name[j], 
               Age = "All" ,
               Gender = c("Male", "Female"))
      
      print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      #grab survival probabilities 1,5,10 years
      sprob <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
        summary(times = c(1,5,10), extend = TRUE)
      
      cols <- lapply(c(2:16) , function(x) sprob[x])
      observedsurprobsKM_gender[[j]] <- do.call(data.frame, cols) %>%
        rename(Gender = strata) %>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female"))
      
      print(paste0("survival probabilites for 1,5,10 years from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
    } else {
      
      print(paste0("Gender stratification KM analysis not carried out for ", outcomeCohort$cohort_name[j], " due to only 1 gender present " , Sys.time()))
      
    }
    
  } # this closes the loop on the analysis containing both genders
  
  # take the results from a list (one element for each cancer) and put into dataframe for KM survival
  observedkmcombined_gender <- dplyr::bind_rows(observedkm_gender) %>%
    rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
    mutate(Stratification = "Gender")
  
  medkmcombined_gender <- dplyr::bind_rows(observedmedianKM_gender) %>%
    mutate(Stratification = "Gender")
  
  #generate the risk table and remove entries < 5 patients
  risktableskm_gender <- dplyr::bind_rows(observedrisktableKM_gender) 
  risktableskm_gender <- risktableskm_gender %>%
    mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    relocate(Cancer) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(Stratification = "Gender")
  
  #generate 1,5,10 probabilities
  sprobkmcombined_gender <- dplyr::bind_rows(observedsurprobsKM_gender) %>%
    mutate(Stratification = "Gender")
  
  info(logger, 'KM analysis for gender stratification COMPLETE')
  
  ###########################
  # AGE STRATIFICATION
  ##########################
  
  observedkm_age <- list()
  observedmedianKM_age <- list()
  observedsurprobsKM_age <- list()
  observedrisktableKM_age <- list()
  
  # loop to carry out for each cancer
  for(j in 1:nrow(outcomeCohort)) { 
    
    #subset the data by cancer type
    data <- dataset %>%
      filter(outcome_cohort_id == j) 
    
    # get the risk table ---
    if(ceiling(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")) <=5) {   
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=1) 
    } else {
      grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=2) 
    }
    observedrisktableKM_age[[j]] <- RiskSetCount(grid,data$time_years[data$age_gr == "18-29"]) %>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
      mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], Gender = "Both", Age = c("18-29" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90")) 
    
    #carry out km estimate
    observedkm_age[[j]] <- survfit (Surv(time_years, status) ~ age_gr, data=data) %>%
      tidy() %>%
      rename(Age = strata) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], 
             Age = str_replace(Age, "age_gr=18-29", "18-29"),
             Age = str_replace(Age, "age_gr=30-39", "30-39"),
             Age = str_replace(Age, "age_gr=40-49", "40-49"),
             Age = str_replace(Age, "age_gr=50-59", "50-59"),
             Age = str_replace(Age, "age_gr=60-69", "60-69"),
             Age = str_replace(Age, "age_gr=70-79", "70-79"),
             Age = str_replace(Age, "age_gr=80-89", "80-89"),
             Age = str_replace(Age, "age_gr=>=90", ">=90"),
             Gender = "Both")
    
    print(paste0("KM for observed data age strat ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    
    # KM median survival---
    modelKM <- survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
      summary()
    
    # to create an age label 
    agelabel <- table(data$age_gr) %>% as.data.frame() %>% filter(Freq != 0)
    
    observedmedianKM_age[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcomeCohort$cohort_name[j], 
             Gender = "Both" ,
             Age = agelabel$Var1)
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
    
    #grab survival probabilities 1,5,10 years
    sprob <- survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
      summary(times = c(1,5,10), extend = TRUE)
    
    cols <- lapply(c(2:16) , function(x) sprob[x])
    observedsurprobsKM_age[[j]] <- do.call(data.frame, cols) %>%
      rename(Age = strata) %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcomeCohort$cohort_name[j], 
             Age = str_replace(Age, "age_gr=18-29", "18-29"),
             Age = str_replace(Age, "age_gr=30-39", "30-39"),
             Age = str_replace(Age, "age_gr=40-49", "40-49"),
             Age = str_replace(Age, "age_gr=50-59", "50-59"),
             Age = str_replace(Age, "age_gr=60-69", "60-69"),
             Age = str_replace(Age, "age_gr=70-79", "70-79"),
             Age = str_replace(Age, "age_gr=80-89", "80-89"),
             Age = str_replace(Age, "age_gr=>=90", ">=90"),
             Gender = "Both")
    
    print(paste0("survival probabilites for 1,5,10 years from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
    
  }
  
  # take the results from a list (one element for each cancer) and put into dataframe ----
  observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
    rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
    mutate(Stratification = "Age")
  
  medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age) %>%
    mutate(Stratification = "Age")
  
  #generate the risk table and obscure entries < 5 patients
  risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age)
  risktableskm_age <- risktableskm_age %>%
    mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    relocate(Cancer) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(Stratification = "Age")
  
  #generate 1,5,10 probabilities
  sprobkmcombined_age <- dplyr::bind_rows(observedsurprobsKM_age) %>%
    mutate(Stratification = "Age")
  
  info(logger, 'KM analysis for AGE stratification COMPLETE')
  
  ##################################################################
  # AGE*GENDER STRATIFICATION
  ##########################
  
  info(logger, 'KM analysis for age*gender stratification START')
  
  # KM observed
  observedkm_age_gender <- list()
  observedmedianKM_age_gender <- list()
  observedsurprobsKM_age_gender <- list()
  observedrisktableKM_age_gender <- list()
  
  # loop to carry out for each cancer
  for(j in 1:nrow(outcomeCohort)) { 
    
    #subset the data by cancer type
    data <- dataset %>%
      filter(outcome_cohort_id == j) 
    
    #creates a test that determines if both genders in the data
    genderlevels <- data %>%
      group_by(gender) %>% summarise(count = n()) %>% tally()
    
    if(genderlevels == 2){
      
      # get the risk table for data ---
      
      if(ceiling(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")) <=5) {   
        grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=1) 
      } else {
        grid <- seq(0,floor(lubridate::time_length(difftime(max(data$outcome_start_date), min(data$outcome_start_date)), "years")),by=2) 
      }
      
      observedrisktableKM_age_gender[[j]] <- RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_18-29"]) %>%
        rbind(grid) %>% as.data.frame() %>%
        `colnames<-`(grid) %>%
        slice(1) %>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_30-39"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_40-49"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_50-59"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_60-69"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_70-79"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_80-89"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_>=90"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_18-29"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_30-39"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_40-49"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_50-59"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_60-69"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_70-79"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_80-89"]))%>%
        rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_>=90"]))%>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j],
               Gender = rep(c("Female", "Male"), each = nlevels(data$age_gr)),
               Age = rep(c("18-29" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"), 2) ) %>%
        unite("GenderAge", c(Gender, Age), remove = FALSE) 
      
      print(paste0("Extract risk table ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      #carry out km estimate ---
      observedkm_age_gender[[j]] <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
        tidy() %>%
        mutate(Method = "Kaplan-Meier", Cancer = outcomeCohort$cohort_name[j], 
               Age = strata ,
               Age = str_replace(Age, "genderAgegp=Female_18-29", "18-29"),
               Age = str_replace(Age, "genderAgegp=Female_30-39", "30-39"),
               Age = str_replace(Age, "genderAgegp=Female_40-49", "40-49"),
               Age = str_replace(Age, "genderAgegp=Female_50-59", "50-59"),
               Age = str_replace(Age, "genderAgegp=Female_60-69", "60-69"),
               Age = str_replace(Age, "genderAgegp=Female_70-79", "70-79"),
               Age = str_replace(Age, "genderAgegp=Female_80-89", "80-89"),
               Age = str_replace(Age, "genderAgegp=Female_>=90", ">=90"),
               Age = str_replace(Age, "genderAgegp=Male_18-29", "18-29"),
               Age = str_replace(Age, "genderAgegp=Male_30-39", "30-39"),
               Age = str_replace(Age, "genderAgegp=Male_40-49", "40-49"),
               Age = str_replace(Age, "genderAgegp=Male_50-59", "50-59"),
               Age = str_replace(Age, "genderAgegp=Male_60-69", "60-69"),
               Age = str_replace(Age, "genderAgegp=Male_70-79", "70-79"),
               Age = str_replace(Age, "genderAgegp=Male_80-89", "80-89"),
               Age = str_replace(Age, "genderAgegp=Male_>=90", ">=90"),       
               Gender = strata,
               Gender = str_replace(Gender, "genderAgegp=Female_18-29", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_30-39", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_40-49", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_50-59", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_60-69", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_70-79", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_80-89", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_>=90", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Male_18-29", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_30-39", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_40-49", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_50-59", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_60-69", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_70-79", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_80-89", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_>=90", "Male"),
               strata = str_replace(strata, "genderAgegp=", "") ) %>%
        rename("GenderAge" = "strata")
      
      print(paste0("KM for observed data age strat ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      
      # KM median survival---
      modelKM <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
        summary()
      
      observedmedianKM_age_gender[[j]] <- modelKM$table %>%
        as.data.frame() %>%
        mutate(Method = "Kaplan-Meier", 
               Cancer = outcomeCohort$cohort_name[j],
               GenderAge = rownames(modelKM$table), 
               GenderAge = str_replace(GenderAge, "genderAgegp=", "")) %>%
        separate(col = "GenderAge", into = c("Gender", "Age"), sep = "_", remove = FALSE)
      
      print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
      #grab survival probabilities 1,5,10 years
      sprob <- survfit(Surv(time_years, status) ~ genderAgegp, data=data) %>%
        summary(times = c(1,5,10), extend = TRUE)
      
      cols <- lapply(c(2:16) , function(x) sprob[x])
      observedsurprobsKM_age_gender[[j]] <- do.call(data.frame, cols) %>%
        mutate(Method = "Kaplan-Meier", 
               Cancer = outcomeCohort$cohort_name[j], 
               Age = strata ,
               Age = str_replace(Age, "genderAgegp=Female_18-29", "18-29"),
               Age = str_replace(Age, "genderAgegp=Female_30-39", "30-39"),
               Age = str_replace(Age, "genderAgegp=Female_40-49", "40-49"),
               Age = str_replace(Age, "genderAgegp=Female_50-59", "50-59"),
               Age = str_replace(Age, "genderAgegp=Female_60-69", "60-69"),
               Age = str_replace(Age, "genderAgegp=Female_70-79", "70-79"),
               Age = str_replace(Age, "genderAgegp=Female_80-89", "80-89"),
               Age = str_replace(Age, "genderAgegp=Female_>=90", ">=90"),
               Age = str_replace(Age, "genderAgegp=Male_18-29", "18-29"),
               Age = str_replace(Age, "genderAgegp=Male_30-39", "30-39"),
               Age = str_replace(Age, "genderAgegp=Male_40-49", "40-49"),
               Age = str_replace(Age, "genderAgegp=Male_50-59", "50-59"),
               Age = str_replace(Age, "genderAgegp=Male_60-69", "60-69"),
               Age = str_replace(Age, "genderAgegp=Male_70-79", "70-79"),
               Age = str_replace(Age, "genderAgegp=Male_80-89", "80-89"),
               Age = str_replace(Age, "genderAgegp=Male_>=90", ">=90"),       
               Gender = strata,
               Gender = str_replace(Gender, "genderAgegp=Female_18-29", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_30-39", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_40-49", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_50-59", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_60-69", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_70-79", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_80-89", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Female_>=90", "Female"),
               Gender = str_replace(Gender, "genderAgegp=Male_18-29", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_30-39", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_40-49", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_50-59", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_60-69", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_70-79", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_80-89", "Male"),
               Gender = str_replace(Gender, "genderAgegp=Male_>=90", "Male"),
               strata = str_replace(strata, "genderAgegp=", "") ) %>%
        rename("GenderAge" = "strata")
      
      print(paste0("survival probabilites for 1, 5,10 years from KM from observed data ", Sys.time()," for ",outcomeCohort$cohort_name[j], " completed"))
      
    } else {
      
      
      print(paste0("Gender*Age stratification KM analysis not carried out for ", outcomeCohort$cohort_name[j], " due to only 1 gender present age stratification will have results " , Sys.time()))
    }
  }
  
  # take the results from a list (one element for each cancer) and put into dataframe ----
  observedkmcombined_age_gender <- dplyr::bind_rows(observedkm_age_gender) %>%
    rename(est = estimate ,ucl = conf.high, lcl = conf.low ) %>%
    mutate(Stratification = "Age*Gender")
  
  medkmcombined_age_gender <- dplyr::bind_rows(observedmedianKM_age_gender)  %>%
    mutate(Stratification = "Age*Gender")
  
  #generate the risk table and remove entries < 5 patients
  risktableskm_age_gender <- dplyr::bind_rows(observedrisktableKM_age_gender) 
  
  risktableskm_age_gender <- risktableskm_age_gender %>%
    mutate_at(.vars = c(1:(ncol(risktableskm_age_gender)-5)), funs(ifelse(.== 0, NA, .))) %>%
    mutate_at(.vars = c(1:(ncol(risktableskm_age_gender)-5)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    relocate(Cancer) %>%
    mutate(across(everything(), as.character))  %>%
    mutate(Stratification = "Age*Gender")
  
  #generate 1,2,3,4,5,10 probabilities
  sprobkmcombined_age_gender <- dplyr::bind_rows(observedsurprobsKM_age_gender) %>%
    mutate(Stratification = "Age*Gender")
  
  
  info(logger, 'KM analysis for AGE*GENDER stratification COMPLETE')
  
  # combine all the survival results -----
  survivalResults <- bind_rows(
    observedkmcombined , # all 
    observedkmcombined_gender , # gender strat 
    observedkmcombined_age , # age strat
    observedkmcombined_age_gender # age gender strat 
  ) %>%
    mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                       max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
  
  #risk table # error with characters and double formats
  riskTableResults <- bind_rows(
    risktableskm , # all
    risktableskm_gender , # gender strat
    risktableskm_age , # age strat
    risktableskm_age_gender # age*gender strat 
  ) %>%
    mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                       max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
  
  #median results
  medianKMResults <- bind_rows( 
    medkmcombined , # all
    medkmcombined_gender , # gender
    medkmcombined_age , # age strat
    medkmcombined_age_gender # age*gender strat 
  ) %>%
    mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                       max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
  
  #1,5,10 survival probabilites results
  SurvProb1510KMResults <- bind_rows( 
    sprobkmcombined , # all
    sprobkmcombined_gender , # gender
    sprobkmcombined_age , # age strat
    sprobkmcombined_age_gender # age*gender strat 
  ) %>%
    mutate(Database = db.name, CalenderYearGp = paste0(min(lubridate::year(lubridate::ymd(data$cohort_end_date))),"-",
                                                       max(lubridate::year(lubridate::ymd(data$cohort_end_date)))))
  
  # put results all together in a list
  survival_study_results <- list(survivalResults ,
                                 riskTableResults,
                                 medianKMResults,
                                 SurvProb1510KMResults)
  
  names(survival_study_results) <- c(paste0("survival_estimates"),
                                     paste0("risk_table_results"),
                                     paste0("median_survival_results"),
                                     paste0("one_five_ten_survival_rates")
  )
  
  
  print(paste0("Survival Analysis completed"))
  
  return(survival_study_results)
  
}
