library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)


# Data prep functions -----
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}

#preparing the output and renaming numbers for incidence and prevalence
prepare_output<-function(result){
  result <- result %>%
    
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerHypopharynx", "Hypopharynx")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerLarynx", "Larynx")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerNasalCavitySinus", "Nasal Cavity & Sinus")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerNasopharynx", "Nasopharynx")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOralCavityPrevalent", "Oral Cavity")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOropharynx", "Oropharynx")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerSalivaryGland", "Salivary Gland")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerTonguePrevalent", "Tongue")) %>% 
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOralCavityIncidence", "Oral Cavity")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerTongueIncidence", "Tongue")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentProstateCancer", "Prostate")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentLungCancer", "Lung")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentBreastCancer", "Breast")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentColorectalCancer", "Colorectal")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentHeadNeckCancer", "Head & Neck")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentLiverCancer", "Liver")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentPancreaticCancer", "Pancreas")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentStomachCancer", "Stomach")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentEsophagealCancer", "Esophagus")) %>%    
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentProstateCancer", "Prostate")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLungCancer", "Lung")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentBreastCancer", "Breast")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentColorectalCancer", "Colorectal")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentHeadNeckCancer", "Head & Neck")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLiverCancer", "Liver")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentPancreaticCancer", "Pancreas")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentStomachCancer", "Stomach")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentEsophagealCancer", "Esophagus")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head & Neck")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancerMaleOnly", "Prostate"))
  
  
  result<- result %>% 
    mutate(denominator_age_group= stringr::str_replace(denominator_age_group, ";", " to ")) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "All")) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "90 to 150", "90 +")) %>% 
    mutate(denominator_age_group = factor(denominator_age_group,
                                          levels = c("All",
                                                     "18 to 29", "30 to 39", "40 to 49",
                                                     "50 to 59", "60 to 69", "70 to 79",
                                                     "80 to 89", "90 +" )))
  
  result <- result %>%
    mutate(database_name = replace(database_name, database_name == "CPRDAurum", "CPRD Aurum")) %>%
    mutate(database_name = replace(database_name, database_name == "CPRDGold", "CPRD Gold")) 
  
  #filter out the results for both genders for prostate cancer (as cohort only in male)
  result <- result %>%
    filter(!(outcome_cohort_name == "Prostate" & denominator_sex == "Both")) %>%
    filter(!(outcome_cohort_name == "Prostate" & denominator_sex == "Female")) 
  
  return(result)
} # need to update this for the different files

#preparation the output and renaming numbers for survival
prepare_output_survival <- function(result){
  result <- result %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerHypopharynx", "Hypopharynx")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerLarynx", "Larynx")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerNasalCavitySinus", "Nasal Cavity & Sinus")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerNasopharynx", "Nasopharynx")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOralCavityPrevalent", "Oral Cavity")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOropharynx", "Oropharynx")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerSalivaryGland", "Salivary Gland")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerTonguePrevalent", "Tongue")) %>% 
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOralCavityIncidence", "Oral Cavity")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerTongueIncidence", "Tongue")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentProstateCancer", "Prostate")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentLungCancer", "Lung")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentBreastCancer", "Breast")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentColorectalCancer", "Colorectal")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentHeadNeckCancer", "Head & Neck")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentLiverCancer", "Liver")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentPancreaticCancer", "Pancreas")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentStomachCancer", "Stomach")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "IncidentEsophagealCancer", "Esophagus")) %>%    
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentProstateCancer", "Prostate")) 
  
  
  result<- result %>% 
    mutate(Age= stringr::str_replace(Age, "-", " to ")) %>% 
    mutate(CalenderYearGp= stringr::str_replace(CalenderYearGp, "-", " to ")) %>% 
    mutate(Age = replace(Age, Age == ">=90", "90 +")) %>% 
    mutate(Age = factor(Age,
                        levels = c("All",
                                   "18 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79",
                                   "80 to 89", "90 +" ))) %>%
    mutate(CalenderYearGp = factor(CalenderYearGp,
                                   levels = c("2000 to 2019",
                                              "2000 to 2004", 
                                              "2005 to 2009", 
                                              "2010 to 2014",
                                              "2015 to 2019")))
  
  result <- result %>%
    mutate(Database = replace(Database, Database == "CPRDAurum", "CPRD Aurum")) %>%
    mutate(Database = replace(Database, Database == "CPRDGold", "CPRD Gold")) 
  
  result <- result %>%
    mutate(Gender=replace(Gender, Cancer=="Prostate", "Male"))
  
  return(result)
} 

# Load, prepare, and merge results -----
results <-list.files(here("networkResults"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("networkResults"))
}

#grab the results from the folders
results <- list.files(
  path = here("networkResults"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)



# merge the prevalence estimates
prevalence_estimates_files<-results[stringr::str_detect(results, ".csv")]
prevalence_estimates_files<-results[stringr::str_detect(results, "prevalence_estimates")]


prevalence_estimates <- list()
for(i in seq_along(prevalence_estimates_files)){
  prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
prevalence_estimates <- prepare_output(prevalence_estimates)
prevalence_estimates <- prevalence_estimates %>% 
  mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
                                       paste0(paste0(nice.num3(prevalence*100), "%"), " (",
                                              paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ", 
                                              paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"),
                                       NA
  ))
saveRDS(prevalence_estimates, 
        here("shiny", "data", "prevalence_estimates.rds"))


# prevalence attrition
prevalence_attrition_files<-results[stringr::str_detect(results, ".csv")]
prevalence_attrition_files<-results[stringr::str_detect(results, "prevalence_attrition")]
prevalence_attrition <- list()
for(i in seq_along(prevalence_attrition_files)){
  prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]], 
                                             show_col_types = FALSE)  
}
prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
prevalence_attrition <- prepare_output(prevalence_attrition)
saveRDS(prevalence_attrition, 
        here("shiny", "data", "/prevalence_attrition.rds"))


#merge incidence results together
# incidence estimates
incidence_estimates_files<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files<-results[stringr::str_detect(results, "incidence_estimates")]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)
saveRDS(incidence_estimates, 
        here("shiny", "data", "/incidence_estimates.rds"))

# incidence attrition
incidence_attrition_files<-results[stringr::str_detect(results, ".csv")]
incidence_attrition_files<-results[stringr::str_detect(results, "incidence_attrition")]
incidence_attrition <- list()
for(i in seq_along(incidence_attrition_files)){
  incidence_attrition[[i]]<-readr::read_csv(incidence_attrition_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_attrition <- dplyr::bind_rows(incidence_attrition)
incidence_attrition <- prepare_output(incidence_attrition)
saveRDS(incidence_attrition, 
        here("shiny", "data", "/incidence_attrition.rds"))


# merge the survival results together
survival_estimates_files<-results[stringr::str_detect(results, ".csv")]
survival_estimates_files<-results[stringr::str_detect(results, "survival_estimates")]
survival_estimates <- list()
for(i in seq_along(survival_estimates_files)){
  survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
                                           show_col_types = FALSE)
}
survival_estimates <- dplyr::bind_rows(survival_estimates)
survival_estimates <- prepare_output_survival(survival_estimates)
saveRDS(survival_estimates,
        here("shiny", "data", "/survival_estimates.rds"))





