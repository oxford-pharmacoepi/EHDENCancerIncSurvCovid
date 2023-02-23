library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)


# Data prep functions -----
# study specific reformatting of results
prepare_output<-function(result){
  # result <- result %>% 
  #   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "New_AS_with_PsO", "Ankylosing spondylitis with psoriasis")) %>% 
  #   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "psoriatic_arthritis", "Psoriatic arthritis")) %>% 
  #   mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "psoriasis", "Psoriasis")) 
  result<- result %>% 
    mutate(denominator_age_group= stringr::str_replace(denominator_age_group, ";", " to ")) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "All")) %>% 
    mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "90 to 150", "90 +")) %>% 
    mutate(denominator_age_group = factor(denominator_age_group,
                                          levels = c("All",
                                                     "18 to 29", "30 to 39", "40 to 49",
                                                     "50 to 59", "60 to 69", "70 to 79",
                                                     "80 to 89", "90 +" )))
  
  return(result)
}

# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}

# Load, prepare, and merge results -----
results<-list.files(here("networkResults"), full.names = TRUE)

# prevalence estimates
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
          here("shiny", "data", "prevalence_attrition.rds"))


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
          here("shiny", "data", "incidence_estimates.rds"))

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
          here("shiny", "data", "incidence_attrition.rds"))
