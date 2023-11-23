#### PACKAGES -----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(ggalt)
library(bslib)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#605ca8" 
  ),
  adminlte_sidebar(
    dark_bg = "#78B7C5", #  "#D8DEE9",
    dark_hover_bg = "#3B9AB2", #"#81A1C1",
    dark_color ="white" ,
    dark_submenu_bg = "#605ca8"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
    #content_bg = "white" 
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446",
    table_border_color = "black"
    
  )
)

# Data prep functions -----
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}

nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}

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
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentEsophagealCancer", "Oesophagus")) %>%    
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentProstateCancer", "Prostate")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLungCancer", "Lung")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentBreastCancer", "Breast")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentColorectalCancer", "Colorectal")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentHeadNeckCancer", "Head & Neck")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLiverCancer", "Liver")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentPancreaticCancer", "Pancreas")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentStomachCancer", "Stomach")) %>%
    mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentEsophagealCancer", "Oesophagus")) %>%
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
    mutate(cdm_name = replace(cdm_name, cdm_name == "CPRDGold", "CPRD GOLD")) 
  
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
    mutate(Cancer = replace(Cancer, Cancer == "IncidentEsophagealCancer", "Oesophagus")) %>%    
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentProstateCancer", "Prostate")) 
  
  
  result<- result %>% 
    mutate(Age = replace(Age, Age == ">=90", "90 +")) %>% 
    mutate(Age = replace(Age, Age == "<30", "18-29")) %>% 
    mutate(Age= stringr::str_replace(Age, "-", " to ")) %>% 
    mutate(CalenderYearGp= stringr::str_replace(CalenderYearGp, "-", " to ")) %>% 
    mutate(Age = factor(Age,
                        levels = c("All",
                                   "18 to 29", "30 to 39", "40 to 49",
                                   "50 to 59", "60 to 69", "70 to 79",
                                   "80 to 89", "90 +" ))) %>%
    mutate(CalenderYearGp = factor(CalenderYearGp,
                                   levels = c("2000 to 2022",
                                              "2000 to 2021",
                                              "2000 to 2004", 
                                              "2005 to 2009", 
                                              "2010 to 2014",
                                              "2015 to 2019",
                                              "2020 to 2022")))
  
  
  result <- result %>%
    mutate(Database = replace(Database, Database == "CPRDGold", "CPRD GOLD")) 
  
  result <- result %>%
    mutate(Sex=replace(Sex, Cancer=="Prostate", "Male"))
  
  return(result)
} 

##preparation the output for table 1
prepare_output_table1 <- function(result){
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
    mutate(Cancer = replace(Cancer, Cancer == "IncidentEsophagealCancer", "Oesophagus")) %>%    
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentProstateCancer", "Prostate")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentBreastCancer", "Breast")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentColorectalCancer", "Colorectal")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentEsophagealCancer", "Oesophagus")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentHeadNeckCancer", "Head & Neck")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentLiverCancer", "Liver")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentLungCancer", "Lung")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentPancreaticCancer", "Pancreas")) %>%
    mutate(Cancer = replace(Cancer, Cancer == "PrevalentStomachCancer", "Stomach")) 
  
  
  
  
  
  result <- result %>%
    mutate(Database = replace(Database, Database == "CPRDAurum", "CPRD Aurum")) %>%
    mutate(Database = replace(Database, Database == "CPRDGoldUpdate2", "CPRD GOLD")) 
  
  
  
  return(result)
} 

# Load, prepare, and merge results -----
results <-list.files(here("data"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("data"))
}

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# merge the prevalence estimates
prevalence_estimates_files<-results[stringr::str_detect(results, ".csv")]
prevalence_estimates_files<-results[stringr::str_detect(results, "period_prevalence")]

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


#merge incidence results together
# incidence estimates
incidence_estimates_files<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files<-results[!(stringr::str_detect(results, "incidence_attrition"))]
incidence_estimates_files<-incidence_estimates_files[(stringr::str_detect(incidence_estimates_files, "incidence_"))]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)


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

# merge the survival results together
survival_estimates_files <- results[stringr::str_detect(results, ".csv")]
survival_estimates_files <- results[stringr::str_detect(results, "survival_estimates")]

survival_estimates <- list()
for(i in seq_along(survival_estimates_files)){
  survival_estimates[[i]]<-readr::read_csv(survival_estimates_files[[i]],
                                           show_col_types = FALSE)
}
survival_estimates <- dplyr::bind_rows(survival_estimates)
survival_estimates <- prepare_output_survival(survival_estimates)


# merge the risk table results together (whole dataset)
survival_risk_table_files<-results[stringr::str_detect(results, ".csv")]
survival_risk_table_files<-results[stringr::str_detect(results, "risk_table_results")]
survival_risk_table_files <- survival_risk_table_files[!stringr::str_detect(survival_risk_table_files, "risk_table_results_cy")]

survival_risk_table <- list()
for(i in seq_along(survival_risk_table_files)){
  survival_risk_table[[i]]<-readr::read_csv(survival_risk_table_files[[i]],
                                            show_col_types = FALSE) %>%
    mutate_if(is.double, as.character)
  
}

survival_risk_table <- dplyr::bind_rows(survival_risk_table)
survival_risk_table <- prepare_output_survival(survival_risk_table)


# merge the risk table results together (calender year results)
survival_risk_table_cy_files<-results[stringr::str_detect(results, ".csv")]
survival_risk_table_cy_files<-results[stringr::str_detect(results, "risk_table_results_cy")]
survival_risk_cy_table <- list()
for(i in seq_along(survival_risk_table_cy_files)){
  survival_risk_cy_table[[i]]<-readr::read_csv(survival_risk_table_cy_files[[i]],
                                               show_col_types = FALSE)  %>%
    mutate_if(is.double, as.character)
}
survival_risk_cy_table <- dplyr::bind_rows(survival_risk_cy_table)
survival_risk_cy_table <- prepare_output_survival(survival_risk_cy_table)


# median results whole database
survival_median_files<-results[stringr::str_detect(results, ".csv")]
survival_median_files<-results[stringr::str_detect(results, "median_survival")]
survival_median_files<-survival_median_files[!(stringr::str_detect(survival_median_files, "median_survival_results_cy"))]
survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE)  
}
survival_median_table <- dplyr::bind_rows(survival_median_table)
survival_median_table <- prepare_output_survival(survival_median_table)

# median results cy database
survival_median_files_cy <-results[stringr::str_detect(results, ".csv")]
survival_median_files_cy <-results[stringr::str_detect(results, "median_survival_results_cy")]
survival_median_table_cy <- list()
for(i in seq_along(survival_median_files_cy)){
  survival_median_table_cy[[i]]<-readr::read_csv(survival_median_files_cy[[i]],
                                              show_col_types = FALSE)  
}
survival_median_table_cy <- dplyr::bind_rows(survival_median_table_cy)
survival_median_table_cy <- prepare_output_survival(survival_median_table_cy)

# round the values before turning them into characters
# survival_median_table <-  survival_median_table %>% 
#   mutate(rmean = nice.num3(rmean)) %>% 
#   mutate(`se(rmean)` = nice.num3(`se(rmean)`)) %>%
#   mutate(median = nice.num3(median)) %>%
#   mutate(`0.95LCL` = nice.num3(`0.95LCL`)) %>%
#   mutate(`0.95UCL` = nice.num3(`0.95UCL`)) %>% 
#   mutate(median = ifelse(median == "NA", NA, median)) %>% 
#   mutate(`0.95LCL` = ifelse(`0.95LCL` == "NA", NA, `0.95LCL`)) %>% 
#   mutate(`0.95UCL` = ifelse(`0.95UCL` == "NA", NA, `0.95UCL`)) 

# 
# #if events less than 5 turn the result into NA
# survival_median_table <-  
#   survival_median_table %>% 
#   mutate(events = ifelse(events <= 5, "<5", events)) %>% 
#   mutate(median = ifelse(events == "<5", " ", median)) %>% 
#   mutate(records = ifelse(events == "<5", " ", records)) %>% 
#   mutate(n.max = ifelse(events == "<5", " ", n.max)) %>% 
#   mutate(n.start = ifelse(events == "<5", " ", n.start)) %>% 
#   mutate(rmean = ifelse(events == "<5", " ", rmean)) %>% 
#   mutate(`se(rmean)` = ifelse(events == "<5", " ", `se(rmean)`)) %>% 
#   mutate(`0.95LCL` = ifelse(events == "<5", " ", `0.95LCL`)) %>% 
#   mutate(`0.95UCL` = ifelse(events == "<5", " ",`0.95UCL`)) 
# 
# # put reason for obscuring i.e. median not achieved
# 
# survival_median_table <-  
#   survival_median_table %>% 
#   mutate(median = ifelse(is.na(`0.95UCL`) == TRUE, "Not achieved", median)) %>% 
#   mutate(`0.95LCL` = ifelse(is.na(`0.95LCL`) == TRUE, "Not calculated", `0.95LCL`)) %>% 
#   mutate(`0.95UCL` = ifelse(is.na(`0.95UCL`) == TRUE, "Not calculated",`0.95UCL`)) %>% 
#   mutate(`0.95LCL` = ifelse(median == "Not achieved", "Not calculated",`0.95LCL`)) %>% 
#   mutate(`0.95LCL` = ifelse(events == "<5", "Result obscured",`0.95LCL`))%>% 
#   mutate(`0.95UCL` = ifelse(events == "<5", "Result obscured",`0.95UCL`)) %>% 
#   mutate(median = ifelse(events == "<5", "Result obscured",median))%>% 
#   mutate(rmean = ifelse(events == "<5", "Result obscured", rmean)) %>% 
#   mutate(`se(rmean)` = ifelse(events == "<5", "Result obscured", `se(rmean)`)) 

# whole pop
survival_estimates_whole <- survival_estimates %>% 
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2022" ) %>%
  droplevels() 

survival_risk_table_whole <- survival_risk_table %>%
  rename(CalendarYearGp = CalenderYearGp)

survival_median_table_whole <- survival_median_table %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2022" ) %>%
  droplevels()

#calendar cy
survival_estimates_cy <- survival_estimates %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2022") %>% 
  droplevels()



