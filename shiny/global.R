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
library(PatientProfiles)
library(dsr)

# 
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/dsr/dsr_0.2.2.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# install.packages("devtools")
# library(devtools)
# packageurl <- 'http://cran.r-project.org/src/contrib/Archive/frailtypack/frailtypack_3.0.1.tar.gz'
# install.packages(packageurl, repos=NULL, type="source")

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
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

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


#merge incidence results together
# incidence estimates not standardized
incidence_estimates_files<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files<-results[!(stringr::str_detect(results, "incidence_attrition"))]
incidence_estimates_files<-incidence_estimates_files[(stringr::str_detect(incidence_estimates_files, "incidence_"))]
incidence_estimates_files <- incidence_estimates_files[1]
incidence_estimates <- list()
for(i in seq_along(incidence_estimates_files)){
  incidence_estimates[[i]]<-readr::read_csv(incidence_estimates_files[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates <- dplyr::bind_rows(incidence_estimates)
incidence_estimates <- prepare_output(incidence_estimates)


# pull out female breast and male prostate
incidence_estimates_breast <- incidence_estimates %>% 
  filter(outcome_cohort_name == "Breast" & denominator_sex == "Female")  %>% 
  filter(denominator_age_group == "All") 

incidence_estimates_prostate <- incidence_estimates %>% 
  filter(outcome_cohort_name == "Prostate") %>% 
  filter(denominator_age_group == "All") 

incidence_estimates <- incidence_estimates %>% 
  filter(outcome_cohort_name != "Breast") %>% 
  filter(outcome_cohort_name != "Prostate") %>% 
  filter(denominator_age_group == "All") %>% 
  filter(denominator_sex == "Both") 

incidence_estimates <- bind_rows(incidence_estimates ,
                                 incidence_estimates_breast, 
                                 incidence_estimates_prostate) %>% 
  rename(Database = cdm_name, Cancer = outcome_cohort_name)

# incidence for age standization
incidence_estimates_files_std<-results[stringr::str_detect(results, ".csv")]
incidence_estimates_files_std<-results[!(stringr::str_detect(results, "incidence_attrition"))]
incidence_estimates_files_std<-incidence_estimates_files_std[(stringr::str_detect(incidence_estimates_files_std, "incidence_"))]
incidence_estimates_files_std <- incidence_estimates_files_std[2]
incidence_estimates_std <- list()
for(i in seq_along(incidence_estimates_files_std)){
  incidence_estimates_std[[i]]<-readr::read_csv(incidence_estimates_files_std[[i]], 
                                            show_col_types = FALSE)  
}
incidence_estimates_std <- dplyr::bind_rows(incidence_estimates_std)
incidence_estimates_std <- prepare_output(incidence_estimates_std)


# pull out female breast and male prostate
incidence_estimates_breast_std <- incidence_estimates_std %>% 
  filter(outcome_cohort_name == "Breast" & denominator_sex == "Female")  %>% 
  filter(denominator_age_group != "All") 

incidence_estimates_prostate_std <- incidence_estimates_std %>% 
  filter(outcome_cohort_name == "Prostate") %>% 
  filter(denominator_age_group != "All") 

incidence_estimates_std <- incidence_estimates_std %>% 
  filter(outcome_cohort_name != "Breast") %>% 
  filter(outcome_cohort_name != "Prostate") %>% 
  filter(denominator_age_group != "All") %>% 
  filter(denominator_sex == "Both") 

incidence_estimates_std <- bind_rows(incidence_estimates_std ,
                                 incidence_estimates_breast_std, 
                                 incidence_estimates_prostate_std) %>% 
  rename(Database = cdm_name, Cancer = outcome_cohort_name) %>% 
  rename(Agegroup = denominator_age_group) %>% 
  filter(analysis_interval == "quarters")

# read in the european age standard 2013
ESP13 <- readr::read_csv(here("www", "ESP13.csv"), 
                                              show_col_types = FALSE) 

#collapse ESP13 
ESP13_updated <- ESP13 %>% 
  filter(Agegroup != "0-4",
         Agegroup != "5-9",
         Agegroup != "10-14",
         Agegroup != "15-19" ) %>% 
  add_row(Agegroup = "18 to 29", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '20-24'| Agegroup == '25-29']))) %>% 
  add_row(Agegroup = "30 to 39", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '30-34'| Agegroup == '35-39']))) %>% 
  add_row(Agegroup = "40 to 49", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '40-44'| Agegroup == '45-49']))) %>% 
  add_row(Agegroup = "50 to 59", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '50-54'| Agegroup == '55-59']))) %>% 
  add_row(Agegroup = "60 to 69", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '60-64'| Agegroup == '65-69']))) %>% 
  add_row(Agegroup = "70 to 79", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '70-74'| Agegroup == '75-79']))) %>% 
  add_row(Agegroup = "80 to 89", ESP2013 = with(ESP13, sum(ESP2013[Agegroup == '80-84'| Agegroup == '85-89']))) %>% 
  filter(Agegroup == "18 to 29" | Agegroup == "30 to 39"| Agegroup == "40 to 49"| Agegroup == "50 to 59" | Agegroup == "60 to 69" |
           Agegroup == "70 to 79" |
           Agegroup == "80 to 89" |
           Agegroup == "90+" ) %>% 
  mutate(Agegroup = replace(Agegroup, Agegroup == "90+", "90 +")) 

#rename ESP column to pop (needs to be pop otherwise will not work)
ESP13_updated <- ESP13_updated %>% 
  rename(pop = ESP2013)

#create a loop for each cancer (all cancers apart from prostate and breast are for single genders)
agestandardizedinc <- list()

for(i in 1:length(table(incidence_estimates_std$Cancer))){
  
  incidence_estimates_i <- incidence_estimates_std %>%
    filter(Cancer == names(table(incidence_estimates_std$Cancer)[i]))
  
  agestandardizedinc[[i]] <- dsr::dsr(
    data = incidence_estimates_i,  # specify object containing number of deaths per stratum
    event = n_events,       # column containing number of deaths per stratum 
    fu = person_years , # column containing number of population per stratum person years
    subgroup = incidence_start_date,   
    refdata = ESP13_updated, # reference population data frame, with column called pop
    method = "gamma",      # method to calculate 95% CI
    sig = 0.95,            # significance level
    mp = 100000,           # we want rates per 100.000 population
    decimals = 2) 
  
  agestandardizedinc[[i]] <- agestandardizedinc[[i]] %>% 
    mutate(Cancer = names(table(incidence_estimates_std$Cancer)[i])) 
  
  print(paste0("age standardization for ", names(table(incidence_estimates_std$Cancer)[i]), " done"))
  
}

agestandardizedinc_final <- bind_rows(agestandardizedinc) %>% 
  mutate(Database = "CPRD GOLD") %>% 
  rename(Crude_IR = `Crude Rate (per 1e+05)`) %>% 
  rename(Std_IR = `Std Rate (per 1e+05)`) %>% 
  rename(`LCL_Crude` = `95% LCL (Crude)`) %>% 
  rename(`UCL_Crude` = `95% UCL (Crude)`) %>% 
  rename(`LCL_Std` = `95% LCL (Std)`) %>% 
  rename(`UCL_Std` = `95% UCL (Std)`) %>% 
  as_tibble() %>% 
  select(c(Subgroup,
           Std_IR,
           LCL_Std,
           UCL_Std, 
           Cancer, 
           Database 
           ))



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

# pull out female breast and male prostate
incidence_attrition_breast <- incidence_attrition %>% 
  filter(outcome_cohort_name == "Breast" & denominator_sex == "Female")  %>% 
  filter(denominator_age_group == "All") 

incidence_attrition_prostate <- incidence_attrition %>% 
  filter(outcome_cohort_name == "Prostate") %>% 
  filter(denominator_age_group == "All") 

incidence_attrition <- incidence_attrition %>% 
  filter(outcome_cohort_name != "Breast") %>% 
  filter(outcome_cohort_name != "Prostate") %>% 
  filter(denominator_age_group == "All") %>% 
  filter(denominator_sex == "Both") 

incidence_attrition <- bind_rows(incidence_attrition ,
                                 incidence_attrition_breast, 
                                 incidence_attrition_prostate) %>% 
  select(-c(analysis_id,
            outcome_cohort_id,
            analysis_repeated_events,
            analysis_complete_database_intervals,
            denominator_cohort_id,                  
            analysis_outcome_washout,
            analysis_min_cell_count,
            denominator_cohort_name,            
            denominator_days_prior_observation,   
            denominator_start_date,
            denominator_end_date,     
            denominator_target_cohort_definition_id,
            denominator_target_cohort_name     
            )) %>% 
  relocate(outcome_cohort_name)


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

# pull out female breast and male prostate
survival_estimates_breast <- survival_estimates %>% 
  filter(Cancer == "Breast" & Sex == "Female")  %>% 
  filter(Age == "All") 

survival_estimates_prostate <- survival_estimates %>% 
  filter(Cancer == "Prostate") %>% 
  filter(Age == "All") 

survival_estimates <- survival_estimates %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate") %>% 
  filter(Age == "All") %>% 
  filter(Sex == "Both")

survival_estimates <- bind_rows(survival_estimates ,
                                survival_estimates_breast, 
                                survival_estimates_prostate)

# risk tables ----------
# merge the risk table (whole dataset)
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

# pull out female breast and male prostate
survival_risk_table_breast <- survival_risk_table %>% 
  filter(Cancer == "Breast" & Sex == "Female")  %>% 
  filter(Age == "All") 

survival_risk_table_prostate <- survival_risk_table %>% 
  filter(Cancer == "Prostate") %>% 
  filter(Age == "All") 

survival_risk_table <- survival_risk_table %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate") %>% 
  filter(Age == "All") %>% 
  filter(Sex == "Both")

survival_risk_table <- bind_rows(survival_risk_table ,
                                 survival_risk_table_breast, 
                                 survival_risk_table_prostate) %>% 
  select(details,
    "0" ,   "0.5" ,"1" ,  "2" ,   "3"   , "4" , "5" , "6" , "7" , "8"   , "9"   ,"10"   ,"11"     ,       
         "12" , "13"  ,   "14" , "15" , "16"  ,  "17" ,  "18" ,  "19" ,  "20" ,   "21"  , "22" ,     
         Cancer,    Sex,    Age,    Database )     


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

# pull out female breast and male prostate
survival_risk_cy_table_breast <- survival_risk_cy_table %>% 
  filter(Cancer == "Breast" & Sex == "Female")  %>% 
  filter(Age == "All") 

survival_risk_cy_table_prostate <- survival_risk_cy_table %>% 
  filter(Cancer == "Prostate") %>% 
  filter(Age == "All") 

survival_risk_cy_table <- survival_risk_cy_table %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate") %>% 
  filter(Age == "All") %>% 
  filter(Sex == "Both")

survival_risk_cy_table <- bind_rows(survival_risk_cy_table ,
                                    survival_risk_cy_table_breast, 
                                    survival_risk_cy_table_prostate)

# median and survival probabilities ------
# median results whole database
survival_median_files <- results[stringr::str_detect(results, ".csv")]
survival_median_files <- results[stringr::str_detect(results, "median_survival")]
survival_median_files <- survival_median_files[!(stringr::str_detect(survival_median_files, "median_survival_results_cy"))]
survival_median_table <- list()
for(i in seq_along(survival_median_files)){
  survival_median_table[[i]]<-readr::read_csv(survival_median_files[[i]],
                                              show_col_types = FALSE)  
}
survival_median_table <- dplyr::bind_rows(survival_median_table)
survival_median_table <- prepare_output_survival(survival_median_table)

# pull out female breast and male prostate
survival_median_table_breast <- survival_median_table %>% 
  filter(Cancer == "Breast" & Sex == "Female")  %>% 
  filter(Age == "All") 

survival_median_table_prostate <- survival_median_table %>% 
  filter(Cancer == "Prostate") %>% 
  filter(Age == "All") 

survival_median_table <- survival_median_table %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate") %>% 
  filter(Age == "All") %>% 
  filter(Sex == "Both")

survival_median_table <- bind_rows(survival_median_table ,
                                   survival_median_table_breast, 
                                   survival_median_table_prostate) %>% 

rename(
  "0.5-year Survival (95% CI)" = `Survival Rate % (95% CI) year 0.5`,
  "1-year Survival (95% CI)" = `Survival Rate % (95% CI) year 1`,
  "2-year Survival (95% CI)" = `Survival Rate % (95% CI) year 2`,
  "3-year Survival (95% CI)" = `Survival Rate % (95% CI) year 3`,
  "5-year Survival (95% CI)" = `Survival Rate % (95% CI) year 5`,
  "Mean Survival (SE)" = `rmean in years (SE)`,
  "Median Survival (95% CI)" = `Median Survival in Years (95% CI)`
) %>% 
  select(!c(
            "se", 
            "Sex", 
            "rmean",
            "median",
            "CalenderYearGp",
            "Survival Rate % (95% CI) year 1.5",
            "Survival Rate % (95% CI) year 2.5",
            "surv year 0.5" ,
            "surv year 1" ,        
            "surv year 1.5"  ,
            "surv year 2"  , 
            "surv year 2.5"  ,
            "surv year 3"     ,                 
            "surv year 5"    ,
            "Method"   ,
            "Age"))



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

# pull out female breast and male prostate
survival_median_table_cy_breast <- survival_median_table_cy %>% 
  filter(Cancer == "Breast" & Sex == "Female")  %>% 
  filter(Age == "All") 

survival_median_table_cy_prostate <- survival_median_table_cy %>% 
  filter(Cancer == "Prostate") %>% 
  filter(Age == "All") 

survival_median_table_cy <- survival_median_table_cy %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate") %>% 
  filter(Age == "All") %>% 
  filter(Sex == "Both")

survival_median_table_cy <- bind_rows(survival_median_table_cy ,
                                      survival_median_table_cy_breast, 
                                      survival_median_table_cy_prostate) %>% 
  
  rename(
    "0.5-year Survival (95% CI)" = `Survival Rate % (95% CI) year 0.5`,
    "1-year Survival (95% CI)" = `Survival Rate % (95% CI) year 1`,
    "2-year Survival (95% CI)" = `Survival Rate % (95% CI) year 2`,
    "3-year Survival (95% CI)" = `Survival Rate % (95% CI) year 3`,
    "5-year Survival (95% CI)" = `Survival Rate % (95% CI) year 5`,
    "Mean Survival (SE)" = `rmean in years (SE)`,
    "Median Survival (95% CI)" = `Median Survival in Years (95% CI)`
  ) %>% 
  select(!c(
    "se", 
    "Sex", 
    "rmean",
    "median",
    "Survival Rate % (95% CI) year 1.5",
    "Survival Rate % (95% CI) year 2.5",
    "surv year 0.5" ,
    "surv year 1" ,        
    "surv year 1.5"  ,
    "surv year 2"  , 
    "surv year 2.5"  ,
    "surv year 3"     ,                 
    "surv year 5"    ,
    "Method"   ,
    "Age"))


# survival estimates -----
# whole pop
survival_estimates_whole <- survival_estimates %>% 
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2022" ) %>%
  droplevels() 

#calendar cy
survival_estimates_cy <- survival_estimates %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2022") %>% 
  droplevels()

# table one ------
tableone_whole_files <- results[stringr::str_detect(results, ".csv")]
tableone_whole_files <- results[stringr::str_detect(results, "tableone")]
tableone_whole <- list()
for(i in seq_along(tableone_whole_files)){
  tableone_whole[[i]] <- readr::read_csv(tableone_whole_files[[i]],
                                                 show_col_types = FALSE)  
}
patient_characteristics <- dplyr::bind_rows(tableone_whole)

# pull out female breast and male prostate
patient_characteristics_breast <- patient_characteristics %>% 
  filter(
    (group_level == "Breast" & strata_level == "Female") |
      (group_level == "Breast" & strata_level == "Female and 2000 to 2004") |
      (group_level == "Breast" & strata_level == "Female and 2005 to 2009") |
      (group_level == "Breast" & strata_level == "Female and 2010 to 2014") |
      (group_level == "Breast" & strata_level == "Female and 2015 to 2019") |
      (group_level == "Breast" & strata_level == "Female and 2020 to 2022")
  ) %>% 
  mutate(strata_name = ifelse(strata_name == "sex", "Overall", strata_name)) %>% 
  mutate(strata_level = ifelse(strata_name == "Overall", "Overall", strata_level))

patient_characteristics_prostate <- patient_characteristics %>% 
  filter((group_level == "Prostate" & strata_level == "Male") |
           (group_level == "Prostate" & strata_level == "Male and 2000 to 2004") |
           (group_level == "Prostate" & strata_level == "Male and 2005 to 2009") |
           (group_level == "Prostate" & strata_level == "Male and 2010 to 2014") |
           (group_level == "Prostate" & strata_level == "Male and 2015 to 2019") |
           (group_level == "Prostate" & strata_level == "Male and 2020 to 2022")  ) %>% 
  mutate(strata_name = ifelse(strata_name == "sex", "Overall", strata_name)) %>% 
  mutate(strata_level = ifelse(strata_name == "Overall", "Overall", strata_level))

patient_characteristics <- patient_characteristics %>% 
  filter(group_level != "Breast") %>% 
  filter(group_level != "Prostate") %>% 
  filter(strata_name == "Overall" | strata_name == "calendar_yr_gp" )

patient_characteristics <- bind_rows(patient_characteristics ,
                                     patient_characteristics_breast, 
                                     patient_characteristics_prostate)

# cdm snapshot ------
snapshot_files <- results[stringr::str_detect(results, ".csv")]
snapshot_files <- results[stringr::str_detect(results, "snapshot")]
snapshotcdm <- list()
for(i in seq_along(snapshot_files)){
  snapshotcdm[[i]] <- readr::read_csv(snapshot_files[[i]],
                                      show_col_types = FALSE)
}
snapshotcdm <- bind_rows(snapshotcdm) %>% 
  select("cdm_name", "person_count", "observation_period_count" ,
         "vocabulary_version") %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  dplyr::mutate(cdm_name = replace(cdm_name, cdm_name == "CPRD_GOLD", "CPRD GOLD")) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version") 
