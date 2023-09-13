library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)
library(ggplot2)
library(scales)
library(ggh4x)
library(readr)
library(rio)
library(tidyverse)

datapath <- "C:/Users/dnewby/Documents/GitHub/EHDENCancerIncidencePrevalence/5_tables/data"

#printing numbers with 3 decimal place and commas
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}

nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}

nice.num1<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}


# for survival probabilites
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
                                   levels = c("2000 to 2019",
                                              "2000 to 2021",
                                              "2000 to 2004", 
                                              "2005 to 2009", 
                                              "2010 to 2014",
                                              "2015 to 2019",
                                              "2020 to 2021")))
  
  
  result <- result %>%
    mutate(Database = replace(Database, Database == "CPRDAurum", "CPRD Aurum")) %>%
    mutate(Database = replace(Database, Database == "CPRDGoldUpdate2", "CPRD GOLD")) 
  
  result <- result %>%
    mutate(Gender=replace(Gender, Cancer=="Prostate", "Male"))
  
  return(result)
} 

# Load, prepare, and merge results -----
results <-list.files(datapath, full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = datapath)
}

#grab the results from the folders
results <- list.files(
  path = datapath,
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)


# merge the survival probabilities estimates
survival_probabilites_files<-results[stringr::str_detect(results, ".csv")]
survival_probabilites_files<-results[stringr::str_detect(results, "one_five_ten_survival_rates")]


survival_probabilites <- list()
for(i in seq_along(survival_probabilites_files)){
  survival_probabilites[[i]]<-readr::read_csv(survival_probabilites_files[[i]],
                                             show_col_types = FALSE)
}

survival_probabilites <- dplyr::bind_rows(survival_probabilites) 
survival_probabilites <- prepare_output_survival(survival_probabilites) %>%
  mutate("Survival Rate % (95% CI)"= ifelse(!is.na(surv),
                                       paste0(paste0(nice.num1(surv*100)), " (",
                                              paste0(nice.num1(lower*100))," - ",
                                              paste0(nice.num1(upper*100)), ")"),
                                       NA
  ))

saveRDS(survival_probabilites,
        paste0(datapath, "/survival_probabilites.rds"))


# table for short term survival for covid paper
# filter to include 1 and 2 year survival

survival_probabilites12 <- survival_probabilites %>% 
  filter(time == 1 | time == 2) %>% 
  select(time, Gender, Cancer, CalenderYearGp, `Survival Rate % (95% CI)`)

# convert from long to wide format based on time column
survival_probabilites_table <- pivot_wider(survival_probabilites12, names_from = "time", values_from = c(`Survival Rate % (95% CI)`)) %>% 
  rename(`% One year survival (95% CI)` = "1", `% Two year survival (95% CI)` = "2" ) 

# extract data for prostate cancer males
survival_probabilites_table_prostate <-
  survival_probabilites_table %>% 
  filter(Cancer == "Prostate")

#extract results for female breast cancer patients
survival_probabilites_table_breast <-
  survival_probabilites_table %>% 
  filter(Cancer == "Breast" & Gender == "Female")

# rwo bind relevant results together
survival_probabilites_final <- 
  survival_probabilites_table %>% 
  filter(Gender == "Both") %>% 
  filter(Cancer != "Breast")

# create final table remove and rename columns
survival_probabilites_final <- rbind(survival_probabilites_final,
                                     survival_probabilites_table_breast,
                                     survival_probabilites_table_prostate ) %>% 
  relocate(`Calendar Year` =CalenderYearGp, .after = last_col()) %>% 
  select(-c(Gender)) %>% 
  arrange(Cancer)

# save as csv
write.csv(survival_probabilites_final ,file = paste0(datapath, "/table2survival_probabilites_covid_paper.csv"))


################# n events, censoring, for cancers

survival_probabilites_events <- survival_probabilites %>% 
  filter(Gender == "Both") %>% 
  filter(Cancer != "Breast")

survival_probabilites_eventsBreast <- survival_probabilites %>% 
  filter(Cancer == "Breast" & Gender == "Female")

survival_probabilites_eventsProstate <- survival_probabilites %>% 
  filter(Cancer == "Prostate")

survival_probabilites_eventsfinal <- rbind(survival_probabilites_events,
                                           survival_probabilites_eventsBreast,
                                           survival_probabilites_eventsProstate ) %>% 
  select(c(time, n.risk, n.event, n.censor, Cancer, CalenderYearGp, `Survival Rate % (95% CI)`  )) %>% 
  arrange(Cancer) %>% 
  rename(`Calendar Year` = CalenderYearGp) %>% 
  relocate(`Survival Rate % (95% CI)`, .after = n.censor) %>% 
  relocate(Cancer) %>% 
  relocate(`Calendar Year`, .after = Cancer)

write.csv(survival_probabilites_eventsfinal ,file = paste0(datapath, "/S6survival_events_covid_paper.csv")) 


####################################################################
# table 1 stratified by calendar year
#to generate a table for each cancer where patients are shown per diagnosis year split (2000-2004, 2005-2009 etc)

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


table1_files<-results[stringr::str_detect(results, ".csv")]
table1_files<-results[stringr::str_detect(results, "Table1CalenderYr")]

table1_results <- list()
for(i in seq_along(table1_files)){
  table1_results[[i]]<-readr::read_csv(table1_files[[i]], 
                                       show_col_types = FALSE)  
}

table1_results <- dplyr::bind_rows(table1_results)

table1_results <- prepare_output_table1(table1_results)

# table1_results <- table1_results %>% 
#   distinct()

table1_results <- table1_results %>% 
  mutate("Variable"= ifelse(!is.na(percent),
                            paste0(n, " (",
                                   paste0(percent, ")")),
                            NA
  )) %>%
  mutate("Variable1"= ifelse(!is.na(mean),
                             paste0(mean, " (SD ",
                                    paste0(standard_deviation, ")")),
                             NA
  )) %>%
  mutate("Variable2"= ifelse(!is.na(median),
                             paste0(median, " (",
                                    paste0(interquartile_range, ")")),
                             NA
  )) %>%
  mutate(Variable = case_when(n == "<5" ~ "<5", TRUE ~ Variable)) %>%
  mutate(Variable = coalesce(Variable, Variable2)) %>%
  mutate(Variable = coalesce(Variable, n))

# remove rows we dont need
table1_results <- table1_results %>% 
  #filter(!grepl("Sex: Female",var)) %>%
  filter(!grepl("Death: Alive",var)) %>%
  filter(!grepl("Prior_history_days_study_start",var))%>%
  filter(!grepl("Prior_history_years_start",var)) %>%
  filter(!grepl("Death: Dead",var)) %>%
  filter(!grepl("time_days",var)) %>%
  filter(!grepl("time_years",var)) %>%
  filter(!grepl("DiabetesMellitus",var)) %>%
  filter(!grepl("Prior_history_years",var)) %>%
  filter(!grepl("CoronaryArteriosclerosis",var)) %>%
  filter(!grepl("CrohnsDisease",var)) %>%
  filter(!grepl("GastroesophagealRefluxDisease",var)) %>%
  filter(!grepl("^HeartDisease",var)) %>%
  filter(!grepl("HepatitisC",var)) %>%
  filter(!grepl("HIV",var)) %>%
  filter(!grepl("HpyloriGIInfection",var)) %>%
  filter(!grepl("LesionLiver",var)) %>%
  filter(!grepl("Obesity",var)) %>%
  filter(!grepl("PeripheralVascularDisease",var)) %>%
  filter(!grepl("Pneumonia",var)) %>%
  filter(!grepl("Psoriasis",var)) %>%
  filter(!grepl("RheumatoidArthritis",var)) %>%
  filter(!grepl("Schizophrenia",var)) %>%
  filter(!grepl("UlcerativeColitis",var)) %>%
  filter(!grepl("UTIDisease",var)) %>%
  filter(!grepl("VisualSystemDisorder",var)) %>%
  filter(!grepl("IncidentBreastCancer",var)) %>%
  filter(!grepl("IncidentColorectalCancer",var)) %>%
  filter(!grepl("IncidentEsophagealCancer",var)) %>%
  filter(!grepl("IncidentHeadNeckCancer",var)) %>%
  filter(!grepl("IncidentLiverCancer",var)) %>%
  filter(!grepl("IncidentLungCancer",var)) %>%
  filter(!grepl("IncidentPancreaticCancer",var)) %>%
  filter(!grepl("IncidentProstateCancer",var)) %>%
  filter(!grepl("IncidentStomachCancer",var)) %>%
  filter(!grepl("alcoholicliverdamage",var)) %>%
  filter(!grepl("alcoholism",var)) %>%
  filter(!grepl("alphaantitrypsindeficiency",var)) %>%
  filter(!grepl("autoimmunehepatitis",var)) %>%
  filter(!grepl("diseaseofliver",var)) %>%
  filter(!grepl("Hemochromatosis",var)) %>%
  filter(!grepl("hepb",var)) %>%
  filter(!grepl("hypercholesterolemia",var)) %>%
  filter(!grepl("nash",var)) %>%
  filter(!grepl("nonalcoholicfattyliver",var)) %>%
  filter(!grepl("obesity_obs_cond",var)) %>% 
  filter(!grepl("alcohol",var)) %>% 
  filter(!grepl("nafld",var)) %>% 
  select(c(var, Variable, Cancer, Calendar_year ))


# make into wider format for results
table1_results <- table1_results %>% 
  tidyr::pivot_wider(names_from = Calendar_year,
                     values_from = Variable, 
                     values_fill = NA
  )


write.csv(table1_results ,file = paste0(datapath, "/S3patientcharacteristics_cy_covid_paper.csv")) 

# make the table one for main paper
table1_files1<-results[stringr::str_detect(results, ".csv")]
table1_files1<-results[stringr::str_detect(results, "Table1")]

table1_files1  <- table1_files1[1]

table1_results1 <- list()

for(i in seq_along(table1_files1)){
  table1_results1[[i]]<-readr::read_csv(table1_files1[[i]], 
                                       show_col_types = FALSE) 
}



table1_results1 <- dplyr::bind_rows(table1_results1)

table1_results1 <- prepare_output_table1(table1_results1)

# table1_results <- table1_results %>% 
#   distinct()

table1_results1 <- table1_results1 %>% 
  mutate("Variable"= ifelse(!is.na(percent),
                            paste0(n, " (",
                                   paste0(percent, ")")),
                            NA
  )) %>%
  mutate("Variable1"= ifelse(!is.na(mean),
                             paste0(mean, " (SD ",
                                    paste0(standard_deviation, ")")),
                             NA
  )) %>%
  mutate("Variable2"= ifelse(!is.na(median),
                             paste0(median, " (",
                                    paste0(interquartile_range, ")")),
                             NA
  )) %>%
  mutate(Variable = case_when(n == "<5" ~ "<5", TRUE ~ Variable)) %>%
  mutate(Variable = coalesce(Variable, Variable2)) %>%
  mutate(Variable = coalesce(Variable, n))

# remove rows we dont need
table1_results1 <- table1_results1 %>% 
  #filter(!grepl("Sex: Female",var)) %>%
  filter(!grepl("Death: Alive",var)) %>%
  filter(!grepl("Prior_history_days_study_start",var))%>%
  filter(!grepl("Prior_history_years_start",var)) %>%
  filter(!grepl("Death: Dead",var)) %>%
  filter(!grepl("time_days",var)) %>%
  filter(!grepl("time_years",var)) %>%
  filter(!grepl("DiabetesMellitus",var)) %>%
  filter(!grepl("Prior_history_years",var)) %>%
  filter(!grepl("CoronaryArteriosclerosis",var)) %>%
  filter(!grepl("CrohnsDisease",var)) %>%
  filter(!grepl("GastroesophagealRefluxDisease",var)) %>%
  filter(!grepl("^HeartDisease",var)) %>%
  filter(!grepl("HepatitisC",var)) %>%
  filter(!grepl("HIV",var)) %>%
  filter(!grepl("HpyloriGIInfection",var)) %>%
  filter(!grepl("LesionLiver",var)) %>%
  filter(!grepl("Obesity",var)) %>%
  filter(!grepl("PeripheralVascularDisease",var)) %>%
  filter(!grepl("Pneumonia",var)) %>%
  filter(!grepl("Psoriasis",var)) %>%
  filter(!grepl("RheumatoidArthritis",var)) %>%
  filter(!grepl("Schizophrenia",var)) %>%
  filter(!grepl("UlcerativeColitis",var)) %>%
  filter(!grepl("UTIDisease",var)) %>%
  filter(!grepl("VisualSystemDisorder",var)) %>%
  filter(!grepl("IncidentBreastCancer",var)) %>%
  filter(!grepl("IncidentColorectalCancer",var)) %>%
  filter(!grepl("IncidentEsophagealCancer",var)) %>%
  filter(!grepl("IncidentHeadNeckCancer",var)) %>%
  filter(!grepl("IncidentLiverCancer",var)) %>%
  filter(!grepl("IncidentLungCancer",var)) %>%
  filter(!grepl("IncidentPancreaticCancer",var)) %>%
  filter(!grepl("IncidentProstateCancer",var)) %>%
  filter(!grepl("IncidentStomachCancer",var)) %>%
  filter(!grepl("alcoholicliverdamage",var)) %>%
  filter(!grepl("alcoholism",var)) %>%
  filter(!grepl("alphaantitrypsindeficiency",var)) %>%
  filter(!grepl("autoimmunehepatitis",var)) %>%
  filter(!grepl("diseaseofliver",var)) %>%
  filter(!grepl("Hemochromatosis",var)) %>%
  filter(!grepl("hepb",var)) %>%
  filter(!grepl("hypercholesterolemia",var)) %>%
  filter(!grepl("nash",var)) %>%
  filter(!grepl("nonalcoholicfattyliver",var)) %>%
  filter(!grepl("obesity_obs_cond",var)) %>% 
  filter(!grepl("alcohol",var)) %>% 
  filter(!grepl("nafld",var)) %>% 
  select(c(var, Variable, Cancer)) %>% 
  mutate(Gender = "Both") %>% 
  mutate(across(everything(), as.character))


# read in males
table1_files2<-results[stringr::str_detect(results, ".csv")]
table1_files2<-results[stringr::str_detect(results, "Table1")]

table1_files2  <- table1_files2[3]

table1_results2 <- list()

for(i in seq_along(table1_files2)){
  table1_results2[[i]]<-readr::read_csv(table1_files2[[i]], 
                                        show_col_types = FALSE) 
}



table1_results2 <- dplyr::bind_rows(table1_results2)

table1_results2 <- prepare_output_table1(table1_results2)

table1_results2 <- table1_results2 %>% 
  mutate("Variable"= ifelse(!is.na(percent),
                            paste0(n, " (",
                                   paste0(percent, ")")),
                            NA
  )) %>%
  mutate("Variable1"= ifelse(!is.na(mean),
                             paste0(mean, " (SD ",
                                    paste0(standard_deviation, ")")),
                             NA
  )) %>%
  mutate("Variable2"= ifelse(!is.na(median),
                             paste0(median, " (",
                                    paste0(interquartile_range, ")")),
                             NA
  )) %>%
  mutate(Variable = case_when(n == "<5" ~ "<5", TRUE ~ Variable)) %>%
  mutate(Variable = coalesce(Variable, Variable2)) %>%
  mutate(Variable = coalesce(Variable, n))

# remove rows we dont need
table1_results2 <- table1_results2 %>% 
 # filter(!grepl("Sex: Female",var)) %>%
  filter(!grepl("Death: Alive",var)) %>%
  filter(!grepl("Prior_history_days_study_start",var))%>%
  filter(!grepl("Prior_history_years_start",var)) %>%
  filter(!grepl("Death: Dead",var)) %>%
  filter(!grepl("time_days",var)) %>%
  filter(!grepl("time_years",var)) %>%
  filter(!grepl("DiabetesMellitus",var)) %>%
  filter(!grepl("Prior_history_years",var)) %>%
  filter(!grepl("CoronaryArteriosclerosis",var)) %>%
  filter(!grepl("CrohnsDisease",var)) %>%
  filter(!grepl("GastroesophagealRefluxDisease",var)) %>%
  filter(!grepl("^HeartDisease",var)) %>%
  filter(!grepl("HepatitisC",var)) %>%
  filter(!grepl("HIV",var)) %>%
  filter(!grepl("HpyloriGIInfection",var)) %>%
  filter(!grepl("LesionLiver",var)) %>%
  filter(!grepl("Obesity",var)) %>%
  filter(!grepl("PeripheralVascularDisease",var)) %>%
  filter(!grepl("Pneumonia",var)) %>%
  filter(!grepl("Psoriasis",var)) %>%
  filter(!grepl("RheumatoidArthritis",var)) %>%
  filter(!grepl("Schizophrenia",var)) %>%
  filter(!grepl("UlcerativeColitis",var)) %>%
  filter(!grepl("UTIDisease",var)) %>%
  filter(!grepl("VisualSystemDisorder",var)) %>%
  filter(!grepl("IncidentBreastCancer",var)) %>%
  filter(!grepl("IncidentColorectalCancer",var)) %>%
  filter(!grepl("IncidentEsophagealCancer",var)) %>%
  filter(!grepl("IncidentHeadNeckCancer",var)) %>%
  filter(!grepl("IncidentLiverCancer",var)) %>%
  filter(!grepl("IncidentLungCancer",var)) %>%
  filter(!grepl("IncidentPancreaticCancer",var)) %>%
  filter(!grepl("IncidentProstateCancer",var)) %>%
  filter(!grepl("IncidentStomachCancer",var)) %>%
  filter(!grepl("alcoholicliverdamage",var)) %>%
  filter(!grepl("alcoholism",var)) %>%
  filter(!grepl("alphaantitrypsindeficiency",var)) %>%
  filter(!grepl("autoimmunehepatitis",var)) %>%
  filter(!grepl("diseaseofliver",var)) %>%
  filter(!grepl("Hemochromatosis",var)) %>%
  filter(!grepl("hepb",var)) %>%
  filter(!grepl("hypercholesterolemia",var)) %>%
  filter(!grepl("nash",var)) %>%
  filter(!grepl("nonalcoholicfattyliver",var)) %>%
  filter(!grepl("obesity_obs_cond",var)) %>% 
  filter(!grepl("alcohol",var)) %>% 
  filter(!grepl("nafld",var)) %>% 
  select(c(var, Variable, Cancer, Gender)) 



# read in females
table1_files3<-results[stringr::str_detect(results, ".csv")]
table1_files3<-results[stringr::str_detect(results, "Table1")]

table1_files3  <- table1_files3[2]

table1_results3 <- list()

for(i in seq_along(table1_files3)){
  table1_results3[[i]]<-readr::read_csv(table1_files3[[i]], 
                                        show_col_types = FALSE) 
}



table1_results3 <- dplyr::bind_rows(table1_results3)

table1_results3 <- prepare_output_table1(table1_results3)

table1_results3 <- table1_results3 %>% 
  mutate("Variable"= ifelse(!is.na(percent),
                            paste0(n, " (",
                                   paste0(percent, ")")),
                            NA
  )) %>%
  mutate("Variable1"= ifelse(!is.na(mean),
                             paste0(mean, " (SD ",
                                    paste0(standard_deviation, ")")),
                             NA
  )) %>%
  mutate("Variable2"= ifelse(!is.na(median),
                             paste0(median, " (",
                                    paste0(interquartile_range, ")")),
                             NA
  )) %>%
  mutate(Variable = case_when(n == "<5" ~ "<5", TRUE ~ Variable)) %>%
  mutate(Variable = coalesce(Variable, Variable2)) %>%
  mutate(Variable = coalesce(Variable, n))

# remove rows we dont need
table1_results3 <- table1_results3 %>% 
  # filter(!grepl("Sex: Female",var)) %>%
  filter(!grepl("Death: Alive",var)) %>%
  filter(!grepl("Prior_history_days_study_start",var))%>%
  filter(!grepl("Prior_history_years_start",var)) %>%
  filter(!grepl("Death: Dead",var)) %>%
  filter(!grepl("time_days",var)) %>%
  filter(!grepl("time_years",var)) %>%
  filter(!grepl("DiabetesMellitus",var)) %>%
  filter(!grepl("Prior_history_years",var)) %>%
  filter(!grepl("CoronaryArteriosclerosis",var)) %>%
  filter(!grepl("CrohnsDisease",var)) %>%
  filter(!grepl("GastroesophagealRefluxDisease",var)) %>%
  filter(!grepl("^HeartDisease",var)) %>%
  filter(!grepl("HepatitisC",var)) %>%
  filter(!grepl("HIV",var)) %>%
  filter(!grepl("HpyloriGIInfection",var)) %>%
  filter(!grepl("LesionLiver",var)) %>%
  filter(!grepl("Obesity",var)) %>%
  filter(!grepl("PeripheralVascularDisease",var)) %>%
  filter(!grepl("Pneumonia",var)) %>%
  filter(!grepl("Psoriasis",var)) %>%
  filter(!grepl("RheumatoidArthritis",var)) %>%
  filter(!grepl("Schizophrenia",var)) %>%
  filter(!grepl("UlcerativeColitis",var)) %>%
  filter(!grepl("UTIDisease",var)) %>%
  filter(!grepl("VisualSystemDisorder",var)) %>%
  filter(!grepl("IncidentBreastCancer",var)) %>%
  filter(!grepl("IncidentColorectalCancer",var)) %>%
  filter(!grepl("IncidentEsophagealCancer",var)) %>%
  filter(!grepl("IncidentHeadNeckCancer",var)) %>%
  filter(!grepl("IncidentLiverCancer",var)) %>%
  filter(!grepl("IncidentLungCancer",var)) %>%
  filter(!grepl("IncidentPancreaticCancer",var)) %>%
  filter(!grepl("IncidentProstateCancer",var)) %>%
  filter(!grepl("IncidentStomachCancer",var)) %>%
  filter(!grepl("alcoholicliverdamage",var)) %>%
  filter(!grepl("alcoholism",var)) %>%
  filter(!grepl("alphaantitrypsindeficiency",var)) %>%
  filter(!grepl("autoimmunehepatitis",var)) %>%
  filter(!grepl("diseaseofliver",var)) %>%
  filter(!grepl("Hemochromatosis",var)) %>%
  filter(!grepl("hepb",var)) %>%
  filter(!grepl("hypercholesterolemia",var)) %>%
  filter(!grepl("nash",var)) %>%
  filter(!grepl("nonalcoholicfattyliver",var)) %>%
  filter(!grepl("obesity_obs_cond",var)) %>% 
  filter(!grepl("alcohol",var)) %>% 
  filter(!grepl("nafld",var)) %>% 
  select(c(var, Variable, Cancer, Gender)) 




# get the results together

#breast cancer females
table1bc_f <- table1_results3 %>% 
  filter(Cancer == "Breast")

#breast cancer from
table1all <- table1_results1 %>% 
  filter(Cancer != "Breast")

#merge back together
finaltable1 <- rbind(table1bc_f, table1all ) %>% 
  select(!(Gender))


# make into wider format for results
finaltable1wide <- finaltable1 %>% 
  tidyr::pivot_wider(names_from = Cancer,
                     values_from = Variable, 
                     values_fill = NA
  )


write.csv(finaltable1wide ,file = paste0(datapath, "/Table1PatientCharacteristics.csv")) 
