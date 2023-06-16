library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)
library(ggplot2)
library(scales)
library(ggh4x)

pathResults <- "C:/Users/dnewby/Desktop/Results"
datapath <- "C:/Users/dnewby/Documents/GitHub/EHDENCancerIncidencePrevalence/CancerIncidencePrevalanceShiny/shiny/data"


# FUNCTIONS ----
# printing numbers with 3 decimal place and commas 
# nice.num3<-function(x) {
#   trimws(format(round(x,3),
#                 big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# 
# #preparing the output and renaming numbers for incidence and prevalence
# prepare_output<-function(result){
#   result <- result %>%
#     
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerHypopharynx", "Hypopharynx")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerLarynx", "Larynx")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerNasalCavitySinus", "Nasal Cavity & Sinus")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerNasopharynx", "Nasopharynx")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOralCavityPrevalent", "Oral Cavity")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOropharynx", "Oropharynx")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerSalivaryGland", "Salivary Gland")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerTonguePrevalent", "Tongue")) %>% 
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerOralCavityIncidence", "Oral Cavity")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "HeadNeckSubtypeCancerTongueIncidence", "Tongue")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentProstateCancer", "Prostate")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentLungCancer", "Lung")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentBreastCancer", "Breast")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentColorectalCancer", "Colorectal")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentHeadNeckCancer", "Head & Neck")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentLiverCancer", "Liver")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentPancreaticCancer", "Pancreas")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentStomachCancer", "Stomach")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "IncidentEsophagealCancer", "Esophagus")) %>%    
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentProstateCancer", "Prostate")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLungCancer", "Lung")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentBreastCancer", "Breast")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentColorectalCancer", "Colorectal")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentHeadNeckCancer", "Head & Neck")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentLiverCancer", "Liver")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentPancreaticCancer", "Pancreas")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentStomachCancer", "Stomach")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "PrevalentEsophagealCancer", "Esophagus")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head & Neck")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
#     mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "ProstateCancerMaleOnly", "Prostate"))
#   
#   
#   result<- result %>% 
#     mutate(denominator_age_group= stringr::str_replace(denominator_age_group, ";", " to ")) %>% 
#     mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "18 to 150", "All")) %>% 
#     mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "90 to 150", "90 +")) %>% 
#     mutate(denominator_age_group = factor(denominator_age_group,
#                                           levels = c("All",
#                                                      "18 to 29", "30 to 39", "40 to 49",
#                                                      "50 to 59", "60 to 69", "70 to 79",
#                                                      "80 to 89", "90 +" )))
#   
#   result <- result %>%
#     mutate(database_name = replace(database_name, database_name == "CPRDAurum", "CPRD Aurum")) %>%
#     mutate(database_name = replace(database_name, database_name == "CPRDGold", "CPRD Gold")) 
#   
#   #filter out the results for both genders for prostate cancer (as cohort only in male)
#   result <- result %>%
#     filter(!(outcome_cohort_name == "Prostate" & denominator_sex == "Both")) %>%
#     filter(!(outcome_cohort_name == "Prostate" & denominator_sex == "Female")) 
#   
#   return(result)
# }
# 
# #preparation the output and renaming numbers for survival
# prepare_output_survival <- function(result){
#   result <- result %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerHypopharynx", "Hypopharynx")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerLarynx", "Larynx")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerNasalCavitySinus", "Nasal Cavity & Sinus")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerNasopharynx", "Nasopharynx")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOralCavityPrevalent", "Oral Cavity")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOropharynx", "Oropharynx")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerSalivaryGland", "Salivary Gland")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerTonguePrevalent", "Tongue")) %>% 
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerOralCavityIncidence", "Oral Cavity")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "HeadNeckSubtypeCancerTongueIncidence", "Tongue")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentProstateCancer", "Prostate")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentLungCancer", "Lung")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentBreastCancer", "Breast")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentColorectalCancer", "Colorectal")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentHeadNeckCancer", "Head & Neck")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentLiverCancer", "Liver")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentPancreaticCancer", "Pancreas")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentStomachCancer", "Stomach")) %>%
#     mutate(Cancer = replace(Cancer, Cancer == "IncidentEsophagealCancer", "Esophagus")) %>%    
#     mutate(Cancer = replace(Cancer, Cancer == "PrevalentProstateCancer", "Prostate")) 
# 
#   
#   result<- result %>% 
#     mutate(Age= stringr::str_replace(Age, "-", " to ")) %>% 
#     mutate(CalendarYearGp= stringr::str_replace(CalendarYearGp, "-", " to ")) %>% 
#     mutate(Age = replace(Age, Age == ">=90", "90 +")) %>% 
#     mutate(Age = factor(Age,
#                                           levels = c("All",
#                                                      "18 to 29", "30 to 39", "40 to 49",
#                                                      "50 to 59", "60 to 69", "70 to 79",
#                                                      "80 to 89", "90 +" ))) %>%
#   mutate(CalendarYearGp = factor(CalendarYearGp,
#                       levels = c("2000 to 2019",
#                                  "2000 to 2004", 
#                                  "2005 to 2009", 
#                                  "2010 to 2014",
#                                  "2015 to 2019")))
#   
#   result <- result %>%
#     mutate(Database = replace(Database, Database == "CPRDAurum", "CPRD Aurum")) %>%
#     mutate(Database = replace(Database, Database == "CPRDGold", "CPRD Gold")) 
#   
#   result <- result %>%
#     mutate(Gender=replace(Gender, Cancer=="Prostate", "Male"))
#   
#   return(result)
# } 


#INCIDENCE
#incidence figure1 whole population stratified by database UPDATED
incidenceFigure1 <- function(incidenceData) {
  
incidenceFigureData <- incidenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group == "All") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                    ymax = incidence_100000_pys_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 3.5) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1))
  
  return(incidenceFigureData)
  
}

#incidence figure2 gender stratification and stratified by database UPDATED
incidenceFigure2 <- function(incidenceData) {
  
 incidenceFigureData <- incidenceData %>%
    filter(denominator_sex != "Both",
           denominator_age_group == "All") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                    ymax = incidence_100000_pys_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name ),size = 2.5) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
    facet_grid(cols = vars(denominator_sex)) 
  
  return(incidenceFigureData)
  
}

#incidence figure3 age stratification and stratified by database 
incidenceFigure3 <- function(incidenceData) {
  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group != "All",
           denominator_sex == "Both") %>% 
    
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = denominator_age_group,
               col = denominator_age_group )) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey60")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey60")) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Age group") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
  facet_grid(cols = vars(database_name)) 
  
  
  return(incidenceFigureData)
}

#incidence figure3a age stratification and stratified by database (AGE ON EACH FACET) UPDATED
incidenceFigure3a <- function(incidenceData) {
  
  incidenceFigureData <- incidenceData %>%
      filter(denominator_sex == "Both",
             denominator_age_group != "All") %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 group = database_name)) +
      geom_line(color = "black", size = 0.25) +
      scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
      scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                      ymax = incidence_100000_pys_95CI_upper, 
                      fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
    scale_shape_manual(values = c(24,21)) +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
            strip.background = element_rect(color = "black", size = 0.6) ,
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent")) +
      labs(x = "Calendar year",
           y = "Incidence rate per 100000 person-years",
           col = "Database name",
           shape = "Database name",
           fill = "Database name") +
      scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                   expand = c(0.06,1)) +
      facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  
  return(incidenceFigureData)
}

#incidence figure3b age stratification and stratified by database (AGE ON EACH FACET) for 1 gender i.e. prostate UPDATED
incidenceFigure3b <- function(incidenceData) {
  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group != "All") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                    ymax = incidence_100000_pys_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  
  return(incidenceFigureData)
}

#incidence figure4 whole population and gender stratification by database updated
incidenceFigure4 <- function(incidenceData) {
  
  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group == "All") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = database_name )) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                    ymax = incidence_100000_pys_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 2.5) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          #panel.spacing.x = unit(0.1,"line"),
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
    facet_grid(cols = vars(denominator_sex)) 
  
  return(incidenceFigureData)
  
}

#incidence figure 5 age * sex stratification for database (n = 2) updated
incidenceFigure5 <- function(incidenceData) {

  
  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group != "All") %>%
    filter(denominator_sex != "Both") %>%
    filter(analysis_interval != "overall") %>% 
    unite(Database_Sex, 
          "database_name", "denominator_sex",
          sep=": ", remove = FALSE) %>% 
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = Database_Sex)) +
    geom_line(color = "black", size = 0.2) +
    scale_colour_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) + #blue, #red #blue, #red 
    scale_fill_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower,
                    ymax = incidence_100000_pys_95CI_upper,
                    fill = Database_Sex), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = Database_Sex, fill = Database_Sex),size = 1.5) +
    scale_shape_manual(values = c(24,22,21,25)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database & Sex",
         shape = "Database & Sex" ,
         fill = "Database & Sex" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  return(incidenceFigureData)
}

# PREVALENCE
#prevalence figure1 whole population stratified by database updated
prevalenceFigure1 <- function(prevalenceData) {
  
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group == "All") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                    ymax = prevalence_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 3.5) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name",
         fill = "Database name" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1))
  
  return(prevalenceFigureData)
  
}

#prevalence figure2 gender stratification and stratified by database updated
prevalenceFigure2 <- function(prevalenceData) {
  
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_sex != "Both",
           denominator_age_group == "All") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = database_name )) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower,
                    ymax = prevalence_95CI_upper,
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 2.5) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name" ,
         fill = "Database name" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
    facet_grid(cols = vars(denominator_sex)) 
  
  return(prevalenceFigureData)
}

#prevalence figure3 age stratification and stratified by database 
prevalenceFigure3 <- function(prevalenceData) {
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group != "All",
           denominator_sex == "Both") %>% 
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = denominator_age_group,
               col = denominator_age_group )) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey60")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey60")) +
    # geom_ribbon(aes(ymin = prevalence_95CI_lower,
    #                 ymax = prevalence_95CI_upper,
    #                 fill = denominator_age_group), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name),size = 2.5) +
    scale_shape_manual(values = c(17,19,15)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          panel.background = element_blank() ,
          strip.background = element_rect(color = "black", size = 0.6) ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Age group") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
  facet_grid(cols = vars(database_name)) 
  
  
  return(prevalenceFigureData)
}

#prevalence figure3 age stratification and stratified by database (AGE AS FACETS) updated
prevalenceFigure3a <- function(prevalenceData) {

  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group != "All") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                    ymax = prevalence_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name ),size = 1.5) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  
  return(prevalenceFigureData)
}

#prevalence figure3 age stratification and stratified by database (AGE AS FACETS)  for 1 gender i.e. prostate
prevalenceFigure3b <- function(prevalenceData) {
  
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group != "All") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                    ymax = prevalence_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  
  return(prevalenceFigureData)
}

#prevalence figure4 whole population and gender stratification by database updated
prevalenceFigure4 <- function(prevalenceData) {
  
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group == "All") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower,
                    ymax = prevalence_95CI_upper,
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 2.5) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name",
         fill = "Database name" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
                 expand = c(0.06,1)) +
    facet_grid(cols = vars(denominator_sex)) 
  
  return(prevalenceFigureData)
}

#prevalence figure 5 age * sex stratification for database (n = 2) updated
prevalenceFigure5 <- function(prevalenceData) {
  
  
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group != "All") %>%
    filter(denominator_sex != "Both") %>%
    filter(analysis_interval != "overall") %>% 
    unite(Database_Sex, 
          "database_name", "denominator_sex",
          sep=": ", remove = FALSE) %>% 
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = Database_Sex)) +
    geom_line(color = "black", size = 0.2) +
    scale_colour_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) + #blue, #red #blue, #red 
    scale_fill_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower,
                    ymax = prevalence_95CI_upper,
                    fill = Database_Sex), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = Database_Sex, fill = Database_Sex),size = 1.5) +
    scale_shape_manual(values = c(24,22,21,25)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database & Sex",
         shape = "Database & Sex" ,
         fill = "Database & Sex" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)
  
  return(prevalenceFigureData)
}

# SURVIVAL
#survival figure1 whole population and stratified by database
survivalFigure1 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2))
  
  return(survivalFigureData)
  
}

#survival figure3 age stratification and stratified by database (AGE AS FACETS) for both genders
survivalFigure3a <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age != "All") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    filter(Gender == "Both") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2)) +
    facet_wrap(~ Age, ncol = 2, scales = "free_x") +
    coord_cartesian(xlim = c(0, 20))
  
  
  return(survivalFigureData)
}

#survival figure3 age stratification and stratified by database (AGE AS FACETS) for 1 gender
survivalFigure3b <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age != "All") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2)) +
    facet_wrap(~ Age, ncol = 2, scales = "free_x") +
    coord_cartesian(xlim = c(0, 20))
  
  
  return(survivalFigureData)
}

#survival figure4 whole population and gender stratification by database
survivalFigure4 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Age == "All") %>%
    filter(CalendarYearGp == "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = Database,
               col = Database )) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = Database),size = 0.5) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Database name",
         linetype = "Database name") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 20, 2)) +
    facet_grid(cols = vars(Gender)) 
  
  return(survivalFigureData)
  
}

# survival figure 5 whole population and gender strat with calendar year strat (puts gender as column and database as rows)
survivalFigure5 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "solid")) +
     labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    ggh4x::facet_grid2(cols = vars(Gender), vars(Database), scales="free", independent = "y") 
  
  
  return(survivalFigureData)
  
}

# survival figure 6 whole population and gender strat with calendar year strat BUT puts database as columns and gender as rows
survivalFigure6 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "solid")) +
    labs(x = "Time (Years)",
         y = "Survival Probability",
         col = "Calendar Year Group",
         linetype = "Calendar Year Group") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    ggh4x::facet_grid2(cols = vars(Database) ,vars(Gender), scales="free", independent = "y") 
  
  
  return(survivalFigureData)
  
}


# ANALYSIS ----
# read in files
prevalence_estimates <- readRDS(paste0(datapath ,"/prevalence_estimates.rds"))
prevalence_attrition <- readRDS(paste0(datapath ,"/prevalence_attrition.rds"))
incidence_estimates <- readRDS(paste0(datapath ,"/incidence_estimates.rds"))
incidence_attrition <- readRDS(paste0(datapath ,"/incidence_attrition.rds"))
survival_estimates <- readRDS(paste0(datapath ,"/survival_estimates.rds"))%>% 
  rename(CalendarYearGp = CalenderYearGp )

# CREATING PLOTS ----
#plot per cancer stratified by database for incidence and prevalence WHOLE POPULATION
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {

#incidence rates
incidence_estimates_i <- incidence_estimates %>%
  filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")

if(names(table(incidence_estimates$outcome_cohort_name)[i]) == "Prostate"){
  print("Prostate cancer plot not drawn ")
} else {

plot1 <- incidenceFigure1(incidence_estimates_i)

plotname <- paste0("IncidenceRatesWholePop_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 7, height = 5, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()
}

#prevalence
prevalence_estimates_i <- prevalence_estimates %>%
  filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")

if(names(table(prevalence_estimates$outcome_cohort_name)[i]) == "Prostate"){
  print("Prostate cancer plot not drawn ")
} else {

plot1 <- prevalenceFigure1(prevalence_estimates_i)

plotname <- paste0("PeriodPrevalenceWholePop_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 7, height = 5, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

}


#survival
survival_estimates_i <- survival_estimates %>% filter(Cancer == names(table(survival_estimates$Cancer)[i]) )

if(names(table(survival_estimates$Cancer)[i]) == "Prostate"){
  print("Prostate cancer plot not drawn ")
} else {

plot1 <- survivalFigure1(survival_estimates_i)

plotname <- paste0("KMSurvivalAllStrat_", names(table(survival_estimates$Cancer)[i]),".png")

png(paste0(pathResults ,"/WholePop/", plotname), width = 7, height = 5, units = "in", res = 1200)

print(plot1, newpage = FALSE)
dev.off()

}

}

#plot per cancer stratified by database for incidence and prevalence GENDER STRATIFICATIONS
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  #incidence rates
  incidence_estimates_i <- incidence_estimates %>%
    filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
  
  plot1 <- incidenceFigure2(incidence_estimates_i)
  
  plotname <- paste0("IncidenceGenderStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
  
  png(paste0(pathResults ,"/GenderStrat/", plotname),
      width = 8, height = 5, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()

  #prevalence
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
  
  plot1 <- prevalenceFigure2(prevalence_estimates_i)
  
  plotname <- paste0("PeriodPrevalenceGenderStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
  
  png(paste0(pathResults ,"/GenderStrat/", plotname),
      width = 8, height = 5 , units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()
  
}

#plot per cancer stratified by database for incidence and prevalence AGE STRATIFICATIONS ON ONE PLOT
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  #filter out prostate cancer
  incidence_estimates_i <- incidence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(incidence_estimates$outcome_cohort_name)[i]) == "Prostate" ){
     print("prostate cancer not run incidence") } else {
  
  #incidence rates
  incidence_estimates_i <- incidence_estimates_i %>%
    filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
  
  plot1 <- incidenceFigure3(incidence_estimates_i)
  
  plotname <- paste0("IncidenceAgeStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
  
  png(paste0(pathResults ,"/AgeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()
     }
  
  #prevalence
  
  #filter out male data
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(prevalence_estimates$outcome_cohort_name)[i]) == "Prostate" ){
    print("prostate cancer not run prevalence") } else {
  
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
  
  plot1 <- prevalenceFigure3(prevalence_estimates_i)
  
  plotname <- paste0("PeriodPrevalenceAgeStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
  
  png(paste0(pathResults ,"/AgeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()
  
    }
}

#plot per cancer stratified by database for incidence and prevalence AGE STRATIFICATIONS FACET BY AGE
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  #filter out prostate cancer
  incidence_estimates_i <- incidence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(incidence_estimates$outcome_cohort_name)[i]) == "Prostate" ){
   
    incidence_estimates_i <- incidence_estimates %>%
      filter(outcome_cohort_name == "Prostate" & analysis_interval == "years") 
    
    plot1 <- incidenceFigure3b(incidence_estimates_i)
    
    plotname <- paste0("FIGURE2_IncidenceAgeStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
    
    } else {
      
      #incidence rates
      incidence_estimates_i <- incidence_estimates_i %>%
        filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
      
      plot1 <- incidenceFigure3a(incidence_estimates_i)
      
      plotname <- paste0("FIGURE2_IncidenceAgeStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
      
      png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
      print(plot1, newpage = FALSE)
      dev.off()
    }
  
  #prevalence
  
  #filter out male data
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(prevalence_estimates$outcome_cohort_name)[i]) == "Prostate" ){

    prevalence_estimates_i <- prevalence_estimates %>%
      filter(outcome_cohort_name == "Prostate"  & analysis_interval == "years")
    
    plot1 <- prevalenceFigure3b(prevalence_estimates_i)
    
    plotname <- paste0("FIGURE4_PrevalenceAgeStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()   
    
    
    
    } else {
      
      prevalence_estimates_i <- prevalence_estimates %>%
        filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
      
      plot1 <- prevalenceFigure3a(prevalence_estimates_i)
      
      plotname <- paste0("FIGURE4_PrevalenceAgeStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
      
      png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
      print(plot1, newpage = FALSE)
      dev.off()
      
    }
  
  #survival
  
  #filter out male data
  survival_estimates_i <- survival_estimates %>%
    filter(Cancer != "Prostate" )
  
  if (names(table(survival_estimates$Cancer)[i]) == "Prostate" ){
    
    survival_estimates_i <- survival_estimates %>%
      filter(Cancer == "Prostate")
    
    plot1 <- survivalFigure3b(survival_estimates_i)
    
    plotname <- paste0("FIGUREX_KMAgeStrat_", names(table(survival_estimates$Cancer)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()   
    
    
    
  } else {
    
    survival_estimates_i <- survival_estimates %>%
      filter(Cancer == names(table(survival_estimates$Cancer)[i]))
    
    plot1 <- survivalFigure3a(survival_estimates_i)
    
    plotname <- paste0("FIGUREX_KMAgeStrat_", names(table(survival_estimates$Cancer)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
    
  }
  
  
}

#plot per cancer stratified by database for incidence and prevalence AGE* SEX STRATIFICATIONS FACET BY AGE
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  #filter out prostate cancer
  incidence_estimates_i <- incidence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(incidence_estimates$outcome_cohort_name)[i]) == "Prostate" ){
    
    print("No plot for prostate cancer")

  } else {
    
    #incidence rates
    incidence_estimates_i <- incidence_estimates_i %>%
      filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
    
    plot1 <- incidenceFigure5(incidence_estimates_i)
    
    plotname <- paste0("FIGUREX_IncidenceAgeSexStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
  }
  
  #prevalence
  
  #filter out male data
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name != "Prostate" )
  
  if (names(table(prevalence_estimates$outcome_cohort_name)[i]) == "Prostate" ){
    
    print("No plot for prostate cancer")
    
    
    
  } else {
    
    prevalence_estimates_i <- prevalence_estimates %>%
      filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
    
    plot1 <- prevalenceFigure5(prevalence_estimates_i)
    
    plotname <- paste0("FIGUREX_PrevalenceAgeSexStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
    
    png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
    
  }
  
  #survival
  
 
  
}

#plot per cancer stratified by database for both, female and males WHOLE AND BOTH GENDERS
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  #incidence rates
  incidence_estimates_i <- incidence_estimates %>%
    filter(outcome_cohort_name == names(table(incidence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")

  plot1 <- incidenceFigure4(incidence_estimates_i)

  plotname <- paste0("FIGURE1_IncidenceGenderAllStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")

  if(names(table(incidence_estimates$outcome_cohort_name)[i]) == "Prostate") {

  png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 7, height = 5, units = "in", res = 1200)

  } else {

  png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)

  }

  print(plot1, newpage = FALSE)
  dev.off()

  #prevalence
  prevalence_estimates_i <- prevalence_estimates %>%
    filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")

  plot1 <- prevalenceFigure4(prevalence_estimates_i)

  plotname <- paste0("FIGURE3_PrevalenceGenderAllStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")


  if(names(table(prevalence_estimates$outcome_cohort_name)[i]) == "Prostate") {

  png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 7, height = 5, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()

  } else {

    png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()

  }


  #survival for whole study period
  survival_estimates_i <- survival_estimates %>%
    filter(Cancer == names(table(survival_estimates$Cancer)[i]) )

  plot1 <- survivalFigure4(survival_estimates_i)

  plotname <- paste0("FIGURE5_KMGenderAllStrat_", names(table(survival_estimates$Cancer)[i]),".png")

  if(names(table(survival_estimates$Cancer)[i]) == "Prostate") {

    png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 7, height = 5, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()


  } else {

    png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
  }
  
  # survival for 5 year study period ----
  survival_estimates_i <- survival_estimates %>%
    filter(Cancer == names(table(survival_estimates$Cancer)[i]) )
  
  plot1 <- survivalFigure5(survival_estimates_i)
  
  plotname <- paste0("FIGURE6_KMCalendarYr_", names(table(survival_estimates$Cancer)[i]),".png")
  
  if(names(table(survival_estimates$Cancer)[i]) == "Prostate") {
  
  png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 7, height = 5, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()
    
    
  } else {
  
  png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()
  }
  
}

##########################################################################################################
### specific updated plots for papers ####
#########################################################################################################


##########################################################################################################
# HEAD AND NECK CANCER SUBTYPES
# INCIDENCE
#plot for all head and neck cancers in one plot
incidenceDatahan <- incidence_estimates %>%
  filter(outcome_cohort_name == "Oral Cavity" |
           outcome_cohort_name == "Tongue" |
           outcome_cohort_name == "Nasal Cavity & Sinus" |
           outcome_cohort_name == "Salivary Gland" |          
           outcome_cohort_name == "Larynx" |
           outcome_cohort_name == "Hypopharynx" |
           outcome_cohort_name == "Nasopharynx" |
           outcome_cohort_name == "Oropharynx"  
           
           )

incidenceFigureData <- incidenceDatahan %>%
  filter(denominator_sex == "Both",
         denominator_age_group == "All", 
         analysis_interval == "years") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name ),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name" ,
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free", ncol = 2)

plotname <- paste0("FIGURE2_IncidenceWholePop_hansubsites.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 8, height = 11, units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()


#PREVALENCE
#plot for all head and neck cancers in one plot
prevalenceDatahan <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Oral Cavity" |
           outcome_cohort_name == "Tongue" |
           outcome_cohort_name == "Nasal Cavity & Sinus" |
           outcome_cohort_name == "Salivary Gland" |          
           outcome_cohort_name == "Larynx" |
           outcome_cohort_name == "Hypopharynx" |
           outcome_cohort_name == "Nasopharynx" |
           outcome_cohort_name == "Oropharynx"  
         
  )

prevalenceFigureData <- prevalenceDatahan %>%
  filter(denominator_sex == "Both",
         denominator_age_group == "All", 
         analysis_interval == "years") %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 1.75) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free", ncol = 2)

plotname <- paste0("FIGURE4_PrevalenceWholePop_hansubsites.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 8, height = 11, units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()


# prevalence stratified by sex and database for all subsites
prevalenceFigureData <- prevalenceDatahan %>%
    filter(denominator_age_group == "All") %>%
    filter(denominator_sex != "Both") %>%
    filter(analysis_interval != "overall") %>% 
    unite(Database_Sex, 
          "database_name", "denominator_sex",
          sep=": ", remove = FALSE) %>% 
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = Database_Sex)) +
    geom_line(color = "black", size = 0.2) +
    scale_colour_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) + #blue, #red #blue, #red 
    scale_fill_manual(values = c("#ED0000FF", "#00468BFF", "#ED0000FF", "#00468BFF")) +
    geom_ribbon(aes(ymin = prevalence_95CI_lower,
                    ymax = prevalence_95CI_upper,
                    fill = Database_Sex), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = Database_Sex, fill = Database_Sex),size = 1.5) +
    scale_shape_manual(values = c(24,22,21,25)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database & Sex",
         shape = "Database & Sex" ,
         fill = "Database & Sex" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ outcome_cohort_name, scales = "free", ncol = 2)
  
plotname <- paste0("FIGURE_S8_PrevalencGender_hansubsites.png")

png(paste0(pathResults ,"/GenderStrat/", plotname),
    width = 8, height = 11, units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()


#############
# incidence by age for each subsite
# removing empty facets

# Hypopharynx
incidence_estimates_hypop <- incidenceDatahan %>%
  filter(outcome_cohort_name == "Hypopharynx" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & 
           denominator_age_group != "30 to 39" &
           denominator_age_group != "40 to 49" &
           denominator_age_group != "90 +" 
         )

plot1 <- incidenceFigure3a(incidence_estimates_hypop)
plotname <- paste0("FIGURE_S2_IncidenceAgeStrat_hypopharynx.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 8, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

# tongue
incidence_estimates_tongue <- incidenceDatahan %>%
  filter(outcome_cohort_name == "Tongue" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & 
           denominator_age_group != "90 +" 
  )

plot1 <- incidenceFigure3a(incidence_estimates_tongue)
plotname <- paste0("FIGURE_S7_IncidenceAgeStrat_tongue.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()



incidenceDatahan_subset <- incidence_estimates %>%
  filter(outcome_cohort_name == "Oral Cavity" |
           outcome_cohort_name == "Salivary Gland" |          
           outcome_cohort_name == "Larynx" |
           outcome_cohort_name == "Oropharynx"  ) %>% 
  filter(analysis_interval == "years") %>% 
  filter(denominator_age_group != "18 to 29" & 
           denominator_age_group != "30 to 39" &
           denominator_age_group != "90 +" )


for(i in 1:length(table(incidenceDatahan_subset$outcome_cohort_name))) {
  
incidenceDatahan_subset1 <- incidenceDatahan_subset %>% 
  filter(outcome_cohort_name == names(table(incidenceDatahan_subset$outcome_cohort_name))[i])
  
plot1 <- incidenceFigure3a(incidenceDatahan_subset1)
  plotname <- paste0("FIGURE_SX_IncidenceAgeStrat_",
                     names(table(incidenceDatahan_subset$outcome_cohort_name))[i],".png")
  
  png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()

}

# # prevalence by age for each subsite
prevalenceDatahan_subset <- prevalence_estimates %>%
  filter( outcome_cohort_name == "Nasal Cavity & Sinus" |
           outcome_cohort_name == "Larynx" |
           outcome_cohort_name == "Hypopharynx"  ) %>% 
  filter(denominator_age_group != "18 to 29" & 
           denominator_age_group != "30 to 39"  )

for(i in 1:length(table(prevalenceDatahan_subset$outcome_cohort_name))) {
  
  prevalenceDatahan_subset1 <- prevalenceDatahan_subset %>% 
    filter(outcome_cohort_name == names(table(prevalenceDatahan_subset$outcome_cohort_name)[i]))
  
  plot1 <- prevalenceFigure3a(prevalenceDatahan_subset1)
  plotname <- paste0("FIGURE_SX_PrevalenceAgeStra_",
                     names(table(prevalenceDatahan_subset$outcome_cohort_name))[i],".png")
  
  png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
  print(plot1, newpage = FALSE)
  dev.off()
  
}


prevalenceDatahan_subset <- prevalenceDatahan %>% 
  filter(outcome_cohort_name == "Nasopharynx" ) %>% 
  filter(denominator_age_group != "90 +"  )

plot1 <- prevalenceFigure3a(prevalenceDatahan_subset)
plotname <- paste0("FIGURE_S13_PrevalenceAgeStra_nasopharynx.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()



prevalenceDatahan_subset <- prevalenceDatahan %>% 
  filter(outcome_cohort_name == "Oropharynx" ) %>% 
  filter(denominator_age_group != "18 to 29")

plot1 <- prevalenceFigure3a(prevalenceDatahan_subset)
plotname <- paste0("FIGURE_S15_PrevalenceAgeStra_oropharynx.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()



#############################################################################################
# breast cancer - make the axis non fixed so can see the IR/prev of the males

#incidence rates
incidence_estimates_i <- incidence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           analysis_interval == "years" &
         denominator_age_group == "All"
         )

# INCDIDENCE

incidenceFigureData <- incidence_estimates_i %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name )) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 2.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        #panel.spacing.x = unit(0.1,"line"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1)) +
ggh4x::facet_grid2(cols = vars(denominator_sex), scales="free", independent = "y") 

plotname <- paste0("FIGURE1_IncidenceGenderAllStrat_Breast.png")

png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)

print(incidenceFigureData, newpage = FALSE)
dev.off()

# PREVALENCE

prevalence_estimates_i <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           denominator_age_group == "All"
  )

prevalenceFigureData <- prevalence_estimates_i %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower,
                  ymax = prevalence_95CI_upper,
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 2.5) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name" ) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1)) +
  ggh4x::facet_grid2(cols = vars(denominator_sex), scales="free", independent = "y") 

plotname <- paste0("FIGURE4_PrevalenceGenderAllStrat_Breast.png")

png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)

print(prevalenceFigureData , newpage = FALSE)
dev.off() 


#breast cancer age effects split by gender - 2 plots created
# females incidence
incidence_estimates_breast_F <- incidence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           analysis_interval == "years" &
           denominator_age_group != "All" &
           denominator_sex == "Female"
  )

incidenceFigureData <- incidence_estimates_breast_F %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE2_IncidenceAgeStrat_Breast_Females.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 6, height = 8, units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()


# males
incidence_estimates_breast_M <- incidence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           analysis_interval == "years" &
           denominator_age_group != "All" &
           denominator_sex == "Male"
  ) %>% 
  filter(denominator_age_group != "18 to 29" & 
           denominator_age_group != "30 to 39" &
           denominator_age_group != "40 to 49" &
           denominator_age_group != "90 +" )

incidenceFigureData <- incidence_estimates_breast_M %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE3_IncidenceAgeStrat_Breast_Males.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 6, height = 6 , units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()

# prevalence for different age groups FEMALES
prevalence_estimates_breast_F <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           denominator_age_group != "All" &
           denominator_sex == "Female"
  )


prevalenceFigureData <- prevalence_estimates_breast_F %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name ),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE5_PrevalenceAgeStrat_Breast_Females.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 6, height = 7 , units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()





prevalence_estimates_breast_M <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           denominator_age_group != "All" &
           denominator_sex == "Male"
  )  %>% 
  filter(denominator_age_group != "18 to 29"  )


prevalenceFigureData <- prevalence_estimates_breast_M %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name ),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE6_PrevalenceAgeStrat_Breast_Males.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 6, height = 7 , units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()


######################################################################################
# Prostate cancer - removing gender facet labels and removing age facets with no data

# incidence whole population
incidenceFigureData <- incidence_estimates %>%
  filter(denominator_sex == "Male",
         denominator_age_group == "All",
         analysis_interval == "years" ,
         outcome_cohort_name == "Prostate") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 3.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name" ) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1))


plotname <- paste0("FIGURE1_Incidence_Males_Prostate.png")
png(paste0(pathResults ,"/WholePop/", plotname), width = 6, height = 5 , units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()

# prevalence whole population

prevalenceFigureData <- prevalence_estimates %>%
  filter(denominator_sex == "Male",
         denominator_age_group == "All",
         outcome_cohort_name == "Prostate") %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 3.5) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.border = element_rect(colour = "black", fill=NA, size=0.6),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name" ) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1))


plotname <- paste0("FIGURE3_Prevalence_Males_Prostate.png")
png(paste0(pathResults ,"/WholePop/", plotname), width = 6, height = 5 , units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()

# KM for prostate cancer
survivalFigureData <- survival_estimates %>%
  filter(Stratification == "None") %>%
  filter(CalendarYearGp == "2000 to 2019") %>%
  filter(Cancer == "Prostate") %>%
  filter(Age == "All") %>%
  ggplot(aes(x = time,
             y = est,
             group = Database,
             col = Database )) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = lcl, 
                  ymax = ucl, 
                  fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
  geom_line(aes(linetype = Database),size = 0.5) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
  labs(x = "Time (Years)",
       y = "Survival Probability",
       col = "Database name",
       linetype = "Database name") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_x_continuous(breaks=seq(0, 20, 2))

plotname <- paste0("FIGURE5_KM_Males_Prostate.png")
png(paste0(pathResults ,"/WholePop/", plotname), width = 6, height = 5 , units = "in", res = 1200)
print(survivalFigureData, newpage = FALSE)
dev.off()

# prostate cancer IR for different age groups
incidenceFigureData <- incidence_estimates %>%
  filter(denominator_sex == "Male",
         analysis_interval == "years" ,
         denominator_age_group != "All", 
         denominator_age_group != "18 to 29", 
         denominator_age_group != "30 to 39", 
        outcome_cohort_name == "Prostate") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE2_Incidence_AgeStrat_Males_Prostate.png")
png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 7 , units = "in", res = 1200)
print(incidenceFigureData , newpage = FALSE)
dev.off()


# prostate cancer IR for different age groups

prevalenceFigureData <- prevalence_estimates %>%
  filter(denominator_sex == "Male",
         denominator_age_group != "All", 
         denominator_age_group != "18 to 29", 
         denominator_age_group != "30 to 39", 
         outcome_cohort_name == "Prostate") %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name ),size = 1.5) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)

plotname <- paste0("FIGURE4_Prevalence_AgeStrat_Males_Prostate.png")
png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 7 , units = "in", res = 1200)
print(prevalenceFigureData , newpage = FALSE)
dev.off()


# survival over calender time prostate 

survivalFigureData <- survival_estimates %>%
  filter(Age == "All") %>%
  filter(Cancer == "Prostate") %>% 
  filter(CalendarYearGp != "2000 to 2019") %>%
  ggplot(aes(x = time,
             y = est,
             group = CalendarYearGp,
             col = CalendarYearGp )) +
  scale_y_continuous( labels = label_percent() ) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
  scale_linetype_manual(values = c("dotted","dashed", "dotdash", "solid")) +
  labs(x = "Time (Years)",
       y = "Survival Probability",
       col = "Calendar Year Group",
       linetype = "Calendar Year Group") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  ggh4x::facet_grid2(cols = vars(Database), scales="free", independent = "y") 


plotname <- paste0("FIGURE6_KM_CY_Males_Prostate.png")
png(paste0(pathResults ,"/WholePop/", plotname), width = 8, height = 4 , units = "in", res = 1200)
print(survivalFigureData , newpage = FALSE)
dev.off()


# age effects KM for prostate cancer

survivalFigureData <- survival_estimates %>%
  filter(Age != "All") %>%
  filter(Age != "18 to 29", 
         Age != "30 to 39") %>%
  filter(CalendarYearGp == "2000 to 2019") %>%
  filter(Cancer == "Prostate") %>%
  ggplot(aes(x = time,
             y = est,
             group = Database,
             col = Database )) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = lcl, 
                  ymax = ucl, 
                  fill = Database), alpha = .15, color = NA, show.legend = FALSE) +
  geom_line(aes(linetype = Database),size = 0.5) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash","dotted")) +
  labs(x = "Time (Years)",
       y = "Survival Probability",
       col = "Database name",
       linetype = "Database name") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_x_continuous(breaks=seq(0, 20, 2)) +
 # ggh4x::facet_grid2(rows = vars(Age), scales="free", independent = "y") 
    facet_wrap(~ Age, ncol = 2, scales = "free_y") +
    coord_cartesian(xlim = c(0, 20))

plotname <- paste0("FIGURE7_KM_ageStrat_Males_Prostate.png")
png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 6 , units = "in", res = 1200)
print(survivalFigureData , newpage = FALSE)
dev.off()

###############################################################################################
# liver cancer
#incidence rates age strat
incidence_estimates_liver <- incidence_estimates %>%
  filter(outcome_cohort_name == "Liver" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39"  )

plot1 <- incidenceFigure3a(incidence_estimates_liver)
plotname <- paste0("FIGURE2_IncidenceAgeStrat_liver.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


#incidence rates age*gender strat
incidence_estimates_liver1 <- incidence_estimates %>%
  filter(outcome_cohort_name == "Liver" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39" &
           denominator_age_group != "90 +" )

plot1 <- incidenceFigure5(incidence_estimates_liver1)
plotname <- paste0("FIGURES1_IncidenceAgeSexStrat_liver.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

###############################################################################################
# lung cancer
#incidence rates age strat
incidence_estimates_lung <- incidence_estimates %>%
  filter(outcome_cohort_name == "Lung" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29"  )

plot1 <- incidenceFigure3a(incidence_estimates_lung)
plotname <- paste0("FIGURE2_IncidenceAgeStrat_lung.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


#incidence rates age*gender strat
incidence_estimates_lung1 <- incidence_estimates %>%
  filter(outcome_cohort_name == "Lung" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29"  )

plot1 <- incidenceFigure5(incidence_estimates_lung1)
plotname <- paste0("FIGURES1_IncidenceAgeSexStrat_lung.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


##############################################################################################

# Oesophageal cancer
#incidence rates age strat
incidence_estimates_Oesophageal <- incidence_estimates %>%
  filter(outcome_cohort_name == "Esophagus" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" &
           denominator_age_group != "30 to 39" 
           )

plot1 <- incidenceFigure3a(incidence_estimates_Oesophageal)
plotname <- paste0("FIGURE2_IncidenceAgeStrat_Esophagus.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


#incidence rates age*gender strat
incidence_estimates_Oesophageal1 <- incidence_estimates %>%
  filter(outcome_cohort_name == "Esophagus" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" &
           denominator_age_group != "30 to 39" )

plot1 <- incidenceFigure5(incidence_estimates_Oesophageal1)
plotname <- paste0("FIGURES1_IncidenceAgeSexStrat_Esophagus.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

# PP for age* sex strat
prevalence_estimates_Oesophageal1 <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Esophagus" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29"  )

plot1 <- prevalenceFigure5(prevalence_estimates_Oesophageal1)
plotname <- paste0("FIGURES1_prevalenceAgeSexStrat_Esophagus.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()






##############################################################################################
#GASTRIC CANCER

#incidence rates age strat
incidence_estimates_gastric <- incidence_estimates %>%
  filter(outcome_cohort_name == "Stomach" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39"  )

plot1 <- incidenceFigure3a(incidence_estimates_gastric)
plotname <- paste0("FIGURE2_IncidenceAgeStrat_stomach.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

#incidence rates age*gender strat
incidence_estimates_gastric1 <- incidence_estimates %>%
  filter(outcome_cohort_name == "Stomach" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39"  )

plot1 <- incidenceFigure5(incidence_estimates_gastric1)
plotname <- paste0("FIGURES1_IncidenceAgeSexStrat_stomach.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()



##########################################################
# pancreatic cancer 
# INCIDENCE AGE PLOT

#incidence rates age strat
incidence_estimates_pancreas <- incidence_estimates %>%
  filter(outcome_cohort_name == "Pancreas" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39"  )

plot1 <- incidenceFigure3a(incidence_estimates_pancreas)
plotname <- paste0("FIGURE2_IncidenceAgeStrat_pancreas_updated.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

# prevalance
prevalence_estimates_pancreas <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Pancreas") %>%
  filter(denominator_age_group != "18 to 29" )

plot1 <- prevalenceFigure3a(prevalence_estimates_pancreas)
plotname <- paste0("FIGURE4_PrevalenceAgeStrat_pancreas_updated.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


#incidence rates age*gender strat
incidence_estimates_pancreas1 <- incidence_estimates %>%
  filter(outcome_cohort_name == "Pancreas" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "30 to 39"  )

plot1 <- incidenceFigure5(incidence_estimates_pancreas1)
plotname <- paste0("FIGURES1_IncidenceAgeSexStrat_pancreas.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

##############################################################################
# other plots

## colorectal cancer incidence removing age groups not needed
incidence_estimates_crc<- incidence_estimates %>%
  filter(outcome_cohort_name == "Colorectal" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29" & denominator_age_group != "90 +"  )

#incidence figure3a age stratification and stratified by database (AGE ON EACH FACET) UPDATED 3 columns
incidenceFigure3c <- function(incidenceData) {
  
  incidenceFigureData <- incidenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group != "All") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = database_name)) +
    geom_line(color = "black", size = 0.25) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                    ymax = incidence_100000_pys_95CI_upper, 
                    fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
    geom_point(aes(shape = database_name, fill = database_name),size = 1.5) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.position = "bottom",
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
                 expand = c(0.06,1)) +
    facet_wrap(~ denominator_age_group, scales = "free", ncol = 3)
  
  
  return(incidenceFigureData)
}

plot1 <- incidenceFigure3c(incidence_estimates_crc)

plotname <- paste0("FIGURE_fellowship_IR_CRC.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 5, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()

######################################
# DARWIN CODE ALL CANCERS IN A FACET

#############
#incidence
#################

incidenceData <- incidence_estimates %>%
  filter(outcome_cohort_name == "Breast" |
           outcome_cohort_name == "Colorectal" |
           outcome_cohort_name == "Head & Neck" |
           outcome_cohort_name == "Liver"  )

incidenceFigureData <- incidenceData %>%
  filter(denominator_sex == "Both",
         denominator_age_group == "All") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             col = database_name,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .3, color = NA, show.legend = FALSE) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        #panel.border = element_blank() ,
        #panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +

  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free", ncol = 2)

plotname <- paste0("IncidenceRatesWholePop_multipleCancers.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 10, height = 8, units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()


### prevalence 

prevalenceData <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Breast" |
           outcome_cohort_name == "Colorectal" |
           outcome_cohort_name == "Head & Neck" |
           outcome_cohort_name == "Liver"  )

prevalenceFigureData <- prevalenceData %>%
  filter(denominator_sex == "Both",
         denominator_age_group == "All") %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             col = database_name,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, 
                  ymax = prevalence_95CI_upper, 
                  fill = database_name), alpha = .3, color = NA, show.legend = FALSE) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        #panel.border = element_blank() ,
        #panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free", ncol = 2)

plotname <- paste0("PrevalenceRatesWholePop_multipleCancers.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 10, height = 8, units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()

##############################################
# 4 cancers gender strat

incidenceFigureData1 <- incidenceData %>%
  filter(denominator_sex != "Both",
         denominator_age_group == "All") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             col = database_name,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .3, color = NA, show.legend = FALSE) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        #panel.border = element_blank() ,
        #panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name + denominator_sex, scales = "free", ncol = 2)

plotname <- paste0("IncidenceRatesGender_multipleCancers.png")

png(paste0(pathResults ,"/GenderStrat/", plotname),
    width = 8, height = 10, units = "in", res = 1200)
print(incidenceFigureData1, newpage = FALSE)
dev.off()

###########################################
# age strat per year

incidenceFigureData2 <- incidenceData %>%
  filter(denominator_sex == "Both",
         denominator_age_group != "All",
         outcome_cohort_name == "Breast",
         database_name == "CPRD Gold") %>%
  ggplot(aes(x = as.factor(incidence_start_date),
             y = incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper,
             col = denominator_age_group,
             group = denominator_age_group)) +
  geom_errorbar(position = position_dodge(width =10)) +
  geom_point(size = 1.5, position = position_dodge(width =10)) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Age Group")


incidenceFigureData2 <- incidenceData %>%
  filter(denominator_sex == "Both",
         denominator_age_group != "All",
         outcome_cohort_name == "Breast",
         database_name == "CPRD Gold") %>%
  ggplot( aes(x = incidence_start_date, y = incidence_100000_pys)) +
  geom_point(aes(color=denominator_age_group), position=position_dodge(width=180))+
  geom_errorbar(aes(color=denominator_age_group, ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper),
                width=0.2, position=position_dodge(width=180)) +
  xlab("Calender year") +
  ylab("Incidence rate per 100000 person-years") +
  scale_y_continuous(
    limits = c(0, NA), breaks=pretty_breaks() ) +
  scale_colour_manual(values = c( "#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + # colours from ggpalette for colour blind
  scale_fill_manual(values = c( "#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Age Group") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "2 year",
               date_labels = "%Y")


#### need this for cancer papers
# facet per age group

incidenceFigureData3 <- incidence_estimates %>%
  filter(denominator_sex == "Both",
         denominator_age_group != "All",
         outcome_cohort_name == "Breast") %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             col = database_name,
             group = database_name)) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .3, color = NA, show.legend = FALSE) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        #panel.border = element_blank() ,
        #panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ denominator_age_group, scales = "free", ncol = 2)


plotname <- paste0("IncidenceRatesAgeGroup_BreastCancer.png")

png(paste0(pathResults ,"/AgeStrat/", plotname),
    width = 8, height = 10, units = "in", res = 1200)
print(incidenceFigureData3, newpage = FALSE)
dev.off()

########################################










####################################
# head and neck cancer subtypes
# subset the data

incidence_estimates_han <- incidence_estimates %>%
  filter(grepl("HeadNeckSubtype", outcome_cohort_name))


for(i in 1:length(table(incidence_estimates_han$outcome_cohort_name))) {
  
  # #incidence rates
  # incidence_estimates_i <- incidence_estimates_han %>%
  #   filter(outcome_cohort_name == names(table(incidence_estimates_han$outcome_cohort_name)[i]) & analysis_interval == "years")
  # 
  # plot1 <- incidenceFigure4(incidence_estimates_i)
  # 
  # plotname <- paste0("IncidenceGenderAllStrat_", names(table(incidence_estimates$outcome_cohort_name)[i]),".png")
  # png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  # 
  # print(plot1, newpage = FALSE)
  # dev.off()
  # 
  #prevalence
  # prevalence_estimates_i <- prevalence_estimates %>%
  #   filter(outcome_cohort_name == names(table(prevalence_estimates$outcome_cohort_name)[i]) & analysis_interval == "years")
  # 
  # plot1 <- prevalenceFigure4(prevalence_estimates_i)
  # 
  # plotname <- paste0("PeriodPrevalenceGenderAllStrat_", names(table(prevalence_estimates$outcome_cohort_name)[i]),".png")
  # 
  # png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  # print(plot1, newpage = FALSE)
  # dev.off()
  
  #survival
  # survival_estimates_i <- survival_estimates %>%
  #   filter(Cancer == names(table(survival_estimates$Cancer)[i]) )
  # 
  # plot1 <- survivalFigure4(survival_estimates_i)
  # 
  # plotname <- paste0("KMSurvivalGenderAllStrat_", names(table(survival_estimates$Cancer)[i]),".png")
  # 
  # png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 10, height = 5, units = "in", res = 1200)
  # print(plot1, newpage = FALSE)
  # dev.off()
  
}






