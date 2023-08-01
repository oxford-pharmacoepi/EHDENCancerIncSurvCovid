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
    geom_point(aes(shape = database_name, fill = database_name ),size = 2) +
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
    geom_point(size = 2) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 1) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 1) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 2) +
    scale_shape_manual(values = c(24,21)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          legend.position='bottom',
          legend.box.spacing = unit(0, "pt") ,
          #panel.spacing.x = unit(0.1,"line"),
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         shape = "Database name",
         fill = "Database name") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
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
    geom_point(aes(shape = Database_Sex, fill = Database_Sex),size = 1) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 2) +
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
    geom_point(aes(shape = database_name),size = 2) +
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
    geom_point(aes(shape = database_name, fill = database_name ),size = 1) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 1) +
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
    geom_point(aes(shape = database_name, fill = database_name),size = 2) +
    scale_shape_manual(values = c(24,21)) +
    scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
          strip.background = element_rect(color = "black", size = 0.6) ,
          panel.background = element_blank() ,
          legend.position='bottom',
          legend.box.spacing = unit(0, "pt") ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         shape = "Database name",
         fill = "Database name" ) +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years"),
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
    geom_point(aes(shape = Database_Sex, fill = Database_Sex),size = 1) +
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
    filter(CalendarYearGp == "2000 to 2019" | CalendarYearGp == "2000 to 2021") %>%
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
          legend.position='bottom',
          legend.box.spacing = unit(0, "pt") ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    scale_x_continuous(breaks=seq(0, 22, 2)) +
    facet_grid(cols = vars(Gender)) 
  
  return(survivalFigureData)
  
}

# survival figure 5 whole population and gender strat with calendar year strat (puts gender as column and database as rows)
survivalFigure5 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    filter(Stratification == "None"| Stratification == "Gender") %>%
    filter(CalendarYearGp != "2000 to 2019") %>%
    filter(CalendarYearGp != "2000 to 2021") %>%
    
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
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
    filter(CalendarYearGp != "2000 to 2021") %>%
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

# survival figure 7 whole population only GOLD calendar time effects
survivalFigure7 <- function(survivalData) {
  
 survivalFigureData <- survivalData %>%
    # #filter(Stratification == "None") %>%
    # filter(CalendarYearGp != "2000 to 2019") %>%
    # filter(CalendarYearGp != "2000 to 2021") %>%
    # filter(Database == "CPRD GOLD") %>% 
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
   geom_ribbon(aes(ymin = lcl, 
                   ymax = ucl, 
                   fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
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
   xlim(0, 2) +
 facet_grid(cols = vars(Cancer)) 
  
  return(survivalFigureData)
  
}

# survival figure 8 whole population only GOLD calendar time effects with new colours
survivalFigure8 <- function(survivalData) {
  
  survivalFigureData <- survivalData %>%
    # #filter(Stratification == "None") %>%
    # filter(CalendarYearGp != "2000 to 2019") %>%
    # filter(CalendarYearGp != "2000 to 2021") %>%
    # filter(Database == "CPRD GOLD") %>% 
    ggplot(aes(x = time,
               y = est,
               group = CalendarYearGp,
               col = CalendarYearGp )) +
    scale_y_continuous( labels = label_percent() ) +
    scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
    scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
    geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
    scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
    geom_ribbon(aes(ymin = lcl, 
                    ymax = ucl, 
                    fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
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
    xlim(0, 2) +
    facet_grid(cols = vars(Cancer)) 
  
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
survival_rates <- readRDS(paste0(datapath ,"/survival_rates_table.rds")) %>% 
  filter(Database == "CPRD GOLD", time == 1) %>% 
  filter(Cancer == "Colorectal" |
           Cancer == "Head & Neck" |
           Cancer == "Liver" |
           Cancer == "Lung" |
           Cancer == "Oesophagus" |
           Cancer == "Pancreas" |
           Cancer == "Breast" |
           Cancer == "Prostate" |
           Cancer == "Stomach" ) %>% 
  filter( CalenderYearGp != "2000 to 2021")
  

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
  
  # #survival
  # 
  # #filter out male data
  # survival_estimates_i <- survival_estimates %>%
  #   filter(Cancer != "Prostate" )
  # 
  # if (names(table(survival_estimates$Cancer)[i]) == "Prostate" ){
  #   
  #   survival_estimates_i <- survival_estimates %>%
  #     filter(Cancer == "Prostate")
  #   
  #   plot1 <- survivalFigure3b(survival_estimates_i)
  #   
  #   plotname <- paste0("FIGUREX_KMAgeStrat_", names(table(survival_estimates$Cancer)[i]),".png")
  #   
  #   png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
  #   print(plot1, newpage = FALSE)
  #   dev.off()   
  #   
  #   
  #   
  # } else {
  #   
  #   survival_estimates_i <- survival_estimates %>%
  #     filter(Cancer == names(table(survival_estimates$Cancer)[i]))
  #   
  #   plot1 <- survivalFigure3a(survival_estimates_i)
  #   
  #   plotname <- paste0("FIGUREX_KMAgeStrat_", names(table(survival_estimates$Cancer)[i]),".png")
  #   
  #   png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
  #   print(plot1, newpage = FALSE)
  #   dev.off()
  #   
  # }
  
  
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

# create data for this also including breast cancer females 
survivalData <- survival_estimates %>%
  filter(Stratification == "None") %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  filter(CalendarYearGp != "2000 to 2021") %>%
  filter(Database == "CPRD GOLD") %>% 
  filter(Cancer != "Breast")

#create a subset for breast cancer females
survivalData1 <- survival_estimates %>%
  filter(Stratification == "Gender") %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  filter(CalendarYearGp != "2000 to 2021") %>%
  filter(Database == "CPRD GOLD") %>% 
  filter(Cancer == "Breast") %>% 
  filter(Gender != "Male")

#combine data back together
survivalData2 <- bind_rows(survivalData,survivalData1)

#plot per cancer for GOLD with no stratification
for(i in 1:length(table(incidence_estimates$outcome_cohort_name))) {
  
  survival_estimates_i <- survivalData2 %>%
    filter(Cancer == names(table(survival_estimates$Cancer)[i]) )
  
  #plot1 <- survivalFigure7(survival_estimates_i)
  plot1 <- survivalFigure8(survival_estimates_i)
  
  plotname <- paste0("FIGURE7_KMCalendarYr_GOLD_", names(table(survival_estimates$Cancer)[i]),".png")
    
    png(paste0(pathResults ,"/GenderWholeStrat/", plotname), width = 7, height = 5, units = "in", res = 1200)
    print(plot1, newpage = FALSE)
    dev.off()

}



##########################################################################################################
### specific updated plots for papers ####
#########################################################################################################


#survival facetted per cancer

survivalFigureData <- survivalData %>%
  filter(Stratification == "None") %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  filter(CalendarYearGp != "2000 to 2021") %>%
  filter(Database == "CPRD GOLD") %>% 
  ggplot(aes(x = time,
             y = est,
             group = CalendarYearGp,
             col = CalendarYearGp )) +
  scale_y_continuous( labels = label_percent() ) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
  scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
  geom_ribbon(aes(ymin = lcl, 
                  ymax = ucl, 
                  fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
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
  xlim(0, 2) +
  facet_grid(cols = vars(Cancer)) 




# pointplots per cancer survival over time
survival_rates1 <- survival_rates %>% 
  filter(Stratification == "None") %>% 
  filter(Cancer != "Breast") %>% 
  filter(Cancer != "Prostate")


survival_rates2 <- survival_rates %>% 
  filter(Cancer == "Breast" & Gender == "Female" & Age == "All") 

survival_rates3 <- survival_rates %>% 
  filter(Cancer == "Prostate" & Gender == "Male" & Age == "All") 

survival_rates4 <- bind_rows(
  survival_rates1 ,
  survival_rates2,
  survival_rates3
) %>% 
  mutate(Survival = surv*100) %>% 
  mutate(lower = lower *100) %>% 
  mutate(upper = upper *100)

#create a bar chart for each survival faceted by cancer
survival_rate_figure <- survival_rates4 %>% 
ggplot(aes(x=CalenderYearGp, y=Survival)) +
geom_point() +
#  geom_linerange(aes(ymin=lower, ymax=upper)) +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
theme(axis.text.x = element_text(angle = 45, hjust=0.95, size = 6),
      panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
      strip.background = element_rect(color = "black", size = 0.6) ,
      panel.background = element_blank() ,
      axis.line = element_line(colour = "black", size = 0.6) ,
      panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed")) +
  labs(x = "Calendar year",
       y = "One year survival (%)") +
  facet_wrap(~Cancer, scales = "free_y")

plotname <- paste0("ShorttermsurvivalmultipleCancers.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 8, height = 8, units = "in", res = 1200)
print(survival_rate_figure, newpage = FALSE)
dev.off()


################################## INCIDENCE #######
#plot for all cancer IRs on one facet
incidenceData <- incidence_estimates %>%
  filter(outcome_cohort_name == "Colorectal" |
           outcome_cohort_name == "Head & Neck" |
           outcome_cohort_name == "Liver" |
           outcome_cohort_name == "Lung" |
           outcome_cohort_name == "Oesophagus" |
           outcome_cohort_name == "Pancreas" |
           outcome_cohort_name == "Stomach"  ) %>% 
  filter(analysis_interval == "years") %>% 
  filter(denominator_sex == "Both", denominator_age_group == "All") %>% 
  filter(database_name == "CPRD GOLD")

  incidenceData1 <- incidence_estimates %>%
    filter(outcome_cohort_name == "Breast" ) %>% 
    filter(analysis_interval == "years") %>% 
    filter(denominator_sex == "Female", denominator_age_group == "All") %>% 
  filter(database_name == "CPRD GOLD")
  
  incidenceData2 <- incidence_estimates %>%
    filter(outcome_cohort_name == "Prostate" ) %>% 
    filter(analysis_interval == "years") %>% 
    filter(denominator_sex == "Male", denominator_age_group == "All") %>% 
    filter(database_name == "CPRD GOLD")
  
#bind rows all data together 
incidenceData3 <- bind_rows(incidenceData, incidenceData1, incidenceData2  )
  
incidenceFigureData <- incidenceData3 %>%
  ggplot(aes(x = incidence_start_date,
             y = incidence_100000_pys,
             group = database_name )) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, 
                  ymax = incidence_100000_pys_95CI_upper, 
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 2) +
  scale_shape_manual(values = c(24,21)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        #panel.spacing.x = unit(0.1,"line"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position='none') +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", colour = "#ED0000FF", size = 0.8) +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free_y", ncol = 3)


plotname <- paste0("IRsWholePop_multipleCancers.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 8, height = 7.5, units = "in", res = 1200)
print(incidenceFigureData, newpage = FALSE)
dev.off()


################################## Prevalence #######
#plot for all cancer IRs on one facet
prevalenceData <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Colorectal" |
           outcome_cohort_name == "Head & Neck" |
           outcome_cohort_name == "Liver" |
           outcome_cohort_name == "Lung" |
           outcome_cohort_name == "Oesophagus" |
           outcome_cohort_name == "Pancreas" |
           outcome_cohort_name == "Stomach"  ) %>% 
  filter(denominator_sex == "Both", denominator_age_group == "All") %>% 
  filter(database_name == "CPRD GOLD")

prevalenceData1 <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Breast" ) %>% 
  filter(denominator_sex == "Female", denominator_age_group == "All") %>% 
  filter(database_name == "CPRD GOLD")

prevalenceData2 <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Prostate" ) %>% 
  filter(denominator_sex == "Male", denominator_age_group == "All") %>% 
  filter(database_name == "CPRD GOLD")

#bind rows all data together 
prevalenceData3 <- bind_rows(prevalenceData, prevalenceData1, prevalenceData2  )

prevalenceFigureData <- prevalenceData3 %>%
  ggplot(aes(x = prevalence_start_date,
             y = prevalence,
             group = database_name )) +
  geom_line(color = "black", size = 0.25) +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower,
                  ymax = prevalence_95CI_upper,
                  fill = database_name), alpha = .15, color = NA, show.legend = FALSE) +
  geom_point(aes(shape = database_name, fill = database_name),size = 2) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous( labels = scales::percent, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        strip.background = element_rect(color = "black", size = 0.6) ,
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        #panel.spacing.x = unit(0.1,"line"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.position='none') +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", colour = "#ED0000FF", size = 0.8) +
  labs(x = "Calendar year",
       y = "Prevalence",
       col = "Database name",
       shape = "Database name",
       fill = "Database name") +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"),
               expand = c(0.06,1)) +
  facet_wrap(~ outcome_cohort_name, scales = "free_y", ncol = 3)


plotname <- paste0("PPsWholePop_multipleCancers.png")

png(paste0(pathResults ,"/WholePop/", plotname),
    width = 8, height = 7.5, units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()


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
# create one removing both results only include males and females as 99% are females
#incidence rates
incidence_estimates_i <- incidence_estimates %>%
  filter(outcome_cohort_name == "Breast" & 
           analysis_interval == "years" &
         denominator_age_group == "All" &
           denominator_sex != "Both"
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
           denominator_age_group == "All" &
           denominator_sex != "Both"
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

plotname <- paste0("FIGURE3_PrevalenceGenderAllStrat_Breast.png")

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

plotname <- paste0("FIGURES3_IncidenceAgeStrat_Breast_Males.png")

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

plotname <- paste0("FIGURE4_PrevalenceAgeStrat_Breast_Females.png")

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

plotname <- paste0("FIGURE5_PrevalenceAgeStrat_Breast_Males.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 6, height = 7 , units = "in", res = 1200)
print(prevalenceFigureData, newpage = FALSE)
dev.off()

# survival plots for females and males
survival_estimates_breast <- survival_estimates %>%
  filter(Cancer == "Breast" & 
           Age == "All" &
           Gender != "Both" &
           CalendarYearGp == "2000 to 2019"
  )  


survivalFigureData <- survival_estimates_breast %>%
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

plotname <- paste0("FIGURE6_KMGenderAllStrat_Breast.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 5 , units = "in", res = 1200)
print(survivalFigureData, newpage = FALSE)
dev.off()


#calendar time - remove both population
survival_estimates_breast1 <- survival_estimates %>%
  filter(Cancer == "Breast" & 
           Age == "All" &
           Gender != "Both" &
           CalendarYearGp != "2000 to 2019"
  ) 

survivalFigureData <- survival_estimates_breast1 %>%
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

plotname <- paste0("FIGURE7_KMCalendarYr_Breast.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 6 , units = "in", res = 1200)
print(survivalFigureData, newpage = FALSE)
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


#prevalence rates age strat
prevalence_estimates_Oesophageal <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Esophagus" ) %>%
  filter(denominator_age_group != "18 to 29" &
           denominator_age_group != "All"
           )
  

plot1 <- prevalenceFigure3a(prevalence_estimates_Oesophageal)
plotname <- paste0("FIGURE4_PrevalenceAgeStrat_Esophagus.png")

png(paste0(pathResults ,"/AgeStrat/", plotname), width = 8, height = 10, units = "in", res = 1200)
print(plot1, newpage = FALSE)
dev.off()


# PP for age* sex strat
prevalence_estimates_Oesophageal1 <- prevalence_estimates %>%
  filter(outcome_cohort_name == "Esophagus" & analysis_interval == "years") %>%
  filter(denominator_age_group != "18 to 29"  )

plot1 <- prevalenceFigure5(prevalence_estimates_Oesophageal1)
plotname <- paste0("FIGURES2_prevalenceAgeSexStrat_Esophagus.png")

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






