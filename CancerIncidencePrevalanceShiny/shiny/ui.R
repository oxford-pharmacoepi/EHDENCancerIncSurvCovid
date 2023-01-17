#### PACKAGES -----
options(encoding = "UTF-8")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(stringr)
library(tidyr)
library(ggalt)

#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),
                 
# title ------ 
# shown across tabs
titlePanel("Incidence and Prevalence of Different cancers"),
               
# set up: pages along the side -----  
                 navlistPanel(
                   
                   
## Introduction  -----  
tabPanel("Background", 
  tags$h3("Background"),
     tags$hr(),
   tags$h4(tags$strong("Please note, the results presented here should be considered as 
                       preliminary and subject to change.")),
   tags$hr(),
    tags$h5("This app is a companion to the study focussing on determining the incidence and prevalence for specific cancers. 
    We will focus on the 8 cancers (Breast, Colorectal, Lung, Liver, Stomach, Head/neck, Prostate and Pancreas). 
    The databases use data from 2000-2019. We used the the data for this time period in accordance with the approved CPRD application
            for the EHDEN wp2 HTA cancer survival use case where we did not the impact of the COVID on the results"),
  HTML('<br>'),
  tags$hr()
), 
## Prevalence ------ 
tabPanel("Population prevalence",	  
         tags$h3("Prevalence estimates"),
         tags$h5("Prevalence estimates are shown below...."),
         tags$hr(),
         tags$h5("Database and study outcome"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_database_name_selector",
                         label = "Database",
                         choices = unique(prevalence_estimates$database_name),
                         selected = unique(prevalence_estimates$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_outcome_cohort_name_selector",
                         label = "Outcome",
                         choices = sort(unique(prevalence_estimates$outcome_cohort_name)),
                         selected = c("Psoriasis"),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tags$hr(),
         tags$h5("Population settings"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_denominator_age_group_selector",
                         label = "Age group",
                         choices = levels(prevalence_estimates$denominator_age_group),
                         selected = "All ages",
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_denominator_sex_selector",
                         label = "Sex",
                         choices = unique(prevalence_estimates$denominator_sex),
                         selected = "Both",
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_denominator_days_prior_history_selector",
                         label = "Days prior history",
                         choices = unique(prevalence_estimates$denominator_days_prior_history),
                         selected = 365,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tags$hr(),
         tags$h5("Analysis settings"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "prevalence_start_date_selector",
                         label = "Prevalence start date",
                         choices = as.character(unique(prevalence_estimates$prevalence_start_date)),
                         selected = as.character(unique(prevalence_estimates$prevalence_start_date)),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_prevalence_estimates') %>% withSpinner()), 
                     tabPanel("Plot of estimates",
                              tags$hr(),
                              tags$h5("Plotting options"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "prevalence_x_axis",
                                              label = "X axis",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "prevalence_start_date"),
                                              selected = "prevalence_start_date",
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = FALSE,)
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "prevalence_plot_facet",
                                              label = "Facet by",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "prevalence_start_date"),
                                              selected = c("outcome_cohort_name",
                                                           "database_name"),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "prevalence_plot_group",
                                              label = "Colour by",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "prevalence_start_date"),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              plotlyOutput('plot_prevalence_estimates', height = "800px") %>% withSpinner() ), 
                     tabPanel("Attrition table", 
                              DTOutput('tbl_prevalence_attrition') %>% withSpinner())
                     )
),

## Incidence ------ 
tabPanel("Population incidence",	  
         tags$h3("Incidence estimates"),
         tags$h5("Incidence estimates are shown below...."),
         tags$hr(),
         tags$h5("Database and study outcome"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_database_name_selector",
                         label = "Database",
                         choices = unique(incidence_estimates$database_name),
                         selected = unique(incidence_estimates$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_outcome_cohort_name_selector",
                         label = "Outcome",
                         choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                         selected = c("Psoriasis"),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tags$hr(),
         tags$h5("Population settings"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_denominator_age_group_selector",
                         label = "Age group",
                         choices = levels(incidence_estimates$denominator_age_group),
                         selected = "All ages",
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_denominator_sex_selector",
                         label = "Sex",
                         choices = unique(incidence_estimates$denominator_sex),
                         selected = "Both",
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_denominator_days_prior_history_selector",
                         label = "Days prior history",
                         choices = unique(incidence_estimates$denominator_days_prior_history),
                         selected = 365,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tags$hr(),
         tags$h5("Analysis settings"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "incidence_start_date_selector",
                         label = "incidence start date",
                         choices = as.character(unique(incidence_estimates$incidence_start_date)),
                         selected = as.character(unique(incidence_estimates$incidence_start_date)),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_incidence_estimates') %>% withSpinner()), 
                     tabPanel("Plot of estimates",
                              tags$hr(),
                              tags$h5("Plotting options"),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "incidence_x_axis",
                                              label = "X axis",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "incidence_start_date"),
                                              selected = "incidence_start_date",
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = FALSE,)
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "incidence_plot_facet",
                                              label = "Facet by",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "incidence_start_date"),
                                              selected = c("outcome_cohort_name",
                                                           "database_name"),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "incidence_plot_group",
                                              label = "Colour by",
                                              choices = c("denominator_age_group", 
                                                          "denominator_sex",
                                                          "denominator_days_prior_history",
                                                          "outcome_cohort_name",
                                                          "database_name",
                                                          "incidence_start_date"),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = TRUE,)
                              ),
                              plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() ), 
                     tabPanel("Attrition table", 
                              DTOutput('tbl_incidence_attrition') %>% withSpinner())
                     )
)
# close -----
                                                   ))



