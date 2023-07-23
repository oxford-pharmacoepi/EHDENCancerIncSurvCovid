library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)
library(ggplot2)
library(scales)
library(ggh4x)
library(readr)


#folder of processed data
datapath <- "C:/Users/dnewby/Documents/GitHub/EHDENCancerIncidencePrevalence/CancerIncidencePrevalanceShiny/shiny/data"
#path to european population standard 2013
ESP13path <- "C:/Users/dnewby/Documents/GitHub/EHDENCancerIncidencePrevalence/4_ageStandardization/ESP13.csv"

#read in the incidence and prevalence files
prevalence_estimates <- readRDS(paste0(datapath ,"/prevalence_estimates.rds"))
incidence_estimates <- readRDS(paste0(datapath ,"/incidence_estimates.rds"))
ESP13 <- read_csv(ESP13path)

#collapse ESP13 and remove ages not used for study
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

#%>% 
 # mutate(Agegroup = replace(Agegroup, Agegroup == "90+", "90"))

#%>% 
# add_row(Agegroup = "All Ages", ESP2013 = sum(as.numeric(ESP13$ESP2013), na.rm = TRUE))


# try with one cancer : colorectal
incidence_estimates_CRC <- incidence_estimates %>% 
  filter(outcome_cohort_name == "Colorectal") %>% 
  filter(analysis_interval != "overall") %>% 
  filter(denominator_sex == "Both") %>% 
  filter(database_name == "CPRD GOLD") %>% 
  filter(denominator_age_group != "All") %>% 
  filter(incidence_start_date == "2006-01-01") %>% 
  rename(Agegroup = denominator_age_group)
  

# merge the two files

esp_CRC <- incidence_estimates_CRC %>% 
  left_join(ESP13_updated, by = "Agegroup") %>% 
  select(n_persons,
         n_events,
         incidence_start_date,
         incidence_100000_pys,
         incidence_100000_pys_95CI_lower,
         incidence_100000_pys_95CI_upper,
         ESP2013) %>% 
  mutate(espbyir = incidence_100000_pys* ESP2013)

ESP2013_sum <- sum(esp_CRC$ESP2013)
irage_summ <- sum(esp_CRC$espbyir)
  
#age standardize = IR sum/esr2013 sum
age_adjust_result <- irage_summ/ESP2013_sum


############### now with packages ###
pacman::p_load(
  rio,                 # import/export data
  here,                # locate files
  tidyverse,           # data management and visualization
  stringr,             # cleaning characters and strings
  frailtypack,         # needed for dsr, for frailty models
  dsr,                 # standardise rates
  PHEindicatormethods) # alternative for rate standardisation

packageurl <- "https://cran.r-project.org/src/contrib/Archive/dsr/dsr_0.2.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

library(dsr)

#rename ESP column to pop
ESP13_updated <- ESP13_updated %>% 
  rename(pop = ESP2013)

#merge data together
esp_CRC1 <- incidence_estimates_CRC %>% 
  left_join(ESP13_updated, by = "Agegroup")


mortality_rate <- dsr::dsr(
  data = esp_CRC1,  # specify object containing number of deaths per stratum
  event = n_events,       # column containing number of deaths per stratum 
  fu = person_years , # column containing number of population per stratum person years
  subgroup = outcome_cohort_name,             # other columns - rates will be standardized by these
  refdata = ESP13_updated, # reference population data frame, with column called pop
  method = "gamma",      # method to calculate 95% CI
  sig = 0.95,            # significance level
  mp = 100000,           # we want rates per 100.000 population
  decimals = 2) 


 # using dsr or doing it manually gives the same answer but dsr calculates CIs


# now to see if it works for all years

incidence_estimates_CRC1 <- incidence_estimates %>% 
  filter(outcome_cohort_name == "Colorectal") %>% 
  filter(analysis_interval != "overall") %>% 
  filter(denominator_sex == "Both") %>% 
  filter(database_name == "CPRD GOLD") %>% 
  filter(denominator_age_group != "All") %>% 
#  filter(incidence_start_date == "2006-01-01") %>% 
  rename(Agegroup = denominator_age_group)

incidence_estimates_CRC1$incidence_start_date <- as.character(incidence_estimates_CRC1$incidence_start_date)


# merge the two files
esp_CRC <- incidence_estimates_CRC1 %>% 
  left_join(ESP13_updated, by = "Agegroup") %>% 
  filter(!is.na(n_events))
  
mortality_rate1 <- dsr::dsr(
  data = esp_CRC,  # specify object containing number of deaths per stratum
  event = n_events,       # column containing number of deaths per stratum 
  fu = person_years , # column containing number of population per stratum person years
  subgroup = incidence_start_date,   
  #outcome_cohort_name, # other columns - rates will be standardized by these
  refdata = ESP13_updated, # reference population data frame, with column called pop
  method = "gamma",      # method to calculate 95% CI
  sig = 0.95,            # significance level
  mp = 100000,           # we want rates per 100.000 population
  decimals = 2) 








