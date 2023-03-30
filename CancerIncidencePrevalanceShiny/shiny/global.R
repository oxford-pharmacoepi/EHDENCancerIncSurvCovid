library(here)
library(dplyr)
library(tidyr)
library(stringr)

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
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}



#### Load data -----
#data
prevalence_estimates <- readRDS(here("data","prevalence_estimates.rds"))
prevalence_attrition <- readRDS(here("data","prevalence_attrition.rds"))
incidence_estimates <- readRDS(here("data","incidence_estimates.rds"))
incidence_attrition <- readRDS(here("data","incidence_attrition.rds"))
# whole pop
survival_estimates_whole <- readRDS(here("data","survival_estimates.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019") %>%
  droplevels()
survival_risk_table <- readRDS(here("data","survival_risk_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp)
survival_median_table <- readRDS(here("data","survival_median_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019") %>%
  droplevels()
survival_rates_table <- readRDS(here("data","survival_rates_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019") %>%
  droplevels()

#calendar pop
survival_estimates_calendar <- readRDS(here("data","survival_estimates.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  droplevels()
survival_risk_table_cy <- readRDS(here("data","survival_risk_table_cy.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp)
survival_median_table_cy <- readRDS(here("data","survival_median_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  droplevels()
survival_rates_table_cy <- readRDS(here("data","survival_rates_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>%
  droplevels() %>%
  filter(time != 10) %>%
  mutate(time = as.character(time))

table_one_results <- readRDS(here("data","table1_results.rds")) 




