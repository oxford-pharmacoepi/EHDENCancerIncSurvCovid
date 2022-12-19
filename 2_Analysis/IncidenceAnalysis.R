# code for getting denominator and estimating incidence below

# cancer populations  -----
print(paste0("- Getting denominator: cancer populations"))
info(logger, "- Getting denominator: cancer populations")

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2000-01-01"),
  endDate = as.Date("2019-01-01"),
  ageGroup =list(
    c(18, 150),
    c(18, 29),
    c(30, 39),
    c(40, 49),
    c(50, 59),
    c(60, 69),
    c(70, 79),
    c(80, 89),
    c(90, 150)
  ),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 365,
  verbose = TRUE
)

#cdm$denominator %>% tally() # to check numbers in denominator population
#attrition(cdm$denominator) # to grab the attrition

print(paste0("- Got denominator: cancer populations"))
info(logger, "- Got denominator: cancer populations")


# Estimate incidence -------
print(paste0("- Getting incidence: cancer populations"))
info(logger, "- Getting incidence: cancer populations")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE, # try this with and without
  minCellCount = 5
)


print(paste0("- Got incidence: cancer population"))
info(logger, "- Got incidence: cancer population")


# Get the results ----------------
print(paste0("- Gathering incidence results: cancer populations"))
info(logger, "- Gathering incidence results: cancer populations")


study_results<- gatherIncidencePrevalenceResults(list(inc),
                             outcomeCohortId = outcome_cohorts$cohortId,
                             outcomeCohortName = outcome_cohorts$cohortName,
                             databaseName = db.name)

print(paste0("- Got incidence results: cancer populations"))
info(logger, "- Got incidence results: cancer populations")

# Export the results -----
print(paste0("- Exporting incidence results: cancer populations"))
info(logger, "- Exporting incidence results: cancer populations")

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults"),
                                 outputFolder=here::here("Results", db.name))


print(paste0("- Exported incidence results: cancer populations"))
info(logger, "- Exported incidence results: cancer populations")

print(paste0("- Plotting incidence results: cancer populations"))
info(logger, "- Plotting incidence results: cancer populations")



###########################################
# plot the results for whole population
inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &&
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot <- as.data.frame(inc_yrs_plot)

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) 

plotname <- paste0("IncidenceRatesWholePop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()

###########################################
# plot the results stratified by gender

inc_yrs_plot1 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 &&
           denominator_age_group == "40;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot1 <- as.data.frame(inc_yrs_plot1)

plotGender <- inc_yrs_plot1 %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  #ggtitle("Incidence rates of anti dementia medications \nin Patients with a diagnosis of dementia") +
  labs(colour = "Cancer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) 

plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()



###########################################
# plot the results stratified by age

inc_yrs_plot2 <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 12 | denominator_cohort_id == 6 | denominator_cohort_id == 9 &&
           denominator_sex == "Both"
         
         ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot2 <- as.data.frame(inc_yrs_plot2)


agelabels <- c(`40;64` = "40-64 Years", 
            `65;79` = "65-79 Years",
            `80;150` = "80+ Years")

plotAge <- inc_yrs_plot2 %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Cancer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAge <- plotAge + facet_wrap(~denominator_age_group, labeller=labeller(denominator_age_group = agelabels), scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 5)
print(plotAge, newpage = FALSE)
dev.off()


###########################################
# plot the results stratified by age AND gender

inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "40;150" &&
           denominator_sex != "Both"
         
  ) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "memantine", "Memantine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "donepezil", "Donepezil")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "rivastigmine", "Rivastigmine")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "galantamine", "Galantamine")) 

inc_yrs_plot <- as.data.frame(inc_yrs_plot)


agelabels <- c(`40;64` = "40-64 Years", 
               `65;79` = "65-79 Years",
               `80;150` = "80+ Years")

plotAgeGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = ir_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = ir_100000_pys_95CI_lower, ymax = ir_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) + 
  xlab("Year") + 
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) + #blue, #red, #lightblue, #green
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF")) +
  labs(colour = "Dementia Medications") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_blank() ,
        #axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) 

plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesAgeGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 7)
print(plotAgeGender, newpage = FALSE)
dev.off()


print(paste0("- Plotted incidence results: cancer populations"))
info(logger, "- Plotted incidence results: cancer populations")
