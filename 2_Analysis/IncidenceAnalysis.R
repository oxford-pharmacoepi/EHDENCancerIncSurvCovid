# code for getting denominator and estimating incidence below

# cancer populations  -----
print(paste0("- Getting denominator: cancer populations"))
info(logger, "- Getting denominator: cancer populations")

#get denomminator
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
print(paste0("- Getting incidence and period prevalence: cancer populations"))
info(logger, "- Getting incidence and period prevalence: cancer populations")

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  interval = "years",
  outcomeWashout = NULL,
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE,
  minCellCount = 5
)

prev_period <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name,
  interval = "years",
  completeDatabaseIntervals = TRUE, 
  fullContribution = FALSE,
  minCellCount = 5
)


print(paste0("- Got incidence and period prevalence: cancer population"))
info(logger, "- Got incidence and period prevalence: cancer population")


# Get the results ----------------
print(paste0("- Gathering incidence and period prevalence results: cancer populations"))
info(logger, "- Gathering incidence and period prevalence results: cancer populations")


study_results<- gatherIncidencePrevalenceResults(list(inc, prev_period),
                             outcomeCohortId = outcome_cohorts$cohortId,
                             outcomeCohortName = outcome_cohorts$cohortName,
                             databaseName = db.name)

print(paste0("- Got incidence and period prevalence results: cancer populations"))
info(logger, "- Got incidence and period prevalence results: cancer populations")

# Export the results -----
print(paste0("- Exporting incidence and period prevalence results: cancer populations"))
info(logger, "- Exporting incidence and period prevalence results: cancer populations")

exportIncidencePrevalenceResults(result=study_results,
                                 zipName= paste0(db.name, "IPResults"),
                                 outputFolder=here::here("Results", db.name))


print(paste0("- Exported incidence and period prevalence results: cancer populations"))
info(logger, "- Exported incidence and period prevalence results: cancer populations")

print(paste0("- Plotting incidence and period prevalence results: cancer populations"))
info(logger, "- Plotting incidence and period prevalence results: cancer populations")



###########################################
# plot the results for whole population

# incidence
inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &
           denominator_age_group == "18;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
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



# period prevalence
pp_yrs_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 3 &
           denominator_age_group == "18;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

plotAll <- pp_yrs_plot %>%
  ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Period Prevalence") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotname <- paste0("PeriodPrevRatesWholePop", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAll, newpage = FALSE)
dev.off()


# ###########################################
# # plot the results stratified by gender

inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
    filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 &
           denominator_age_group == "18;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotGender <- plotGender + facet_wrap(~denominator_sex, scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()

# period prevalence
pp_yrs_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_cohort_id == 1 | denominator_cohort_id == 2 &
           denominator_age_group == "18;150") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

plotGender <- pp_yrs_plot %>%
  ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Period Prevalence") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotGender <- plotGender + facet_wrap(~denominator_sex , scales="free_y") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))


plotname <- paste0("PeriodPrevRatesGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 10, height = 5)
print(plotGender, newpage = FALSE)
dev.off()


# ###########################################
# # plot the results stratified by age

# incidence
inc_yrs_plot <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "18;150" &
           denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()


agelabels <- c(
  `18;29` = "18-29 Years",
  `30;39` = "30-39 Years",
  `40;49` = "40-49 Years",
  `50;59` = "50-59 Years",
  `60;69` = "60-69 Years",
  `70;79` = "70-79 Years",
  `80;89` = "80-89 Years",
  `90;150` = "90+ Years")

plotAge <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("IncidenceRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAge, newpage = FALSE)
dev.off()



# period prevalence
pp_yrs_plot <- study_results$prevalence_estimates %>%  # need to amend this bit of code to select the estimates relating to inc_yrs
  filter(denominator_age_group != "18;150" &
           denominator_sex == "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

plotAge <- pp_yrs_plot %>%
  ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Period Prevalence") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("PeriodPrevRatesAge", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 7, height = 5)
print(plotAge, newpage = FALSE)
dev.off()


# ###########################################
# # plot the results stratified by age AND gender

# incidence
inc_yrs_plot <- study_results$incidence_estimates %>% 
  filter(denominator_age_group != "18;150" &
           denominator_sex != "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(incidence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(
  `18;29` = "18-29 Years",
  `30;39` = "30-39 Years",
  `40;49` = "40-49 Years",
  `50;59` = "50-59 Years",
  `60;69` = "60-69 Years",
  `70;79` = "70-79 Years",
  `80;89` = "80-89 Years",
  `90;150` = "90+ Years")

plotAgeGender <- inc_yrs_plot %>%
  ggplot( aes(x = time, y = incidence_100000_pys, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Incidence Rate (per 100000 py)") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
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

# period prevalence
pp_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_age_group != "18;150" &
           denominator_sex != "Both") %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantProstateCancer", "Prostate")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLungCancer", "Lung")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantBreastCancer", "Breast")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantColorectalCancer", "Colorectal")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantHeadNeckCancer", "Head and Neck")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantLiverCancer", "Liver")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantPancreaticCancer", "Pancreas")) %>%
  mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "MalignantStomachCancer", "Stomach")) %>%
  mutate(time = format(prevalence_start_date, format="%Y")) %>%
  as.data.frame()

agelabels <- c(
  `18;29` = "18-29 Years",
  `30;39` = "30-39 Years",
  `40;49` = "40-49 Years",
  `50;59` = "50-59 Years",
  `60;69` = "60-69 Years",
  `70;79` = "70-79 Years",
  `80;89` = "80-89 Years",
  `90;150` = "90+ Years")

plotAgeGender <- pp_yrs_plot %>%
  ggplot( aes(x = time, y = prevalence, group=outcome_cohort_name, color = outcome_cohort_name)) +
  geom_ribbon(aes(ymin = prevalence_95CI_lower, ymax = prevalence_95CI_upper, fill = outcome_cohort_name), alpha = .3, color = NA, show.legend = FALSE) +
  geom_line(color = "black", size = 0.25) +
  geom_point(size = 2.5) +
  xlab("Year") +
  ylab("Period Prevalence") +
  scale_colour_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark read, gry
  scale_fill_manual(values = c("#00468BFF", "#ED0000FF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "grey")) +
  labs(colour = "Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

plotAgeGender <- plotAgeGender + facet_grid(denominator_sex ~ denominator_age_group , labeller=labeller(denominator_age_group = agelabels), scales = "free") +
  theme(strip.background = element_rect(colour="black", fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))

plotname <- paste0("PeriodPrevRatesAgeGender", db.name,".pdf")

pdf(here("Results",db.name, plotname),
    width = 15, height = 7)
print(plotAgeGender, newpage = FALSE)
dev.off()

print(paste0("- Plotted incidence and period prevalence results: cancer populations"))
info(logger, "- Plotted incidence and period prevalence results: cancer populations")


