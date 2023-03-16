#### SERVER ------
server <-	function(input, output, session) {
  # prevalence
  get_prevalence_estimates<-reactive({
    
    table<-prevalence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)  %>% 
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  

    table
  }) 
  
  output$tbl_prevalence_estimates<-  renderDataTable({

    table<-get_prevalence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>% 
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>% 
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>% 
      mutate(prevalence= ifelse(!is.na(prevalence),
               paste0(prevalence, " (",
                                prevalence_95CI_lower," to ", 
                                prevalence_95CI_upper, ")"))) %>% 
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id","prevalence",
                "denominator_strata_cohort_definition_id",
                "denominator_strata_cohort_name")) %>%
      relocate(result_id, .after = prevalence_end_date) %>%
    relocate(`Prevalence (95% CI)`, .before = outcome_cohort_name)

    
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "prevalence_estimates"))
              ))
  } )
  
  output$plot_prevalence_estimates<- renderPlotly({ 
    
    table<-get_prevalence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))

    if(is.null(input$prevalence_plot_group)){
      if(!is.null(input$prevalence_plot_facet)){
      p<-table %>% 
        unite("facet_var", 
              c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                          ymin = "prevalence_95CI_lower",
                          ymax = "prevalence_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0) +
        facet_wrap(vars(facet_var),ncol = 2)+
        scale_y_continuous(
          labels = scales::percent,
          limits = c(0, NA)
        ) +
        theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$prevalence_plot_group) ){ 
      
      if(is.null(input$prevalence_plot_facet) ){ 
      p<-table %>% 
        unite("Group", 
              c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                          ymin = "prevalence_95CI_lower",
                          ymax = "prevalence_95CI_upper",
                          group="Group",
                          colour="Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0, position=position_dodge(width=1)) +
        theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet) ){
        if(!is.null(input$prevalence_plot_group) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
          unite("facet_var", 
                c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          facet_wrap(vars(facet_var),ncol = 2)+  
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          )  +
          theme_bw()
        }
      }
      
    }
    
    p
    
    })
  
  get_prevalence_attrition<-reactive({
    
    table<-prevalence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  
    
    table
  }) 
  
  output$tbl_prevalence_attrition<-  renderDataTable({
    
    table<-get_prevalence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_full_contribution","analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "prevalence_attrition"))
              ))
  })
 
  # incidence
  get_incidence_estimates<-reactive({
    
    table<-incidence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                #analysis_interval,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector)  %>%
      filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector) 

    table
  }) 
  output$tbl_incidence_estimates<-  renderDataTable({

    table<-get_incidence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
               paste0(incidence_100000_pys, " (",
                                incidence_100000_pys_95CI_lower," to ", 
                                incidence_100000_pys_95CI_upper, ")"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id",
                "denominator_strata_cohort_definition_id",
                "denominator_strata_cohort_name")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "incidence_estimates"))
              ))
  } )
  output$plot_incidence_estimates<- renderPlotly({ 
    
    table<-get_incidence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))

    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
      p<-table %>% 
        unite("facet_var", 
              c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0) +
        facet_wrap(vars(facet_var),ncol = 2)+
        scale_y_continuous(
          limits = c(0, NA)
        ) +
        theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
      p<-table %>% 
        unite("Group", 
              c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group="Group",
                          colour="Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0, position=position_dodge(width=1)) +
        theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          facet_wrap(vars(facet_var),ncol = 2)+  
          scale_y_continuous(
            limits = c(0, NA)
          )  +
          theme_bw()
        }
      }
      
    }
    
    p
    
    })
  get_incidence_attrition<-reactive({
    
    table<-incidence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                #analysis_interval,
                analysis_repeated_events,analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector) %>%
    filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector) 
    table
  }) 
  output$tbl_incidence_attrition<-  renderDataTable({
    
    table<-get_incidence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id")) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "attrition_attrition"))
              ))
  })  
  
  # Survival
  # survival estimates: whole population
  get_survival_estimates<-reactive({
    
    table<-survival_estimates_whole %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      filter(CalenderYearGp %in% input$calender_year_selector) 
    
    table
  }) 
  output$tbl_survival_estimates<-  renderDataTable({

    table<-get_survival_estimates()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      select(!c("Stratification", "std.error")) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_estimates"))
              ))
  } )

  # output$plot_incidence_estimates<- renderPlotly({
  # 
  #   table<-get_incidence_estimates()
  #   validate(need(ncol(table)>1,
  #                 "No results for selected inputs"))
  # 
  #   if(is.null(input$incidence_plot_group)){
  #     if(!is.null(input$incidence_plot_facet)){
  #       p<-table %>%
  #         unite("facet_var",
  #               c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
  #         ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
  #                           ymin = "incidence_100000_pys_95CI_lower",
  #                           ymax = "incidence_100000_pys_95CI_upper")) +
  #         geom_point(position=position_dodge(width=1))+
  #         geom_errorbar(width=0) +
  #         facet_wrap(vars(facet_var),ncol = 2)+
  #         scale_y_continuous(
  #           limits = c(0, NA)
  #         ) +
  #         theme_bw()
  #     } else{
  #       p<-table %>%
  #         ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
  #                           ymin = "incidence_100000_pys_95CI_lower",
  #                           ymax = "incidence_100000_pys_95CI_upper")) +
  #         geom_point(position=position_dodge(width=1))+
  #         geom_errorbar(width=0) +
  #         scale_y_continuous(
  #           limits = c(0, NA)
  #         ) +
  #         theme_bw()
  #     }
  #   }
  # 
  # 
  #   if(!is.null(input$incidence_plot_group) ){
  # 
  #     if(is.null(input$incidence_plot_facet) ){
  #       p<-table %>%
  #         unite("Group",
  #               c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
  #         ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
  #                           ymin = "incidence_100000_pys_95CI_lower",
  #                           ymax = "incidence_100000_pys_95CI_upper",
  #                           group="Group",
  #                           colour="Group")) +
  #         geom_point(position=position_dodge(width=1))+
  #         geom_errorbar(width=0, position=position_dodge(width=1)) +
  #         theme_bw()
  #     }
  # 
  #     if(!is.null(input$incidence_plot_facet) ){
  #       if(!is.null(input$incidence_plot_group) ){
  #         p<-table %>%
  #           unite("Group",
  #                 c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
  #           unite("facet_var",
  #                 c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
  #           ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
  #                             ymin = "incidence_100000_pys_95CI_lower",
  #                             ymax = "incidence_100000_pys_95CI_upper",
  #                             group="Group",
  #                             colour="Group")) +
  #           geom_point(position=position_dodge(width=1))+
  #           geom_errorbar(width=0, position=position_dodge(width=1)) +
  #           facet_wrap(vars(facet_var),ncol = 2)+
  #           scale_y_continuous(
  #             limits = c(0, NA)
  #           )  +
  #           theme_bw()
  #       }
  #     }
  # 
  #   }
  # 
  #   p
  # 
  # })
  
  
# survival estimates: calender year population
  get_survival_estimates_cy<-reactive({
    
    table<-survival_estimates_calender %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector_cy)  %>% 
      filter(Age %in% input$survival_age_group_selector_cy)     %>% 
      filter(Gender %in% input$survival_sex_selector_cy)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector_cy) 
    
    table
  }) 
  output$tbl_survival_estimates_cy<-  renderDataTable({
    
    table<-get_survival_estimates_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      select(!c("Stratification", "std.error")) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_estimates_calenderYears"))
              ))
  } )

# risk table whole population 
  get_survival_risktable<-reactive({
    
    table<-survival_risk_table %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector) 
    
    table
  }) 
  output$tbl_survival_risk_table<-  renderDataTable({
    
    table<-get_survival_risktable()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification"))

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_risk_table"))
              ))
  } )
  
# risk table calender years  
  get_survival_risktable_cy<-reactive({
    
    table<- survival_risk_table_cy %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector_cy)  %>% 
      filter(Age %in% input$survival_age_group_selector_cy)     %>% 
      filter(Gender %in% input$survival_sex_selector_cy)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector_cy) 
    
    table
  }) 
  output$tbl_survival_risk_table_cy <-  renderDataTable({
    
    table<-get_survival_risktable_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_risk_table_by_calenderyr"))
              ))
  } )

# median survival calender years
  get_survival_median_table_cy<-reactive({

    table<- survival_median_table_cy %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector_cy)  %>%
      filter(Age %in% input$survival_age_group_selector_cy)     %>%
      filter(Gender %in% input$survival_sex_selector_cy)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector_cy)

    table
  })
  output$tbl_survival_median_table_cy <-  renderDataTable({

    table<-get_survival_median_table_cy()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      select(!c("Stratification"))

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_median_by_calenderyr"))
              ))
  } )

# median survival whole population
  get_survival_median_table<-reactive({
    
    table<-survival_median_table %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      filter(CalenderYearGp %in% input$calender_year_selector) 
    
    table
  }) 
  output$tbl_survival_median_table<-  renderDataTable({
    
    table<-get_survival_median_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification")) %>%
      relocate(Cancer) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_median_survival"))
              ))
  } )

# survival risk rates calender years
  get_survival_rates_table_cy<-reactive({
    
    table<- survival_rates_table_cy %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector_cy)  %>%
      filter(Age %in% input$survival_age_group_selector_cy)     %>%
      filter(Gender %in% input$survival_sex_selector_cy)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector_cy)
    
    table
  })
  output$tbl_survival_rates_table_cy <-  renderDataTable({
    
    table<-get_survival_rates_table_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_median_by_calenderyr"))
              ))
  } )
  
# survival risk rates whole population
  get_survival_rates_table<-reactive({
    
    table<- survival_rates_table %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector)  %>%
      filter(Age %in% input$survival_age_group_selector)     %>%
      filter(Gender %in% input$survival_sex_selector)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector)  %>%
      filter(CalenderYearGp %in% input$calender_year_selector)
    
    table
  })
  output$tbl_survival_rates_table <-  renderDataTable({
    
    table<-get_survival_rates_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'l<"sep">Bfrtip',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_median_whole_pop"))
              ))
  } )

   
}