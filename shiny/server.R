#### SERVER ------
server <-	function(input, output, session) {

  
  # cdm snapshot------
  output$tbl_cdm_snaphot <- renderText(kable(snapshotcdm) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(snapshotcdm)
      gtsave(x, file)
    }
  )
  
  
  # incidence attrition -----
  get_table_attrition <-reactive({
    
    table <- incidence_attrition %>% 
      filter(denominator_sex %in% input$attrition_sex_selector) %>% 
      filter(denominator_age_group %in% input$attrition_age_selector) %>% 
      filter(outcome_cohort_name %in% input$attrition_cohort_name_selector) %>% 
      filter(analysis_interval %in% input$attrition_time_selector)
    
    table
  }) 
  
  output$tbl_incidence_attrition <- renderText(kable(get_table_attrition()) %>%
                                             kable_styling("striped", full_width = F) )
  
  output$dt_incidence_attrition_word <- downloadHandler(
    filename = function() {
      "incidence_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_table_attrition())
      gtsave(x, file)
    }
  )
  
  
  # patient_characteristics ----
  get_patient_characteristics <- reactive({
    
    validate(
      need(input$demographics_cohort_selector != "", "Please select a cohort")
    )
    
    validate(
      need(input$demographics_selector != "", "Please select a demographic")
    )
    
    patient_characteristics <- patient_characteristics %>% 
      filter(strata_level %in% input$demographics_selector) %>% 
      filter(group_level %in% input$demographics_cohort_selector) %>% 
      filter(group_level %in%  
               stringr::str_replace_all(
                 stringr::str_to_sentence(input$demographics_cohort_selector),
                 "_", " ")
      )
     
    
    patient_characteristics
  })
  
  
  output$gt_patient_characteristics  <- render_gt({
    PatientProfiles::gtCharacteristics(get_patient_characteristics())
  })  
  
  
  output$gt_patient_characteristics_word <- downloadHandler(
    filename = function() {
      "patient_characteristics.docx"
    },
    content = function(file) {

      gtsave(PatientProfiles::gtCharacteristics(get_patient_characteristics()), file)
    }
  )
  
  

  # surv risk table --------
  get_risk_table <- reactive({
    
    
    validate(
      need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selector != "", "Please select a database")
    )
    validate(
      need(input$risk_table_sex_selector != "", "Please select a sex")
    )
    
 
    table <- survival_risk_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) %>% 
      filter(Sex %in% input$risk_table_sex_selector)
    
    table
    
  })
  
  
  output$dt_risk_table <- renderText(kable(get_risk_table()) %>%
                                       kable_styling("striped", full_width = F) )
  
  
  output$gt_risk_table_word <- downloadHandler(
    filename = function() {
      "risk_table.docx"
    },
    content = function(file) {
      x <- gt(get_risk_table())
      gtsave(x, file)
    }
  )  
  
  
  # surv stats --------
  get_surv_stats_table <- reactive({


    validate(
      need(input$median_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$median_database_name_selector != "", "Please select a database")
    )
    validate(
      need(input$median_sex_selector != "", "Please select a sex")
    )


    table <- survival_median_table %>%
      filter(Cancer %in% input$median_cohort_name_selector) %>%
      filter(Database %in% input$median_database_name_selector) %>%
      filter(Sex %in% input$median_sex_selector)

    table

  })


  output$dt_median_table <- renderText(kable(get_surv_stats_table()) %>%
                                       kable_styling("striped", full_width = F) )


  output$gt_median_table_word <- downloadHandler(
    filename = function() {
      "survival_statistics.docx"
    },
    content = function(file) {
      x <- gt(get_surv_stats_table())
      gtsave(x, file)
    }
  )
  
  # survival plots -------
  get_surv_plot <- reactive({
    
    validate(
      need(input$survival_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selector != "", "Please select a database")
    )
    validate(
      need(input$survival_sex_selector != "", "Please select a sex")
    )
    
    validate(
      need(input$surv_plot_group != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$surv_plot_facet != "", "Please select a group to facet by")
    )

    
    plot_data <- survival_estimates_whole %>%
      filter(Database %in% input$survival_database_selector) %>%
      filter(Cancer %in% input$survival_cohort_name_selector) %>%
      filter(Sex %in% input$survival_sex_selector) 
    
    if (input$show_ci) {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw() 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw() 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw() 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw() 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + scale_y_continuous(labels = scales::percent,
                                        limits = c(0, NA))
      
      plot 
      
    } else {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw() 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw() 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw() 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw() 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + scale_y_continuous(labels = scales::percent,
                                        limits = c(0, NA)
      
      )
      
      plot      
      
      
    }
    
    
    
    
    
  })
  
  output$survivalPlot <- renderPlot(
    get_surv_plot()
  )
 
  
  
  # surv risk table CY --------
  get_risk_tablecy <- reactive({
    
    
    validate(
      need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selector != "", "Please select a database")
    )
    validate(
      need(input$risk_table_sex_selector != "", "Please select a sex")
    )
    
    
    table <- survival_risk_cy_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) %>% 
      filter(Sex %in% input$risk_table_sex_selector)
    
    table
    
  })
  
  
  output$dt_risk_tablecy <- renderText(kable(get_risk_tablecy()) %>%
                                       kable_styling("striped", full_width = F) )
  
  
  output$gt_risk_tablecy_word <- downloadHandler(
    filename = function() {
      "risk_table_calendar_year.docx"
    },
    content = function(file) {
      x <- gt(get_risk_tablecy())
      gtsave(x, file)
    }
  )  
  
  
  # surv stats CY --------
  get_surv_stats_tablecy <- reactive({
    
    
    validate(
      need(input$median_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$median_database_name_selector != "", "Please select a database")
    )
    validate(
      need(input$median_sex_selector != "", "Please select a sex")
    )
    
    
    table <- survival_median_table_cy %>%
      filter(Cancer %in% input$median_cohort_name_selector) %>%
      filter(Database %in% input$median_database_name_selector) %>%
      filter(Sex %in% input$median_sex_selector)
    
    table
    
  })
  
  
  output$dt_median_tablecy <- renderText(kable(get_surv_stats_tablecy()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_median_tablecy_word <- downloadHandler(
    filename = function() {
      "survival_statistics.docx"
    },
    content = function(file) {
      x <- gt(get_surv_stats_tablecy())
      gtsave(x, file)
    }
  )
  
  # survival plots CY -------
  get_surv_plot_cy <- reactive({

    validate(
      need(input$survival_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selector != "", "Please select a database")
    )
    validate(
      need(input$survival_sex_selector != "", "Please select a sex")
    )

    validate(
      need(input$surv_plot_facet != "", "Please select a group to facet by")
    )


    plot_data <- survival_estimates_cy %>%
      filter(Database %in% input$survival_database_selector) %>%
      filter(Cancer %in% input$survival_cohort_name_selector) %>%
      filter(Sex %in% input$survival_sex_selector)

    if (input$show_ci) {

      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw()



      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw()

      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw()

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw()

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot + scale_y_continuous(labels = scales::percent,
                                        limits = c(0, NA))

      plot

    } else {

      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw()



      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw()

      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 2) +
          theme_bw()

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw()

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot + scale_y_continuous(labels = scales::percent,
                                        limits = c(0, NA)

      )

      plot


    }





  })

  output$survivalPlotcy <- renderPlot(
    get_surv_plot_cy()
  )

  
  
  
  
  
  # output$plot_survival_estimates_cy<- renderPlotly({
  #   
  #   table<-get_survival_estimates_cy()
  #   validate(need(ncol(table)>1,
  #                 "No results for selected inputs"))
  #   
  #   if(is.null(input$survival_plot_group_cy)){
  #     if(!is.null(input$survival_plot_facet_cy)){
  #       p<-table %>%
  #         unite("facet_var",
  #               c(all_of(input$survival_plot_facet_cy)), remove = FALSE, sep = "; ") %>%
  #         ggplot(aes_string(x=input$time, y="est", colour="CalendarYearGp")) +
  #         geom_line() +
  #         facet_wrap(vars(facet_var),ncol = 2)+
  #         scale_y_continuous(limits = c(NA, 1) ) +
  #         theme_bw()
  #     } else{
  #       p<-table %>%
  #         ggplot(aes_string(x=input$time, y="est", colour="CalendarYearGp")) +
  #         scale_y_continuous(
  #           limits = c(NA, 1)
  #         ) +
  #         theme_bw()
  #     }
  #   }
  #   
  #   
  #   if(!is.null(input$survival_plot_group_cy) ){
  #     
  #     if(is.null(input$survival_plot_facet_cy) ){
  #       p<-table %>%
  #         unite("Group",
  #               c(all_of(input$survival_plot_group_cy)), remove = FALSE, sep = "; ") %>%
  #         ggplot(aes_string(x=input$time, y="est",
  #                           group="Group",
  #                           colour="Group")) +
  #         geom_line() +
  #         theme_bw()
  #     }
  #     
  #     if(!is.null(input$survival_plot_facet_cy) ){
  #       if(!is.null(input$survival_plot_group_cy) ){
  #         p<-table %>%
  #           unite("Group",
  #                 c(all_of(input$survival_plot_group_cy)), remove = FALSE, sep = "; ") %>%
  #           unite("facet_var",
  #                 c(all_of(input$survival_plot_facet_cy)), remove = FALSE, sep = "; ") %>%
  #           ggplot(aes_string(x=input$time, y="est",
  #                             group="Group",
  #                             colour="Group")) +
  #           geom_line() +
  #           facet_wrap(vars(facet_var_cy),ncol = 2)+
  #           scale_y_continuous(limits = c(NA, 1) )  +
  #           theme_bw()
  #       }
  #     }
  #     
  #   }
  #   
  #   p
  #   
  # })    
  
  
  
   
  
   
}