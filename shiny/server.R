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
      filter(group_level %in% input$demographics_cohort_selector) 
     
    
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
    
 
    table <- survival_risk_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) 
    
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


    table <- survival_median_table %>%
      filter(Cancer %in% input$median_cohort_name_selector) %>%
      filter(Database %in% input$median_database_name_selector)

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
  
  
  
  get_inc_estimates_table <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector != "", "Please select a cohort")
    )
    validate(
      need(input$inc_est_analysis_selector != "", "Please select analysis interval")
    )
    
    
    table <- incidence_estimates %>%
      filter(Cancer %in% input$inc_estimates_cohort_selector) %>%
      filter(analysis_interval %in% input$inc_est_analysis_selector) %>% 
      relocate(Cancer) %>% 
      select(-c(analysis_id,
                outcome_cohort_id,
                analysis_repeated_events,
                analysis_min_cell_count,
                denominator_target_cohort_name,
                denominator_cohort_name,
                denominator_age_group,
                denominator_sex,
                denominator_days_prior_observation,   
                denominator_start_date,
                denominator_end_date,
                denominator_target_cohort_definition_id,
                analysis_complete_database_intervals,
                analysis_outcome_washout,
                denominator_cohort_id,
                analysis_interval,
                cohort_obscured,
                result_obscured
                
                ))
    
    table
    
  })
  
  
  output$dt_inc_est_table <- renderText(kable(get_inc_estimates_table()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$dt_inc_est_table_word <- downloadHandler(
    filename = function() {
      "incidence_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_inc_estimates_table())
      gtsave(x, file)
    }
  )
  
  get_inc_estimates_table_std <- reactive({
    
    
    validate(
      need(input$inc_estimates_cohort_selector_std != "", "Please select a cohort")
    )
    
    table <- agestandardizedinc_final %>%
      filter(Cancer %in% input$inc_estimates_cohort_selector_std) %>%
      relocate(Cancer) %>% 
      rename("incidence_start_date" = "Subgroup")
   
    
    table
    
  })
  
  
  output$dt_inc_est_table_std <- renderText(kable(get_inc_estimates_table_std()) %>%
                                          kable_styling("striped", full_width = F) )
  
  
  output$dt_inc_est_table_word_std <- downloadHandler(
    filename = function() {
      "std_incidence_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_inc_estimates_table_std())
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
      need(input$surv_plot_group != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$surv_plot_facet != "", "Please select a group to facet by")
    )

    
    plot_data <- survival_estimates_whole %>%
      filter(Database %in% input$survival_database_selector) %>%
      filter(Cancer %in% input$survival_cohort_name_selector) 
    
    if (input$show_ci) {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed")) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))
      
      plot 
      
    } else {
      
      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))

        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3, scales = "free_y") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"))
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          scale_y_continuous( labels = label_percent() ) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed")) 
        
      }
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))

      
      plot      
      
      
    }
    
    
    
    
    
  })
  
  output$survivalPlot <- renderPlot(
    get_surv_plot()
  )
  
  output$survival_download_plot <- downloadHandler(
    filename = function() {
      "Survival_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot(),
        width = as.numeric(input$survival_download_width),
        height = as.numeric(input$survival_download_height),
        dpi = as.numeric(input$survival_download_dpi),
        units = "cm"
      )
    }
  )
 
  
  
  # surv risk table CY --------
  get_risk_tablecy <- reactive({
    
    
    validate(
      need(input$risk_table_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selectorcy != "", "Please select a database")
    )

    
    table <- survival_risk_cy_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selectorcy) %>%
      filter(Database %in% input$risk_table_database_name_selectorcy) 
    
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
      need(input$median_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$median_database_name_selectorcy != "", "Please select a database")
    )
    
    
    table <- survival_median_table_cy %>%
      filter(Cancer %in% input$median_cohort_name_selectorcy) %>%
      filter(Database %in% input$median_database_name_selectorcy) 
    
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
      need(input$survival_cohort_name_selectorcy != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selectorcy != "", "Please select a database")
    )
    
    validate(
      need(input$surv_plot_facetcy != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$surv_plot_facetcy != "", "Please select a group to facet by")
    )


    plot_data <- survival_estimates_cy %>%
      filter(Database %in% input$survival_database_selectorcy) %>%
      filter(Cancer %in% input$survival_cohort_name_selectorcy) %>% 
      filter(time <= 2.5000000000000)

    if (input$show_ci_cy) {

      if (!is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
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
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)



      } else if (!is.null(input$surv_plot_groupcy) && is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
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
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)

      } else if (is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
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
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) 

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          #geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
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
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot  +
        theme(strip.text = element_text(size = 15, face = "bold"))

      plot

    } else {

      if (!is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)



      } else if (!is.null(input$surv_plot_groupcy) && is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_groupcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) 

      } else if (is.null(input$surv_plot_groupcy) && !is.null(input$surv_plot_facetcy)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facetcy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
          scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
          scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
          labs(x = "Time (Years)",
               y = "Survival Probability",
               col = "Calendar Year Group",
               linetype = "Calendar Year Group") +
          theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "darkgray", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          geom_line(aes(linetype = CalendarYearGp),size = 0.85) +
          xlim(0, 2.5) 

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")

      )

      plot


    }





  })

  output$survivalPlotcy <- renderPlot(
    get_surv_plot_cy()
  )

  output$survival_cy_download_plot <- downloadHandler(
    filename = function() {
      "Survival_calendar_time_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_surv_plot_cy(),
        width = as.numeric(input$survival_download_widthcy),
        height = as.numeric(input$survival_download_heightcy),
        dpi = as.numeric(input$survival_download_dpicy),
        units = "cm"
      )
    }
  )
  
  
  get_incidence_plot <- reactive({
    
    validate(
      need(input$incidence_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$incidence_database_selector != "", "Please select a database")
    )
    
    validate(
      need(input$incidence_plot_facet != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$incidence_plot_facet != "", "Please select a group to facet by")
    )
    
    
    plot_data <- incidence_estimates %>%
        # first deselect settings which did not vary for this study
        select(!c(analysis_id,
                  analysis_complete_database_intervals,
                  denominator_start_date,
                  denominator_days_prior_observation,
                  analysis_outcome_washout,
                  denominator_target_cohort_definition_id,
                  analysis_repeated_events,
                  analysis_min_cell_count,
                  denominator_target_cohort_name,
                  cohort_obscured,
                  result_obscured,
                  outcome_cohort_id,
                  denominator_cohort_name,
                  denominator_cohort_id,
                  denominator_end_date)) %>%
        filter(Database %in% input$incidence_database_selector)  %>%
        filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>%
        filter(Cancer %in% input$incidence_cohort_name_selector)  %>%
        filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector)
    
    
    if (input$show_error_bars) {
      
      if (!is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(colour = "black")+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.1, colour = "black") + 
          geom_line(color = "black", size = 0.25) +
          geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          #geom_point(aes(fill = Group, colour = Group), size = 2) +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                legend.position = 'none',
                text = element_text(size = 15))

        
      } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(colour = "black")+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.3, colour = "black") + 
          geom_line(color = "black", size = 0.25) +
          geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                legend.position = 'none',
                text = element_text(size = 15))
        
      } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(colour = "black")+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.3, colour = "black") + 
          geom_line(color = "black", size = 0.25) +
          geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          facet_wrap(vars(facet_var),ncol = 3, scales = "free_y")+
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                legend.position = 'none',
                text = element_text(size = 15)) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(colour = "black")+
          geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper), 
                      alpha = 0.3, colour = "black") + 
          geom_line(color = "black", size = 0.25) +
          geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
          labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
          scale_y_continuous(limits = c(0, NA)) +
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                strip.background = element_rect(color = "black", size = 0.6) ,
                panel.background = element_blank() ,
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                legend.position = 'none',
                text = element_text(size = 15)) 
        
      }
      
      
      # Move scale_y_continuous outside of ggplot
      plot <- plot + 
        theme(strip.text = element_text(size = 15, face = "bold")
              
        )
      
      plot
      


      } else {
        
        
        if (!is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x = input$incidence_x_axis, y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper")) +
            geom_point(position = position_dodge(width = 1)) +
            geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(width = 0, position = position_dodge(width = 1)) +
            scale_color_manual(values = "black") +  # Set the color of points and error bars
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  legend.position = 'none',
                  text = element_text(size = 15)) +
            facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
          
        } else if (!is.null(input$incidence_plot_group) && is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("Group", c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x = input$incidence_x_axis, y = "incidence_100000_pys",
                                ymin = "incidence_100000_pys_95CI_lower",
                                ymax = "incidence_100000_pys_95CI_upper")) +
              geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
            geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
              scale_y_continuous(limits = c(0, NA)) +
              geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                    strip.background = element_rect(color = "black", size = 0.6),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.6),
                    panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                    legend.position = 'none',
                    text = element_text(size = 15)) 
            
          
        } else if (is.null(input$incidence_plot_group) && !is.null(input$incidence_plot_facet)) {
          plot <- plot_data %>%
            unite("facet_var", c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x = input$incidence_x_axis, y = "incidence_100000_pys",
                                ymin = "incidence_100000_pys_95CI_lower",
                                ymax = "incidence_100000_pys_95CI_upper")) +
              geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
            geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
              scale_y_continuous(limits = c(0, NA)) +
              geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                    strip.background = element_rect(color = "black", size = 0.6),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.6),
                    panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                    legend.position = 'none',
                    text = element_text(size = 15)) +
              facet_wrap(vars(facet_var), ncol = 3, scales = "free_y")
            
          
        } else {
          plot <- plot_data %>%
            ggplot(aes_string(x = input$incidence_x_axis, y = "incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper")) +
            geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
            geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
            labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
            scale_y_continuous(limits = c(0, NA)) +
            geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
                  strip.background = element_rect(color = "black", size = 0.6),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black", size = 0.6),
                  panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                  legend.position = 'none',
                  text = element_text(size = 15))
          
          
        }
        
        
        # Move scale_y_continuous outside of ggplot
        plot <- plot + 
          theme(strip.text = element_text(size = 15, face = "bold")
                
          )
        
        plot
        
      
        
      }
      
    
  })
  
  output$incidencePlot <- renderPlot(
    get_incidence_plot()
  )
  
  output$incidence_download_plot <- downloadHandler(
    filename = function() {
      "Crude_incidence_estimates_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_incidence_plot(),
        width = as.numeric(input$incidence_download_width),
        height = as.numeric(input$incidence_download_height),
        dpi = as.numeric(input$incidence_download_dpi),
        units = "cm"
      )
    }
  )
  
  
  get_incidence_plot_std <- reactive({
    
    validate(
      need(input$incidence_cohort_name_selector_std != "", "Please select a cohort")
    )
    validate(
      need(input$incidence_database_selector_std != "", "Please select a database")
    )
    
    validate(
      need(input$incidence_plot_facet_std != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$incidence_plot_facet_std != "", "Please select a group to facet by")
    )
    
  
  plot_data <- agestandardizedinc_final %>%
    filter(Database %in% input$incidence_database_selector_std)  %>%
    filter(as.character(Subgroup) %in% input$incidence_start_date_selector_std)  %>%
    filter(Cancer %in% input$incidence_cohort_name_selector_std)
  
  
  if (input$show_error_bars_std) {
    
    if (!is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        unite("facet_var_std", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(colour = "black")+
        geom_ribbon(aes(ymin = LCL_Std, ymax = UCL_Std), 
                    alpha = 0.1, colour = "black") + 
        geom_line(color = "black", size = 0.25) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var_std),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15))
      
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(colour = "black")+
        geom_ribbon(aes(ymin = LCL_Std, ymax = UCL_Std), 
                    alpha = 0.1, colour = "black") + 
        geom_line(color = "black", size = 0.25) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15))
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var_std", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(colour = "black")+
        geom_ribbon(aes(ymin = LCL_Std, ymax = UCL_Std), 
                    alpha = 0.1, colour = "black") + 
        geom_line(color = "black", size = 0.25) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        facet_wrap(vars(facet_var_std),ncol = 3, scales = "free_y")+
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15))
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(colour = "black")+
        geom_ribbon(aes(ymin = LCL_Std, ymax = UCL_Std), 
                    alpha = 0.1, colour = "black") + 
        geom_line(color = "black", size = 0.25) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6) ,
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15))
      
    }
    
    
    # Move scale_y_continuous outside of ggplot
    plot <- plot + 
      theme(strip.text = element_text(size = 15, face = "bold")
            
      )
    
    plot
    
    
    
  } else {
    
    
    if (!is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        unite("facet_var_std", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(position = position_dodge(width = 1)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        geom_errorbar(width = 0, position = position_dodge(width = 1)) +
        scale_color_manual(values = "black") +  # Set the color of points and error bars
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15)) +
        facet_wrap(vars(facet_var_std), ncol = 3, scales = "free_y")
      
    } else if (!is.null(input$incidence_plot_group_std) && is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("Group", c(all_of(input$incidence_plot_group_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15)) 
      
      
    } else if (is.null(input$incidence_plot_group_std) && !is.null(input$incidence_plot_facet_std)) {
      plot <- plot_data %>%
        unite("facet_var_std", c(all_of(input$incidence_plot_facet_std)), remove = FALSE, sep = "; ") %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15)) +
        facet_wrap(vars(facet_var_std), ncol = 3, scales = "free_y")
      
      
    } else {
      plot <- plot_data %>%
        ggplot(aes_string(x=input$incidence_x_axis_std, y="Std_IR",
                          ymin = "LCL_Std",
                          ymax = "UCL_Std")) +
        geom_point(aes(colour = "black"), size = 2, position = position_dodge(width = 1)) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="solid", colour = "#ED0000FF", size = 1) +
        labs(x = "Calendar Year", y = "Incidence Rate per 100,000 person-years") +
        scale_y_continuous(limits = c(0, NA)) +
        geom_errorbar(aes(colour = "black"), width = 0, position = position_dodge(width = 1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
              strip.background = element_rect(color = "black", size = 0.6),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.position = 'none',
              text = element_text(size = 15))
      
      
    }
    
    
    # Move scale_y_continuous outside of ggplot
    plot <- plot + 
      theme(strip.text = element_text(size = 15, face = "bold")
            
      )
    
    plot
    
    
    
  }
  
  
})

output$incidencePlotstd <- renderPlot(
  get_incidence_plot_std()
)

output$incidence_download_plot_std <- downloadHandler(
  filename = function() {
    "Std_incidence_estimates_plot.png"
  },
  content = function(file) {
    ggsave(
      file,
      get_incidence_plot_std(),
      width = as.numeric(input$incidence_download_widthstd),
      height = as.numeric(input$incidence_download_heightstd),
      dpi = as.numeric(input$incidence_download_dpistd),
      units = "cm"
    )
  }
)
  
  
   
}