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
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15) 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Group, colour = Group), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
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
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15) 
        
        
        
      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15) 
        
      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15) 
        
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
      need(input$risk_table_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$risk_table_database_name_selector != "", "Please select a database")
    )

    
    table <- survival_risk_cy_table %>%
      filter(Cancer %in% input$risk_table_cohort_name_selector) %>%
      filter(Database %in% input$risk_table_database_name_selector) 
    
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
    
    
    table <- survival_median_table_cy %>%
      filter(Cancer %in% input$median_cohort_name_selector) %>%
      filter(Database %in% input$median_database_name_selector) 
    
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
  
  
  
  
  
  # survivalFigureData <- survivalData2 %>%
  #   filter(Cancer == "Colorectal" |
  #            Cancer == "Head & Neck" |
  #            Cancer == "Liver" |
  #            Cancer == "Lung" |
  #            Cancer == "Oesophagus" |
  #            Cancer == "Pancreas" |
  #            Cancer == "Breast" |
  #            Cancer == "Prostate" |
  #            Cancer == "Stomach" ) %>% 
  #   filter(time <= 2.1000000000000) %>% 
  #   ggplot(aes(x = time,
  #              y = est,
  #              group = CalendarYearGp,
  #              col = CalendarYearGp )) +
  #   scale_y_continuous( labels = label_percent() ) +
  #   scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + #blue, #red, #lightblue, #green, purple, peach, dark red, gry
  #   scale_fill_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) +
  #   geom_line(aes(linetype = CalendarYearGp),size = 0.5) +
  #   scale_linetype_manual(values = c("dotted","dashed", "dotdash", "twodash","solid", "longdash")) +
  #   geom_ribbon(aes(ymin = lcl, 
  #                   ymax = ucl, 
  #                   fill = CalendarYearGp), alpha = .15, color = NA, show.legend = FALSE) +
  #   labs(x = "Time (Years)",
  #        y = "Survival Probability",
  #        col = "Calendar Year Group",
  #        linetype = "Calendar Year Group") +
  #   theme(panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
  #         strip.background = element_rect(color = "black", size = 0.6) ,
  #         panel.background = element_blank() ,
  #         panel.spacing.x = unit(0.05, "cm") ,
  #         panel.spacing.y = unit(0.1, "cm") ,
  #         axis.text = element_text(size = 8) ,
  #         #axis.title.y = element_text(size = 15),
  #         #axis.title.x = element_text(size = 15),
  #         legend.text = element_text(size=8),
  #         legend.title = element_text(size=8),
  #         #strip.text.x = element_text(size = 15),
  #         axis.line = element_line(colour = "black", size = 0.6) ,
  #         panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
  #         legend.box.spacing = unit(0, "pt") ,
  #         legend.key = element_rect(fill = "transparent", colour = "transparent"),
  #         legend.position='bottom') +
  #   xlim(0, 2) +
  #   facet_wrap(~ Cancer, scales = "free_y", ncol = 3)
  
  # survival plots CY -------
  get_surv_plot_cy <- reactive({

    validate(
      need(input$survival_cohort_name_selector != "", "Please select a cohort")
    )
    validate(
      need(input$survival_database_selector != "", "Please select a database")
    )
    
    validate(
      need(input$surv_plot_facet != "", "Please select a group to colour by")
    )
    
    validate(
      need(input$surv_plot_facet != "", "Please select a group to facet by")
    )


    plot_data <- survival_estimates_cy %>%
      filter(Database %in% input$survival_database_selector) %>%
      filter(Cancer %in% input$survival_cohort_name_selector) %>% 
      filter(time <= 2.5000000000000)

    if (input$show_ci_cy) {

      if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          
          ggplot(aes(x = time,
                     y = est,
                     group = CalendarYearGp,
                     col = CalendarYearGp )) +
          scale_y_continuous( labels = label_percent() ) +
          scale_colour_manual(values = c("black", "black", "black", "black", "#ED0000FF", "#FDAF91FF", "#AD002AFF", "grey")) + 
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
                panel.spacing.x = unit(0.05, "cm") ,
                panel.spacing.y = unit(0.1, "cm") ,
                axis.text = element_text(size = 15) ,
                axis.title = element_text(size = 15) ,
                legend.text = element_text(size= 15),
                legend.title = element_text(size= 15),
                axis.line = element_line(colour = "black", size = 0.6) ,
                panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                legend.box.spacing = unit(0, "pt") ,
                legend.key = element_rect(fill = "transparent", colour = "transparent"),
                legend.position='bottom') +
          xlim(0, 2.5) +
          facet_wrap(~ Cancer, scales = "free_y", ncol = 3)



      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp, fill = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15)

      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp, fill = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15)

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = CalendarYearGp)) +
          geom_line() +
          geom_ribbon(aes(ymin = lcl, ymax = ucl, colour = CalendarYearGp, fill = CalendarYearGp), alpha = 0.3) +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15)

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
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15)



      } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15)

      } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
        plot <- plot_data %>%
          unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          facet_wrap(vars(facet_var), ncol = 3) +
          theme_bw(base_size = 15)

      } else {
        plot <- plot_data %>%
          ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, colour = CalendarYearGp)) +
          geom_line() +
          xlab("Time (Years)") +
          ylab("Survival Function (%)") +
          theme_bw(base_size = 15)

      }

      # Move scale_y_continuous outside of ggplot
      plot <- plot + scale_y_continuous(limits = c(0, NA)

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
        width = as.numeric(input$survival_download_width),
        height = as.numeric(input$survival_download_height),
        dpi = as.numeric(input$survival_download_dpi),
        units = "cm"
      )
    }
  )
  
   
}