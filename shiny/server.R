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
  
  # database details
  # output$tbl_database_details <- renderText(kable(database_details) %>%
  #                                             kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_database_details_word <- downloadHandler(
  #   filename = function() {
  #     "database_description.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(database_details)
  #     gtsave(x, file)
  #   }
  # )
  # 
  
  # # attrition details
  # output$tbl_incidence_attrition <- renderText(kable(incidence_attrition) %>%
  #                                             kable_styling("striped", full_width = F) )
  # 
  # 
  # output$dt_incidence_attrition_word <- downloadHandler(
  #   filename = function() {
  #     "incidence_attrition.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(incidence_attrition)
  #     gtsave(x, file)
  #   }
  # )
  
  
  # cohort attrition -----
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
  
  # clinical codelists
  # output$tbl_codelists <- renderText(kable(concepts_lists) %>%
  #                                      kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_codelists_word <- downloadHandler(
  #   filename = function() {
  #     "concept_lists.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(concepts_lists)
  #     gtsave(x, file)
  #   }
  # )
  

  
  
  
  # # demographics --------
  # 
  # get_demographics <- reactive({
  #   
  #   table <- demographics %>% 
  #     filter(Sex %in% input$tableone_sex_selector) %>% 
  #     filter(Age %in% input$tableone_age_selector) 
  #   
  #   selected_columns <- c("Description", input$demographics_database_name_selector)
  #   table <- table[, selected_columns, drop = FALSE]
  #   
  #   table
  #   
  # }) 
  # 
  # 
  # output$dt_demographics <- renderText(kable(get_demographics()) %>%
  #                                        kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_demographics_word <- downloadHandler(
  #   filename = function() {
  #     "demographics.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_demographics())
  #     gtsave(x, file)
  #   }
  # )  
  # 
  # 
  # # table one --------
  # 
  # get_table_one <- reactive({
  #   
  #   table <- tableone_final %>% 
  #     filter(Cancer %in% input$tableone_cohort_name_selector) %>% 
  #     filter(Sex %in% input$tableone_sex_selector) %>% 
  #     filter(Age %in% input$tableone_age_selector) 
  #   
  #   selected_columns <- c("Description", input$tableone_database_name_selector)
  #   table <- table[, selected_columns, drop = FALSE]
  #   
  #   table
  #   
  # }) 
  # 
  # 
  # output$dt_tableone <- renderText(kable(get_table_one()) %>%
  #                                    kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_tableone_word <- downloadHandler(
  #   filename = function() {
  #     "table_one.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_table_one())
  #     gtsave(x, file)
  #   }
  # )
  
  output$dt_demographics <- renderText(tableone_whole_processed)


  output$gt_demo_word <- downloadHandler(
    filename = function() {
      "table_one.docx"
    },
    content = function(file) {
      x <- gt(tableone_whole_processed)
      gtsave(x, file)
    }
  )

  
  
  # 
  # 
  # # gof results --------
  # get_gof <- reactive({
  #   
  #   table <- GOFResults %>% 
  #     filter(Database %in% input$gof_database_selector) %>% 
  #     filter(Cancer %in% input$gof_cohort_name_selector) %>% 
  #     filter(Sex %in% input$gof_sex_selector) %>% 
  #     filter(Age %in% input$gof_age_selector) 
  #   
  #   table
  #   
  # }) 
  # 
  # 
  # output$dt_gof <- renderText(kable(get_gof()) %>%
  #                               kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_gof_word <- downloadHandler(
  #   filename = function() {
  #     "gof.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_gof())
  #     gtsave(x, file)
  #   }
  # )  
  # 
  # # extrapolation parameters --------
  # get_param <- reactive({
  #   
  #   table <- ExtrpolationParameters %>% 
  #     filter(Database %in% input$param_database_selector) %>% 
  #     filter(Cancer %in% input$param_cohort_name_selector) %>% 
  #     filter(Sex %in% input$param_sex_selector) %>% 
  #     filter(Age %in% input$param_age_selector) 
  #   
  #   table
  #   
  # }) 
  # 
  # # output$dt_param <- renderText({
  # #   get_param_text <- kable(get_param()) %>%
  # #     kable_styling("striped", full_width = F)
  # #   
  # #   
  # # })
  # 
  # output$dt_param <- renderDT({
  #   datatable(get_param(), options = list(scrollX = TRUE, 
  #                                         dom = 't', 
  #                                         searching = FALSE), 
  #             rownames = FALSE, width = '50%')
  # })
  # 
  # # 
  # # output$dt_param <- renderText(kable(get_param()) %>%
  # #                               kable_styling("striped", full_width = F) )
  # 
  # 
  # output$gt_param_word <- downloadHandler(
  #   filename = function() {
  #     "extrapolation_parameters.docx"
  #   },
  #   content = function(file) {
  #     x <- gt(get_param())
  #     gtsave(x, file)
  #   }
  # )  
  # 
  # 
  # get_surv_plot <- reactive({
  #   plot_data <- survivalResults %>%
  #     filter(Database %in% input$survival_database_selector) %>%
  #     filter(Cancer %in% input$survival_cohort_name_selector) %>%
  #     filter(Age %in% input$survival_age_selector) %>%
  #     filter(Sex %in% input$survival_sex_selector) %>%
  #     filter(Method %in% input$survival_method_selector)
  #   
  #   if (!is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
  #       unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       #scale_linetype_manual(values = rep("solid", length(unique(plot_data$Method)))) +
  #       xlab("Time (Years)") +
  #       ylab("Survival Function (%)") +
  #       facet_wrap(vars(facet_var), ncol = 2) +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else if (!is.null(input$surv_plot_group) && is.null(input$surv_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("Group", c(all_of(input$surv_plot_group)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       #scale_linetype_manual(values = rep("solid", length(unique(plot_data$Method)))) +
  #       xlab("Time (Years)") +
  #       ylab("Survival Function (%)") +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else if (is.null(input$surv_plot_group) && !is.null(input$surv_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("facet_var", c(all_of(input$surv_plot_facet)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       #scale_linetype_manual(values = rep("solid", length(unique(plot_data$Method)))) +
  #       xlab("Time (Years)") +
  #       ylab("Survival Function (%)") +
  #       facet_wrap(vars(facet_var), ncol = 2) +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else {
  #     plot <- plot_data %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       # scale_linetype_manual(values = rep("solid", length(unique(plot_data$Method)))) +
  #       xlab("Time (Years)") +
  #       ylab("Survival Function (%)") +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   }
  #   
  #   # Move scale_y_continuous outside of ggplot
  #   plot <- plot + scale_y_continuous(limits = c(0, NA))
  #   
  #   plot
  # })
  # 
  # output$survivalPlot <- renderPlot(
  #   get_surv_plot()
  # )
  # 
  # 
  # get_hot_plot <- reactive({
  #   plot_data <- hazOverTimeResults %>%
  #     filter(Database %in% input$hot_database_selector) %>%
  #     filter(Cancer %in% input$hot_cohort_name_selector) %>%
  #     filter(Age %in% input$hot_age_selector) %>%
  #     filter(Sex %in% input$hot_sex_selector) %>%
  #     filter(Method %in% input$hot_method_selector)
  #   
  #   if (!is.null(input$hot_plot_group) && !is.null(input$hot_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("Group", c(all_of(input$hot_plot_group)), remove = FALSE, sep = "; ") %>%
  #       unite("facet_var", c(all_of(input$hot_plot_facet)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       xlab("Time (Years)") +
  #       ylab("Hazard Function") +
  #       facet_wrap(vars(facet_var), ncol = 2) +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else if (!is.null(input$hot_plot_group) && is.null(input$hot_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("Group", c(all_of(input$hot_plot_group)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       xlab("Time (Years)") +
  #       ylab("Hazard Function") +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else if (is.null(input$hot_plot_group) && !is.null(input$hot_plot_facet)) {
  #     plot <- plot_data %>%
  #       unite("facet_var", c(all_of(input$hot_plot_facet)), remove = FALSE, sep = "; ") %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       xlab("Time (Years)") +
  #       ylab("Hazard Function") +
  #       facet_wrap(vars(facet_var), ncol = 2) +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   } else {
  #     plot <- plot_data %>%
  #       ggplot(aes(x = time, y = est, ymin = lcl, ymax = ucl, group = Group, colour = Group, fill = Group)) +
  #       geom_line(aes(size = ifelse(Method == "Kaplan-Meier", "Thicker", "Regular"))) +
  #       scale_size_manual(values = c("Thicker" = 1.5, "Regular" = 0.75)) +
  #       xlab("Time (Years)") +
  #       ylab("Hazard Function") +
  #       theme_bw() +
  #       guides(size = FALSE)
  #     
  #   }
  #   
  #   # Move scale_y_continuous outside of ggplot
  #   plot <- plot + scale_y_continuous(limits = c(0, NA))
  #   
  #   plot
  # })
  # 
  # output$hotPlot <- renderPlot(
  #   get_hot_plot()
  # ) 
  
  
  
  
  
   
}