#- learned: iteration in inputs and outputs
#- creation of function applied in a shiny module (inputs only are read in this general module, not in particular functions)

#' @description generate descriptive statistical information of the pull dataset
#' in the application
#' @param id to connect each input and the output in the same session 
#' @return descriptive statistical information
descriptStatsInput_body <- function(id){
  ns <- NS(id)
  list(
    # BOXES
    fluidRow(
      valueBoxOutput(outputId = ns("desc_coef_var"), width = 6),
      valueBoxOutput(outputId = ns("desc_standard_error"), width = 6)
    ),
    fluidRow(
      valueBoxOutput(outputId = ns("desc_skeweness"), width = 6),
      valueBoxOutput(outputId = ns("desc_kurtosis"), width = 6)
    ),
    # TABLES 
    tabsetPanel(
      id = ns("all_levels_tabs"),
      type = "tabs",
      tabPanel(title = "Frequency",
               value = 1,
               DT::DTOutput(ns("frequency_df"))),
      tabPanel(title = "Central Measures",
               value = 2,
               DT::DTOutput(ns("central_df"))),
      tabPanel(title = "Dispersion Measures",
               value = 3,
               DT::DTOutput(ns("dispersion_df"))),
      tabPanel(title = "Conditional Information",
               value = 4,
               tabsetPanel(
                 tabPanel(title = "Conditional Frequency",
                          DT::DTOutput(ns("cond_freq_df"))),
                 tabPanel(title = "Conditional Expectancy",
                          DT::DTOutput(ns("cond_exp_df")))
               ))),
    # PLOTS
    conditionalPanel(ns = ns,
                     condition = "input.level_DescriptStats == 'all'",
                     fluidRow(
                       map2(c("Standard Error", "Comparing Means"),
                            list(plotly::plotlyOutput(ns("plot_standard_error")),
                                 chartjs::chartjsOutput(outputId = ns("plot_comparing_means"),
                                                        height = "399", width = "915")),
                            ~boxPlus(
                              title = .x,
                              collapsible = TRUE,
                              closable = FALSE,
                              width = 6,
                              status = "danger",
                              solidHeader = TRUE,
                              .y))
                     )),
    conditionalPanel(ns = ns,
                     condition = "input.level_DescriptStats == 'specific_level'",
                     fluidRow(
                       map2(c("Ogive Plot", "Acumulative Distribution", 
                              "Histogram", "General Box Plot"),
                            list(plotly::plotlyOutput(ns("plot_acumulative_1")),
                                 plotOutput(ns("plot_acumulative_2")),
                                 plotly::plotlyOutput(ns("plot_histogram")),
                                 plotly::plotlyOutput(ns("plot_level_general_boxplot"))),
                            ~boxPlus(
                              title = .x,
                              collapsible = TRUE,
                              closable = FALSE,
                              width = 6,
                              status = "danger",
                              solidHeader = TRUE,
                              .y)),
                       column(
                         width = 12,
                         offset = 3,
                         boxPlus(
                           title = "Bins Box Plot",
                           collapsible = TRUE,
                           closable = FALSE,
                           width = 6,
                           status = "danger",
                           solidHeader = TRUE,
                           plotly::plotlyOutput(ns("plot_level_bins_boxplot"))))
                     ))
  ) %>% do.call(what = tagList, args = .)
}

#' @description define parameter to select periodicity and other to select level
#' in the application
#' @param id to connect each inputs and the output in the same session  
#' @return inputs to select the options of periodicity and levels in the 
#' application
descriptStatsInput_rightsidebar <- function(id){
  ns <- NS(id)
  fluidRow(
    actionButton(inputId = ns("generateDescriptStats"),
                 label = "Generate Stats",
                 icon = icon(name = "fas fa-file-signature",
                             lib = "font-awesome"))
  )
}

#' @description Creation of interactive clickable descriptive statistical info
#' in the application
#' @param parameters of a shiny module 
#' @return interactive descriptive statistical info
descriptStatsOutput <- function(input, output, session){
  if(file.exists(path)){
    # Read pulled Dataset 
    df <- reaDataset_reactive(id = "pull_desc_stats", path = path)
    # Generate Descriptive Statistics 
    generate_box_info_reactive(id = "pull_desc_stats", #general id
                               id_reference = reactive(input$generateDescriptStats),
                               title = "Generate Descriptive Statistics",
                               uId = "generate_DescriptStats",
                               actionId = "generate_DescriptStatss",
                               uioutput = TRUE)
    output$generate_DescriptStats <- date_selection_reactive(
      id = "pull_desc_stats",
      df = df(),
      add_elements = TRUE,
      uId_add_element = "options_DescriptStats") 
    output$options_DescriptStats <- renderUI({
      ns <- session$ns
      list(      
        radioButtons(inputId = ns("level_DescriptStats"),
                     label = "Select the level to make analysis",
                     choices = list(
                       "All levels" = "all",
                       "Specific level" = "specific_level")),
        hr(),
        conditionalPanel(ns = ns,
                         condition = "input.level_DescriptStats == 'specific_level'",
                         selectInput(inputId = ns("names_leves_DescriptStats"),
                                     label = "Select a level",
                                     choices = 
                                       distribution_reactive(id = "pull_desc_stats",
                                                             df = df()) %>% 
                                       names())
        )) %>% do.call(what = tagList, args = .)
    })
    # Define the dataset for analysis 
    final_df <- define_dataset_reactive(
      id = "pull_desc_stats", 
      df = df(), 
      choices_report = reactive(input$choices_report), 
      year_year = reactive(input$year_year),
      month_year = reactive(input$month_year),
      month_month = reactive(input$month_month),
      semester_year = reactive(input$semester_year),
      semester_semester = reactive(input$semester_semester),
      quarter_year = reactive(input$quarter_year),
      quarter_quarter = reactive(input$quarter_quarter))
    
    coefficients <- reactive({
      map(c("coef_var", "standard_error", 
            "skeweness", "kurtosis"), function(table){
              map(c(TRUE, FALSE), function(description){
                map(c("all", input$names_leves_DescriptStats),
                    ~general_Descript_Stats(df = final_df(),
                                            level_string = .x,
                                            table = table,
                                            description = description)) %>% 
                  set_names("all", "level")
              }) %>% set_names("Description", "Not Description")
            }) %>% set_names("coef_var", "standard_error", 
                             "skeweness", "kurtosis") %>% 
        return()
    })
    observeEvent(input$generate_DescriptStatss, {
      req(final_df())
      factor_variables <- classes_vector(data_type = "factor", 
                                         df = df())
      numeric_int_variables <- classes_vector(data_type = c("integer", "numeric"), 
                                              df = df()) 
      if(length(factor_variables) == 0 | length(numeric_int_variables) == 0 |
         (length(factor_variables) == 0 & length(numeric_int_variables) == 0)){
        message_reactive(id = "pull_desc_stats", type = "fail", 
                         text = "You need to have at least one factor variable and numeric or Integer variables")
        
      }else if(length(numeric_int_variables) != 1){ #in this case for the analysis
        message_reactive(id = "pull_desc_stats", type = "fail", 
                         text = "You need ONE numeric or integer variable to make an UNIVARIABLE ANALYSIS")
        
      }else if(is.character(final_df())){
        message_reactive(id = "pull_desc_stats", type = "fail", 
                         text = "You do not have any information in that period")
        
      }else if(is.null(final_df())){#if the dataset only contains a date and/or character variable (s)
        message_reactive(id = "pull_desc_stats", type = "fail",  
                         text = "You need to pull the correct variables")
      }else{
        if(input$level_DescriptStats == "all"){
          # BOXES
          iwalk(c("desc_coef_var", "desc_standard_error",
                  "desc_skeweness", "desc_kurtosis"), function(x, index){
                    
                    values_coefficients <- list(
                      coefficients()[["coef_var"]][["Not Description"]][["all"]],
                      coefficients()[["standard_error"]][["Not Description"]][["all"]],
                      coefficients()[["skeweness"]][["Not Description"]][["all"]],
                      coefficients()[["kurtosis"]][["Not Description"]][["all"]])
                    
                    desc_coefficients <- list(
                      coefficients()[["coef_var"]][["Description"]][["all"]],
                      coefficients()[["standard_error"]][["Description"]][["all"]],
                      coefficients()[["skeweness"]][["Description"]][["all"]],
                      coefficients()[["kurtosis"]][["Description"]][["all"]])
                    
                    subtitle_coefficients <- c("Coefficient of Variation",
                                               "Standard Error",
                                               "Skeweness",
                                               "Kurtosis")
                    icons_coefficients <- list(
                      icon(name = "glyphicon glyphicon-indent-right", lib = "glyphicon"),
                      icon(name = "glyphicon glyphicon-stats", lib = "glyphicon"),
                      icon(name = "glyphicon glyphicon-signal", lib = "glyphicon"),
                      icon(name = "fas fa-chart-line"))
                    
                    colors_coefficients <- list(
                      ifelse(values_coefficients[[1]] < 1, "aqua", "red"),
                      "aqua",
                      map(3:4, function(i){
                        case_when(
                          values_coefficients[[i]] < 0 ~ "light-blue",
                          values_coefficients[[i]] == 0 ~ "aqua",
                          values_coefficients[[i]] > 0 ~ "navy")
                      })) %>% unlist(recursive = FALSE)
                    
                    output[[x]] <- renderValueBox({
                      valueBox(
                        value = values_coefficients[[index]],
                        subtitle = glue("{subtitle_coefficients[index]} = {desc_coefficients[[index]]}"),
                        icon = icons_coefficients[[index]],
                        color = colors_coefficients[[index]])
                    })
                  })
          # TABLES
          hideTab(inputId = "all_levels_tabs", target = "1")
          showTab(inputId = "all_levels_tabs", target = "4")
          
          output$central_df <- DT::renderDT({
            general_Descript_Stats(df = final_df(),
                                   level_string = "all",
                                   table = "central",
                                   plot = "table") %>% desing_DT()})
          
          iwalk(c("dispersion_df", "cond_freq_df", "cond_exp_df"),
                function(x, index){
                  table <- c("dispersion",
                             "conditional frequency",
                             "conditional expectation")
                  output[[x]] <- DT::renderDT({
                    general_Descript_Stats(df = final_df(),
                                           level_string = "all",
                                           table = table[index]) %>% desing_DT()})
                })
          # PLOTS
          output$plot_standard_error <- plotly::renderPlotly({
            general_Descript_Stats(df = final_df(),
                                   level = "all",
                                   table = "central",
                                   plot = "standard_error")})
          output$plot_comparing_means <- chartjs::renderChartjs({
            general_Descript_Stats(df = final_df(),
                                   level = "all",
                                   table = "central",
                                   plot = "comparing_means")})
          
        }else if(input$level_DescriptStats == "specific_level"){
          # BOXES
          iwalk(c("desc_coef_var", "desc_standard_error",
                  "desc_skeweness", "desc_kurtosis"), function(x, index){
                    
                    values_coefficients <- list(
                      coefficients()[["coef_var"]][["Not Description"]][["level"]],
                      coefficients()[["standard_error"]][["Not Description"]][["level"]],
                      coefficients()[["skeweness"]][["Not Description"]][["level"]],
                      coefficients()[["kurtosis"]][["Not Description"]][["level"]])
                    
                    desc_coefficients <- list(
                      coefficients()[["coef_var"]][["Description"]][["level"]],
                      coefficients()[["standard_error"]][["Description"]][["level"]],
                      coefficients()[["skeweness"]][["Description"]][["level"]],
                      coefficients()[["kurtosis"]][["Description"]][["level"]])
                    
                    subtitle_coefficients <- c("Coefficient of Variation",
                                               "Standard Error",
                                               "Skeweness",
                                               "Kurtosis")
                    icons_coefficients <- list(
                      icon(name = "glyphicon glyphicon-indent-right", lib = "glyphicon"),
                      icon(name = "glyphicon glyphicon-stats", lib = "glyphicon"),
                      icon(name = "glyphicon glyphicon-signal", lib = "glyphicon"),
                      icon(name = "fas fa-chart-line"))
                    
                    colors_coefficients <- list(
                      ifelse(values_coefficients[[1]] < 1, "aqua", "red"),
                      "aqua",
                      map(3:4, function(i){
                        case_when(
                          values_coefficients[[i]] < 0 ~ "light-blue",
                          values_coefficients[[i]] == 0 ~ "aqua",
                          values_coefficients[[i]] > 0 ~ "navy")
                      })) %>% unlist(recursive = FALSE)
                    
                    output[[x]] <- renderValueBox({
                      valueBox(
                        value = values_coefficients[[index]],
                        subtitle = glue("{subtitle_coefficients[index]} = {desc_coefficients[[index]]}"),
                        icon = icons_coefficients[[index]],
                        color = colors_coefficients[[index]])
                    })
                  })
          # TABLES
          showTab(inputId = "all_levels_tabs", target = "1")
          hideTab(inputId = "all_levels_tabs", target = "4")
          
          output$dispersion_df <- DT::renderDT({
            general_Descript_Stats(df = final_df(),
                                   level_string = input$names_leves_DescriptStats,
                                   table = "dispersion") %>% desing_DT()})
          
          iwalk(c("frequency_df", "central_df"), function(x, index){
            table <- c("frequency", "central")
            output[[x]] <- DT::renderDT({
              general_Descript_Stats(df = final_df(),
                                     level_string = input$names_leves_DescriptStats,
                                     table = table[index],
                                     plot = "table") %>% desing_DT()})
          })
          # PLOTS
          output$plot_acumulative_2 <- renderPlot({
            general_Descript_Stats(df = final_df(),
                                   level_string = input$names_leves_DescriptStats,
                                   table = "frequency",
                                   plot = "acumulative_2")
          })
          iwalk(c("plot_acumulative_1", "plot_histogram",
                  "plot_level_general_boxplot", "plot_level_bins_boxplot"),
                function(x, index){
                  table <- c(rep("frequency", 2), rep("central", 2))
                  plot <- c("acumulative_1", "histogram",
                            "level_general_boxplot", "level_bins_boxplot")
                  
                  output[[x]] <- plotly::renderPlotly({
                    general_Descript_Stats(df = final_df(),
                                           level_string = input$names_leves_DescriptStats,
                                           table =table[index] ,
                                           plot = plot[index])})
                })
        }
        message_reactive(id = "pull_desc_stats", type = "success", 
                         text = "Corrected pull information")
      }
    })
  }else{
    generate_box_info_reactive(id = "pull_desc_stats",
                               id_reference = reactive(input$generateDescriptStats),
                               title = "Generate Descriptive Statistics",
                               uioutput = FALSE,
                               message_string = "Please pull a Dataset from the SQL server")
  }
}