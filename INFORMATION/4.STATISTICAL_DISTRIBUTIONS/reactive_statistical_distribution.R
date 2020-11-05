# Right Side --------------------------------------------------------------
#' @description generate the right side of the parameters in the application
#' @param id to connect each input and the output in the same session 
#' @return inputs to select the options in the application
distributionInput_rightsidebar <- function(id){
  uiOutput(NS(id, "distribution_rightsidebar"))
}

# Body Side ---------------------------------------------------------------
#' @description generate the body in the application
#' @param id to connect each input and the output in the same session 
#' @return representation of the app
distributionInput_body <- function(id){
  uiOutput(NS(id, "distribution_body")) 
}

# Output ------------------------------------------------------------------
#' @description Creation of interactive clickable statistical distributions
#' in the application
#' @param parameters of a shiny module 
#' @return interactive statistical distributions
distributionOutput <- function(input, output, session, 
                               distribution_type = c("DISCRETE", "CONTINOUS")){
  ns <- session$ns
  
  dist_names <- list(
    DISCRETE = c("Binomial", "Hyper-Geometric", "Poisson",
                 "Geometric (I)", "Geometric (II)",
                 "Negative Binomial (I)", "Negative Binomial (II)"),
    CONTINOUS = c("Exponential", "Gamma", "Chi Square",
                  "Beta", "Weibull", "T Student",
                  "Logistic", "Normal", "Log Normal",
                  "Cauchy", "Fisher")
  )
  specific_dist <- map2(dist_types, dist_names, 
                        ~ .x %>% set_names(.y)) %>% 
    pluck(distribution_type)
  
  id_distribution <- c(DISCRETE = "discrete_distributions",
                       CONTINOUS = "continous_distributions")
  id_distribution <- id_distribution[distribution_type]
  
  # Right Side --------------------------------------------------------------
  output$distribution_rightsidebar <- renderUI({
    list(
      selectInput(inputId = ns("specific_distribution"),
                  label = "Distribution",
                  choices = specific_dist,
                  multiple = FALSE,
                  selected = NULL),
      hr(),
      uiOutput(ns("distribution_args")),
      hr(),
      radioButtons(inputId = ns("distribution_tails"),
                   label = NULL, 
                   choices = helpDist_library(choice = "tail")),
      hr(),
      uiOutput(ns("distribution_values"))
    ) %>% do.call(what = tagList, args = .)
  })
  
  output$distribution_args <- renderUI({
    distribution_dinamicInput(id = id_distribution, 
                              distribution_type = distribution_type,
                              choice = "arguments",
                              distribution = reactive(input$specific_distribution) %>% 
                                debounce(millis = 500)) #debounce necesary to obligate not to lock the inputs
  })
  output$distribution_values <- renderUI({
    distribution_dinamicInput(id = id_distribution, 
                              distribution_type = distribution_type,
                              choice = "x_value",
                              distribution = reactive(input$specific_distribution) %>% 
                                debounce(millis = 500),
                              tail = reactive(input$distribution_tails) %>% 
                                debounce(millis = 500)) 
  })
  
  # Body Side ---------------------------------------------------------------
  tab_plot <- "distribution_plot"
  
  output$distribution_body <- renderUI({
    
    req(input$specific_distribution, input$distribution_tails) #not errors at the beginning
    
    distribution_body(id = id_distribution, 
                      distribution_type = distribution_type, 
                      tab_table = "distribution_dt", 
                      tab_plot = tab_plot,
                      id_reference = "distribution_accordion", 
                      uId_distribution = "distribution_repre", 
                      uId_probability = "distribution_func_repre",
                      uId_summary = "summary_repre")
  })
  # Dinamic Inputs
  values_arguments <- reactive({
    inputIds <- map(c("arguments", "x_value"),
                    ~pluck(
                      map(helpDist_library(distribution_type = distribution_type,
                                           choice = .x), input$specific_distribution),
                      "inputId")
    )
    values_inputIds <- map(map(inputIds, ~glue("input${.x}")), 
                           ~ map(..1, ~eval(parse(text = .x)))) %>%  #eval parse is not working in  particular module
      set_names("arguments", "x_value")
    values_inputIds
  })
  
  # Outputs
  ## TABLE
  output$distribution_dt <- DT::renderDT({
    distribution_reactive(id = id_distribution, 
                          distribution_type = distribution_type, 
                          distribution = reactive(input$specific_distribution) %>% 
                            debounce(millis = 300), 
                          values = values_arguments(),
                          tail = reactive(input$distribution_tails) %>% 
                            debounce(millis = 300),
                          result_choice = "table_plot",
                          return = "table")
  })
  
  ## PLOT
  distributions_plot <- reactive({
    distribution_reactive(id = id_distribution,
                          distribution_type = distribution_type,
                          distribution = reactive(input$specific_distribution) %>%
                            debounce(millis = 300), 
                          values = values_arguments(),
                          tail = reactive(input$distribution_tails) %>%
                            debounce(millis = 300),
                          result_choice = "table_plot",
                          return = "plot")
  })
  output[[glue("DISCRETE_{tab_plot}")]] <- plotly::renderPlotly({
    distributions_plot()
  })
  output[[glue("CONTINOUS_{tab_plot}")]] <- renderPlot({
    distributions_plot()
  })
  
  ## DISTRIBUTION REPRESENTATION
  output$distribution_repre <- renderUI({
    distribution_reactive(id = id_distribution, 
                          distribution_type = distribution_type, 
                          distribution = reactive(input$specific_distribution) %>% 
                            debounce(millis = 300), 
                          values = values_arguments(),
                          tail = reactive(input$distribution_tails) %>% 
                            debounce(millis = 300),
                          result_choice = "distribution")
  })
  ## DISTRIBUTION FUNCTION REPRESENTATION
  output$distribution_func_repre <- renderUI({
    distribution_function_representation(
      distribution_choice = input$specific_distribution)
  })
  ## SUMMARY REPRESENTATION
  output$summary_repre <- renderUI({
    distribution_reactive(id = id_distribution, 
                          distribution_type = distribution_type, 
                          distribution = reactive(input$specific_distribution) %>% 
                            debounce(millis = 300), 
                          values = values_arguments(),
                          result_choice = "summary")
  })
}

