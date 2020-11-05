#right
continous_distInput_rightsidebar <- function(id){
  uiOutput(NS(id, "cont_distrib"))
  
}
#body
continous_distInput_body <- function(id){
  uiOutput(NS(id, "cont_body_distrib"))
}

#output
continous_distOutput <- function(input, output, session){
  
  ns <- session$ns
  
  # Right Side --------------------------------------------------------------
  output$cont_distrib <- renderUI({
    
    list(
      selectInput(inputId = ns("cont_dist"),
                  label = "Distribution",
                  choices = c("Exponential" = "exponential",
                              "Gamma" = "gamma",
                              "Chi Square" = "chi_square",
                              "Beta" =  "beta",
                              "Weibull" = "weibull",
                              "T Student" = "student",
                              "Logistic" = "logistic",
                              "Normal" = "normal",
                              "Log Normal" = "log_normal",
                              "Cauchy" = "cauchy",
                              "Fisher" = "fisher"),
                  multiple = FALSE,
                  selected = NULL),
      hr(),
      uiOutput(ns("continous_arguments")),
      hr(),
      radioButtons(inputId = ns("cont_tail"),
                   label = NULL,
                   choices = helpCont_library(choice = "tail")),
      hr(),
      uiOutput(ns("continous_values"))
      
    ) %>% do.call(what = tagList, args = .)
  })
  
  output$continous_arguments <- renderUI({
    inumericInput_reactive(id = "continous_distributions",
                           distribution = reactive(input$cont_dist) %>% 
                             debounce(millis = 500), #debounce necesary to obligate not to lock the inputs
                           choice = "arguments")
  })
  output$continous_values <- renderUI({
    inumericInput_reactive(id = "continous_distributions",
                           distribution = reactive(input$cont_dist) %>% 
                             debounce(millis = 500),
                           tail = reactive(input$cont_tail) %>% 
                             debounce(millis = 500),
                           choice = "x_value")
  })
  
  # Body Side ---------------------------------------------------------------
  output$cont_body_distrib <- renderUI({
    
    req(input$cont_dist, input$cont_tail) #not errors at the beginning
    
    distribution_body(id = "continous_distributions", 
                      tab_table = "dt_continous", 
                      tab_plot = "plot_continous", 
                      id_reference = "accordion_continous", 
                      uId_distribution = "distribution_continous",
                      uId_probability = "probability_continous", 
                      uId_summary = "summary_continous")
  })
  
  # Dinamic Inputs #check isolate to bring the result ones
  values_arguments <- reactive({
    inputIds <- map(c("arguments", "x_value"),
                    ~pluck(map(helpCont_library(choice = .x), input$cont_dist),
                           "inputId"))
   
    values_inputIds <- map(map(inputIds, ~glue("input${.x}")), 
                           ~map(.x, ~eval(parse(text = ..1)))) %>%  #eval parse is not working in  particular module
      set_names("arguments", "values")
    values_inputIds 
  })
  
  # Outputs 
  output$dt_continous <- DT::renderDT({
    continous_distribution_reactive(id = "continous_distributions",
                                    values = values_arguments(),
                                    distribution = reactive(input$cont_dist) %>% 
                                      debounce(millis = 300),
                                    tail = reactive(input$cont_tail) %>% 
                                      debounce(millis = 300),
                                    result_choice = "table_plot",
                                    return = "table")
  })
  output$plot_continous <- renderPlot({
    continous_distribution_reactive(id = "continous_distributions",
                                    values = values_arguments(),
                                    distribution = reactive(input$cont_dist) %>% 
                                      debounce(millis = 300),
                                    tail = reactive(input$cont_tail) %>% 
                                      debounce(millis = 300),
                                    result_choice = "table_plot",
                                    return = "plot")
  })
  output$distribution_continous <- renderUI({
    continous_distribution_reactive(id = "continous_distributions",
                                    values = values_arguments(),
                                    distribution = reactive(input$cont_dist) %>% 
                                      debounce(millis = 300),
                                    tail = reactive(input$cont_tail) %>% 
                                      debounce(millis = 300),
                                    result_choice = "distribution")
  })
  output$probability_continous <- renderUI({
    distribution_function_representation(distribution_choice = input$cont_dist)
  })
  output$summary_continous <- renderUI({
    continous_distribution_reactive(id = "continous_distributions",
                                    values = values_arguments(),
                                    distribution = reactive(input$cont_dist),
                                    result_choice = "summary")
  })
}


