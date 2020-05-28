####### Statistical Distributions ##########################

### Discrete Distributions ##################

# rightsidebar ------------------------------------------------------------
#' @description create all the necessary inputs for each discrete distribution
#' @param id to connect each inputs and the output in the same session 
#' @return inputs for the rightsidebar
discrete_distInput_rightsidebar <- function(id){
  ns = NS(id)
  tagList( #to allow to work the conditionalpannel in a function
    selectInput(inputId = ns("discrete"), label = "Distribution", 
                choices = c("Binomial", "Geometric", "Negative Binomial",
                            "Hyper-Geometric", "Poisson"),
                multiple = FALSE,
                selected = NULL),
    hr(), #Add a horizontal line for separation
    #Binomial
    conditionalPanel(ns = ns, #this is necessary in each conditionalpannel to work
                     condition = "input.discrete == 'Binomial'",
                     numericInput(inputId = ns("n_binomial"), label =  "Number of trials \\(n\\):",
                                  min = 0, step = 1, value = 20),
                     hr(),
                     numericInput(inputId = ns("p_binomial"), label = "Probability of success \\(p\\):",
                                  min = 0, max = 1, step = 0.01, value = 0.5),
                     hr(),
                     radioButtons(inputId = ns("tails_binomial"), label = NULL,
                                  choices = c(
                                    "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                    "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                    "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                  )),
                     hr(),
                     conditionalPanel(ns = ns,
                                      condition = "input.tails_binomial == 'lower_tail' || input.tails_binomial == 'upper_tail'",
                                      numericInput(inputId = ns("x_binomial"), label = "\\(x\\):", 
                                                   min = 0, step = 1, value = 8)),
                     conditionalPanel(ns = ns,
                                      condition = "input.tails_binomial == 'interval'",
                                      numericInput(inputId = ns("a_binomial"), label = "\\(a\\):", 
                                                   min = 0, step = 1, value = 8),
                                      numericInput(inputId = ns("b_binomial"), label = "\\(b:\\ (a \\leq b) \\)", 
                                                   min = 0, step = 1, value = 8))
    ),
    #Geometric
    conditionalPanel(ns = ns,
                     condition = "input.discrete ==  'Geometric'",
                     selectInput(inputId = ns("geometric"), label = NULL,
                                 choices = c("Geometric (I)" = "geometric1", "Geometric(II)" = "geometric2")),
                     hr(),
                     ##Geometric (I)
                     conditionalPanel(ns = ns,
                                      condition = "input.geometric == 'geometric1'",
                                      numericInput(inputId = ns("p_geometric1"), label = "Probability of success \\(p\\):",
                                                   min = 0, max = 1, step = 0.01, value = 0.5),
                                      hr(),
                                      radioButtons(inputId = ns("tails_geometric1"), label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_geometric1 == 'lower_tail' || input.tails_geometric1 == 'upper_tail'",
                                                       helpText("Number of failures before the \\(1^{st}\\) success"), #Add additional text
                                                       numericInput(inputId = ns("x_geometric1"), label = "\\(x\\):",
                                                                    min = 0, step = 1, value = 1)),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_geometric1 == 'interval'",
                                                       helpText("Number of failures before the \\(1^{st}\\) success"),
                                                       numericInput(inputId = ns("a_geometric1"), label = "\\(a\\):",
                                                                    min = 0, step = 1, value = 1),
                                                       numericInput(inputId = ns("b_geometric1"), label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 0, step = 1, value = 1))
                     ),
                     ##Geometric (II)
                     conditionalPanel(ns = ns,
                                      condition = "input.geometric == 'geometric2'",
                                      numericInput(inputId = ns("p_geometic2"), label = "Probability of success \\(p\\):",
                                                   min = 0, max = 1, step = 0.01, value = 0.5),
                                      hr(),
                                      radioButtons(inputId = ns("tails_geometric2"), label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_geometric2 == 'lower_tail' || input.tails_geometric2 == 'upper_tail'",
                                                       helpText("The trial on which the \\(1^{st}\\) success occurs"),
                                                       numericInput(inputId = ns("x_geometric2"), label = "\\(x\\):",
                                                                    min = 1, step = 1, value = 2)),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_geometric2 == 'interval'",
                                                       helpText("The trial on which the \\(1^{st}\\) success occurs"),
                                                       numericInput(inputId = ns("a_geometric2"), label = "\\(a\\):",
                                                                    min = 1, step = 1, value = 2),
                                                       numericInput(inputId = ns("b_geometric2"), label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 1, step = 1, value = 2))
                     )
    ),
    #Negative Binomial
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Negative Binomial'",
                     selectInput(inputId = ns("negativebinomial"), label = NULL,
                                 choices = c("Negative Binomial (I)" = "negativebinomial1", "Negative Binomial (II)" = "negativebinomial2")),
                     hr(),
                     #Negative Binomial (I)
                     conditionalPanel(ns = ns,
                                      condition = "input.negativebinomial == 'negativebinomial1'",
                                      numericInput(inputId = ns("r_negativebinomial1"), label = "Number of successes \\(r\\):",
                                                   min = 1, step = 1, value = 5),
                                      hr(),
                                      numericInput(inputId = ns("p_negativebinomial1"), label = "Probability of success \\(p\\):",
                                                   min = 0, max = 1, step = 0.01, value = 0.5),
                                      hr(),
                                      radioButtons(inputId = ns("tails_negativebinomial1"), label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_negativebinomial1 == 'lower_tail' || input.tails_negativebinomial1 == 'upper_tail'",
                                                       helpText("Number of failures before the \\(r^{th}\\) success"),
                                                       numericInput(inputId = ns("x_negativebinomial1"), label = "\\(x\\):",
                                                                    min = 0, step = 1, value = 2)),
                                      conditionalPanel(ns = ns, 
                                                       condition = "input.tails_negativebinomial1 == 'interval'",
                                                       helpText("Number of failures before the \\(r^{th}\\) success"),
                                                       numericInput(inputId = ns("a_negativebinomial1"), label = "\\(a\\):",
                                                                    min = 0, step = 1, value = 2),
                                                       numericInput(inputId = ns("b_negativebinomial1"), label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 0, step = 1, value = 2))              
                     ),
                     #Negative Binomial (II)
                     conditionalPanel(ns = ns,
                                      condition = "input.negativebinomial == 'negativebinomial2'",
                                      numericInput(inputId = ns("r_negativebinomial2"), label = "Number of successes \\(r\\):",
                                                   min = 1, step = 1, value = 5),
                                      hr(),
                                      numericInput(inputId = ns("p_negativebinomial2"), label = "Probability of success \\(p\\):",
                                                   min = 0, max = 1, step = 0.01, value = 0.5),
                                      hr(),
                                      radioButtons(inputId = ns("tails_negativebinomial2"), label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_negativebinomial2 == 'lower_tail' || input.tails_negativebinomial2 == 'upper_tail'",
                                                       helpText("The trial on which the \\(r^{th}\\) success occurs"),
                                                       numericInput(inputId = ns("x_negativebinomial2"), label = "\\(x\\): \\( (x \\geq r) \\)",
                                                                    min = 1, step = 1, value = 6)),
                                      conditionalPanel(ns = ns,
                                                       condition = "input.tails_negativebinomial2 == 'interval'",
                                                       helpText("The trial on which the \\(r^{th}\\) success occurs"),
                                                       numericInput(inputId = ns("a_negativebinomial2"), label = "\\(a\\): \\( (a \\geq r) \\)",
                                                                    min = 1, step = 1, value = 6),
                                                       numericInput(inputId = ns("b_negativebinomial2"), label = "\\(b\\): \\( (b \\geq r) \\)",
                                                                    min = 1, step = 1, value = 6))
                     )
    ),
    #Hyper-Geometric
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Hyper-Geometric'",
                     numericInput(inputId = ns("n_hypergeometric"), label = "Sample size \\(n\\):",
                                  min = 0, step = 1, value = 100),
                     hr(),
                     numericInput(inputId = ns("N_hypergeometric"), label = "Total number of objects \\(N\\):",
                                  min = 0, step = 1, value = 500),
                     hr(),
                     numericInput(inputId = ns("M_hypergeometric"), label = "Number of successes \\(M\\):",
                                  min = 0, step = 1, value = 50),
                     hr(),
                     radioButtons(inputId = ns("tails_hypergeometric"), label = NULL,
                                  choices = c(
                                    "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                    "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                    "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                  )),
                     hr(),
                     conditionalPanel(ns = ns, 
                                      condition = "input.tails_hypergeometric == 'lower_tail' || input.tails_hypergeometric == 'upper_tail'",
                                      numericInput(inputId = ns("x_hypergeometric"), label = "\\(x\\):",
                                                   min = 0, step = 1, value = 8)),
                     conditionalPanel(ns = ns,
                                      condition = "input.tails_hypergeometric == 'interval'",
                                      numericInput(inputId = ns("a_hypergeometric"), label = "\\(a\\):",
                                                   min = 0, step = 1, value = 8),
                                      numericInput(inputId = ns("b_hypergeometric"), label = "\\(b:\\ (a \\leq b) \\)",
                                                   min = 0, step = 1, value = 8))
    ),
    #Poisson
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Poisson'",
                     numericInput(inputId = ns("lambda_poisson"), label = "Rate \\(\\lambda\\):",
                                  min = 1, step = 1, value = 4),
                     hr(),
                     radioButtons(inputId = ns("tails_poisson"), label = NULL,
                                  choices = c(
                                    "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                    "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                    "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                  )),
                     hr(),
                     conditionalPanel(ns = ns,
                                      condition = "input.tails_poisson == 'lower_tail' || input.tails_poisson == 'upper_tail'",
                                      numericInput(inputId = ns("x_poisson"), label = "\\(x\\):",
                                                   min = 0, step = 1, value = 6)),
                     conditionalPanel(ns = ns,
                                      condition = "input.tails_poisson == 'interval'",
                                      numericInput(inputId = ns("a_poisson"), label = "\\(a\\):",
                                                   min = 0, step = 1, value = 6),
                                      numericInput(inputId = ns("b_poisson"), label = "\\(b:\\ (a \\leq b) \\)",
                                                   min = 0, step = 1, value = 6))
    )
  )
}

# BODY --------------------------------------------------------------------
#' @description create all the necessary inputs for each discrete distribution
#' @param id to connect each inputs and the output in the same session 
#' @return dataset, visualizations and ui references in boxes
discrete_distInput_body <- function(id){
  ns = NS(id)
  tagList(
    #Binomial
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Binomial'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_Binomial"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_Binomial")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 4,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_Binomial"))),
                             accordionItem(
                               id = 5,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_Binomial"))),
                             accordionItem(
                               id = 6,
                               title = "Summary",
                               color = "info",
                               collapsed = TRUE,
                               uiOutput(ns("summary_Binomial")))
                           ))))
    ),
    #Geometric (I)
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Geometric' && input.geometric == 'geometric1'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_geometric1"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_geometric1")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 7,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_geometric1"))),
                             accordionItem(
                               id = 8,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_geometric1"))),
                             accordionItem(
                               id = 9,
                               title = "Summary",
                               color = "info",
                               collapsed = TRUE,
                               uiOutput(ns("summary_geometric1")))
                           ))))
    ),
    #Geometric (II)
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Geometric' && input.geometric == 'geometric2'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_geometric2"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_geometric2")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 10,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_geometric2"))),
                             accordionItem(
                               id = 11,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_geometric2"))),
                             accordionItem(
                               id = 12,
                               title = "Summary",
                               color = "info",
                               collapsed = TRUE,
                               uiOutput(ns("summary_geometric2")))
                           ))))
    ),
    #Negative Binomial (I)
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Negative Binomial' && input.negativebinomial == 'negativebinomial1'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_negativebinomial1"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_negativebinomial1")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 13,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_negativebinomial1"))),
                             accordionItem(
                               id = 14,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_negativebinomial1"))),
                             accordionItem(
                               id = 15,
                               title = "Summary",
                               color = "info",
                               collapsed = TRUE,
                               uiOutput(ns("summary_negativebinomial1")))
                           ))))
    ),
    #Negative Binomial (II)
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Negative Binomial' && input.negativebinomial == 'negativebinomial2'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_negativebinomial2"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_negativebinomial2")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 16,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_negativebinomial2"))),
                             accordionItem(
                               id = 17,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_negativebinomial2"))),
                             accordionItem(
                               id = 18,
                               title = "Summary",
                               color = "info",
                               uiOutput(ns("summary_negativebinomial2")))
                           ))))
    ),
    #Hyper-Geometric
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Hyper-Geometric'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_HyperGeometric"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_HyperGeometric")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 19,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_HyperGeometric"))),
                             accordionItem(
                               id = 20,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_HyperGeometric"))),
                             accordionItem(
                               id = 21,
                               title = "Summary",
                               color = "info",
                               uiOutput(ns("summary_HyperGeometric")))
                           ))))
    ),
    #Poisson
    conditionalPanel(ns = ns,
                     condition = "input.discrete == 'Poisson'",
                     tabsetPanel(
                       tabPanel(title = "Dataset",
                                DT::DTOutput(ns("dt_Poisson"))),
                       tabPanel(title = "Plot",
                                plotly::plotlyOutput(ns("plot_Poisson")))
                     ),
                     fluidRow(
                       column(
                         width = 6,
                         offset = 3,
                         box(
                           title = "Distribution Information",
                           width = NULL,
                           accordion(
                             accordionItem(
                               id = 22,
                               title = "Distribution Function",
                               color = "danger",
                               collapsed = TRUE,
                               uiOutput(ns("distribution_Poisson"))),
                             accordionItem(
                               id = 23,
                               title = "Probability Function",
                               color = "warning",
                               collapsed = TRUE,
                               uiOutput(ns("probability_Poisson"))),
                             accordionItem(
                               id = 24,
                               title = "Summary",
                               color = "info",
                               collapsed = TRUE,
                               uiOutput(ns("summary_Poisson")))
                           ))))
    )
  )
}

# OUTPUT ------------------------------------------------------------------
#' @description create submodules each representing a discrete distribution
#' @param parameters of a shiny module 
#' @return serever representation of each discreate distribution
discrete_distOutput <- function(input, output, session){
  #Binomial
  my_binomial_reactive <- callModule(module = binomial_reactive, id = "bin_react",
                                     tails = reactive(input$tails_binomial), n = reactive(input$n_binomial), 
                                     p = reactive(input$p_binomial), x = reactive(input$x_binomial), 
                                     a = reactive(input$a_binomial), b = reactive(input$b_binomial))
  
  my_binomial_information_reactive <- callModule(module = binomial_information_reactive, id = "bin_info_react", 
                                                 tails = reactive(input$tails_binomial), n = reactive(input$n_binomial), 
                                                 p = reactive(input$p_binomial), x = reactive(input$x_binomial), 
                                                 a = reactive(input$a_binomial), b = reactive(input$b_binomial))
  
  output$dt_Binomial <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_binomial_reactive())})
  output$plot_Binomial <- plotly::renderPlotly({discrete_plot(df_discrete = my_binomial_reactive())})
  output$distribution_Binomial <- renderUI({my_binomial_information_reactive()})
  output$probability_Binomial <- renderUI({prob_representation (distribution_choice = "binomial")})
  output$summary_Binomial <- renderUI({summary_representation(distribution_choice = "binomial", n_dist = input$n_binomial, p_dist = input$p_binomial)})
  
  
  #Geometric (I)
  my_geometric1_reactive <- callModule(module = geometric1_reactive, id = "geo1_react", tails = reactive(input$tails_geometric1),
                                       p = reactive(input$p_geometric1), x = reactive(input$x_geometric1), 
                                       a = reactive(input$a_geometric1), b = reactive(input$b_geometric1))
  
  my_geometric1_information_reactive <- callModule(module = geometric1_information_reactive, id = "geo1_info_react", tails = reactive(input$tails_geometric1),
                                                   p = reactive(input$p_geometric1), x = reactive(input$x_geometric1), 
                                                   a = reactive(input$a_geometric1), b = reactive(input$b_geometric1))
  
  output$dt_geometric1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric1_reactive())})
  output$plot_geometric1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric1_reactive())})
  output$distribution_geometric1 <- renderUI({my_geometric1_information_reactive()})
  output$probability_geometric1 <- renderUI({prob_representation (distribution_choice = "geometric1")})
  output$summary_geometric1 <- renderUI({summary_representation(distribution_choice = "geometric1", p_dist = input$p_geometric1)})
  
  
  #Geometric (II)
  my_geometric2_reactive <- callModule(module = geometric2_reactive, id = "geo2_react", tails = reactive(input$tails_geometric2),
                                       p = reactive(input$p_geometic2), x = reactive(input$x_geometric2), 
                                       a = reactive(input$a_geometric2), b = reactive(input$b_geometric2))
  
  my_geometric2_information_reactive <- callModule(module = geometric2_information_reactive, id = "geo2_info_react", tails = reactive(input$tails_geometric2),
                                                   p = reactive(input$p_geometic2), x = reactive(input$x_geometric2), 
                                                   a = reactive(input$a_geometric2), b = reactive(input$b_geometric2))
  
  output$dt_geometric2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric2_reactive())})
  output$plot_geometric2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric2_reactive())})
  output$distribution_geometric2 <- renderUI({my_geometric2_information_reactive()})
  output$probability_geometric2 <- renderUI({prob_representation (distribution_choice = "geometric2")})
  output$summary_geometric2 <- renderUI({summary_representation(distribution_choice = "geometric2", p_dist = input$p_geometic2)})
  
  
  #Negative Binomial (I)
  my_negativebinomial1_reactive <- callModule(module = negativebinomial1_reactive, id = "neg1_react", 
                                              tails = reactive(input$tails_negativebinomial1), n = reactive(input$r_negativebinomial1),
                                              p = reactive(input$p_negativebinomial1), x = reactive(input$x_negativebinomial1), 
                                              a = reactive(input$a_negativebinomial1), b = reactive(input$b_negativebinomial1))
  
  my_negativebinomial1_information_reactive <- callModule(module = negativebinomial1_information_reactive, id = "neg1_info_react", 
                                                          tails = reactive(input$tails_negativebinomial1), n = reactive(input$r_negativebinomial1),
                                                          p = reactive(input$p_negativebinomial1), x = reactive(input$x_negativebinomial1), 
                                                          a = reactive(input$a_negativebinomial1), b = reactive(input$b_negativebinomial1))
  
  output$dt_negativebinomial1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial1_reactive())})
  output$plot_negativebinomial1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial1_reactive())}) 
  output$distribution_negativebinomial1 <- renderUI({my_negativebinomial1_information_reactive()})
  output$probability_negativebinomial1 <- renderUI({prob_representation (distribution_choice = "negative1")})
  output$summary_negativebinomial1 <- renderUI({summary_representation(distribution_choice = "negative1",
                                                                       n_dist = input$r_negativebinomial1, p_dist = input$p_negativebinomial1)})
  
  #Negative Binomial (II)
  my_negativebinomial2_reactive <- callModule(module = negativebinomial2_reactive, id = "neg2_react",
                                              tails = reactive(input$tails_negativebinomial2), n = reactive(input$r_negativebinomial2), 
                                              p = reactive(input$p_negativebinomial2), x = reactive(input$x_negativebinomial2), 
                                              a = reactive(input$a_negativebinomial2), b = reactive(input$b_negativebinomial2))
  
  my_negativebinomial2_information_reactive <- callModule(module = negativebinomial2_information_reactive, id = "neg2_info_react",
                                                          tails = reactive(input$tails_negativebinomial2), n = reactive(input$r_negativebinomial2), 
                                                          p = reactive(input$p_negativebinomial2), x = reactive(input$x_negativebinomial2), 
                                                          a = reactive(input$a_negativebinomial2), b = reactive(input$b_negativebinomial2))
  
  output$dt_negativebinomial2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial2_reactive())})
  output$plot_negativebinomial2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial2_reactive())})
  output$distribution_negativebinomial2 <- renderUI({my_negativebinomial2_information_reactive()})
  output$probability_negativebinomial2 <- renderUI({prob_representation (distribution_choice = "negative2")})
  output$summary_negativebinomial2 <- renderUI({summary_representation(distribution_choice ="negative2", 
                                                                       n_dist = input$r_negativebinomial2, p_dist = input$p_negativebinomial2)})
  
  #Hyper-Geometric
  my_hypergeometric_reactive <- callModule(module = hypergeometric_reactive, id = "hyper_react", tails = reactive(input$tails_hypergeometric), 
                                           M = reactive(input$M_hypergeometric), N = reactive(input$N_hypergeometric-input$M_hypergeometric), 
                                           n = reactive(input$n_hypergeometric), x = reactive(input$x_hypergeometric), 
                                           a = reactive(input$a_hypergeometric), b = reactive(input$b_hypergeometric))
  
  my_hypergeometric_information_reactive <- callModule(module = hypergeometric_information_reactive, id = "hyper_info_react", tails = reactive(input$tails_hypergeometric), 
                                                       M = reactive(input$M_hypergeometric), N = reactive(input$N_hypergeometric-input$M_hypergeometric), 
                                                       n = reactive(input$n_hypergeometric), x = reactive(input$x_hypergeometric), 
                                                       a = reactive(input$a_hypergeometric), b = reactive(input$b_hypergeometric))
  
  output$dt_HyperGeometric <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_hypergeometric_reactive())})
  output$plot_HyperGeometric <- plotly::renderPlotly({discrete_plot(df_discrete = my_hypergeometric_reactive())})
  output$distribution_HyperGeometric <- renderUI({my_hypergeometric_information_reactive()})
  output$probability_HyperGeometric <- renderUI({prob_representation (distribution_choice = "hypergeometric")})
  output$summary_HyperGeometric <- renderUI({summary_representation(distribution_choice = "hypergeometric", n_dist = input$n_hypergeometric,
                                                                    M_dist = input$M_hypergeometric, N_dist = input$N_hypergeometric)})
  
  #Poisson
  my_poisson_reactive <- callModule(module = poisson_reactive, id = "poi_react", tails = reactive(input$tails_poisson), 
                                    lambda = reactive(input$lambda_poisson), x = reactive(input$x_poisson), 
                                    a = reactive(input$a_poisson), b = reactive(input$b_poisson))
  
  my_poisson_information_reactive <- callModule(module = poisson_information_reactive, id = "poi_info_react", tails = reactive(input$tails_poisson), 
                                                lambda = reactive(input$lambda_poisson), x = reactive(input$x_poisson), 
                                                a = reactive(input$a_poisson), b = reactive(input$b_poisson))
  
  
  output$dt_Poisson <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_poisson_reactive())})
  output$plot_Poisson <- plotly::renderPlotly({discrete_plot(df_discrete = my_poisson_reactive())})
  output$distribution_Poisson <- renderUI({my_poisson_information_reactive()})
  output$probability_Poisson <- renderUI({prob_representation (distribution_choice = "poisson")})
  output$summary_Poisson <- renderUI({summary_representation(distribution_choice = "poisson", lambda_dist = input$lambda_poisson)})
  
  
}
