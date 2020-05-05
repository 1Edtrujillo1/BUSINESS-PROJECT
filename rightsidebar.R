
rightsidebar <- rightSidebar(
  
  
  # Add the background of the right bar -------------------------------------
  
  background = "light",
  
  # Add the labels of the right bar -------------------------------------------
  
  ######################## Database Options ################################  
  
  rightSidebarTabContent(
    id = 1,
    title = "DataBases",
    icon = "database",
    active = FALSE, #Se pueda mover entre los temas
    sliderInput(
      "obs",
      "Number of observations:",
      min = 0, max = 1000, value = 500
    )
  ),
  
  ######################## Filters Options ################################
  
  rightSidebarTabContent(
    id = 2,
    title = "Parameters",
    icon = "filter",
    active = FALSE,
    
    ####### Statistical Distributions ##########################
    
    ### Discrete Distributions ##################
    
    conditionalPanel(condition = "input.leftbar == 'SDiscrete'",
                     selectInput(inputId = "discrete", label = "Distribution", 
                                 choices = c("Binomial", "Geometric", "Negative Binomial",
                                             "Hyper-Geometric", "Poisson"),
                                 multiple = FALSE),
                     
                     hr(), #Add a horizontal line for separation
                     
                     #Binomial
                     conditionalPanel(condition = "input.discrete == 'Binomial'",
                                      numericInput(inputId = "n_binomial", label =  "Number of trials \\(n\\):",
                                                   min = 0, step = 1, value = 20),
                                      hr(),
                                      numericInput(inputId = "p_binomial", label = "Probability of success \\(p\\):",
                                                   min = 0, max = 1, step = 0.01, value = 0.5),
                                      hr(),
                                      radioButtons(inputId = "tails_binomial", label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(condition = "input.tails_binomial == 'lower_tail' || input.tails_binomial == 'upper_tail'",
                                                       numericInput(inputId = "x_binomial", label = "\\(x\\):",
                                                                    min = 0, step = 1, value = 8)),
                                      conditionalPanel(condition = "input.tails_binomial == 'interval'",
                                                       numericInput(inputId = "a_binomial", label = "\\(a\\):",
                                                                    min = 0, step = 1, value = 8),
                                                       numericInput(inputId = "b_binomial", label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 0, step = 1, value = 8)
                                      )),
                     
                     #Geometric
                     conditionalPanel(condition = "input.discrete ==  'Geometric'",
                                      selectInput(inputId = "geometric", label = NULL,
                                                  choices = c("Geometric (I)" = "geometric1", "Geometric(II)" = "geometric2")),
                                      hr(),
                                      ##Geometric (I)
                                      conditionalPanel(condition = "input.geometric == 'geometric1'",
                                                       numericInput(inputId = "p_geometric1", label = "Probability of success \\(p\\):",
                                                                    min = 0, max = 1, step = 0.01, value = 0.5),
                                                       hr(),
                                                       radioButtons(inputId = "tails_geometric1", label = NULL,
                                                                    choices = c(
                                                                      "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                                      "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                                      "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                                    )),
                                                       hr(),
                                                       conditionalPanel(condition = "input.tails_geometric1 == 'lower_tail' || input.tails_geometric1 == 'upper_tail'",
                                                                        helpText("Number of failures before the \\(1^{st}\\) success"), #Add additional text
                                                                        numericInput(inputId = "x_geometric1", label = "\\(x\\):",
                                                                                     min = 0, step = 1, value = 1)),
                                                       conditionalPanel(condition = "input.tails_geometric1 == 'interval'",
                                                                        helpText("Number of failures before the \\(1^{st}\\) success"),
                                                                        numericInput(inputId = "a_geometric1", label = "\\(a\\):",
                                                                                     min = 0, step = 1, value = 1),
                                                                        numericInput(inputId = "b_geometric1", label = "\\(b:\\ (a \\leq b) \\)",
                                                                                     min = 0, step = 1, value = 1))
                                      ),
                                      ##Geometric (II)
                                      conditionalPanel(condition = "input.geometric == 'geometric2'",
                                                       numericInput(inputId = "p_geometic2", label = "Probability of success \\(p\\):",
                                                                    min = 0, max = 1, step = 0.01, value = 0.5),
                                                       hr(),
                                                       radioButtons(inputId = "tails_geometric2", label = NULL,
                                                                    choices = c(
                                                                      "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                                      "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                                      "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                                    )),
                                                       hr(),
                                                       conditionalPanel(condition = "input.tails_geometric2 == 'lower_tail' || input.tails_geometric2 == 'upper_tail'",
                                                                        helpText("The trial on which the \\(1^{st}\\) success occurs"),
                                                                        numericInput(inputId = "x_geometric2", label = "\\(x\\):",
                                                                                     min = 1, step = 1, value = 2)),
                                                       conditionalPanel(condition = "input.tails_geometric2 == 'interval'",
                                                                        helpText("The trial on which the \\(1^{st}\\) success occurs"),
                                                                        numericInput(inputId = "a_geometric2", label = "\\(a\\):",
                                                                                     min = 1, step = 1, value = 2),
                                                                        numericInput(inputId = "b_geometric2", label = "\\(b:\\ (a \\leq b) \\)",
                                                                                     min = 1, step = 1, value = 2))
                                      )
                     ),
                     
                     #Negative Binomial
                     conditionalPanel(condition = "input.discrete == 'Negative Binomial'",
                                      selectInput(inputId = "negativebinomial", label = NULL,
                                                  choices = c("Negative Binomial (I)" = "negativebinomial1", "Negative Binomial (II)" = "negativebinomial2")),
                                      hr(),
                                      #Negative Binomial (I)
                                      conditionalPanel(condition = "input.negativebinomial == 'negativebinomial1'",
                                                       numericInput(inputId = "r_negativebinomial1", label = "Number of successes \\(r\\):",
                                                                    min = 1, step = 1, value = 5),
                                                       hr(),
                                                       numericInput(inputId = "p_negativebinomial1", label = "Probability of success \\(p\\):",
                                                                    min = 0, max = 1, step = 0.01, value = 0.5),
                                                       hr(),
                                                       radioButtons(inputId = "tails_negativebinomial1", label = NULL,
                                                                    choices = c(
                                                                      "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                                      "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                                      "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                                    )),
                                                       hr(),
                                                       conditionalPanel(condition = "input.tails_negativebinomial1 == 'lower_tail' || input.tails_negativebinomial1 == 'upper_tail'",
                                                                        helpText("Number of failures before the \\(r^{th}\\) success"),
                                                                        numericInput(inputId = "x_negativebinomial1", label = "\\(x\\):",
                                                                                     min = 0, step = 1, value = 2)),
                                                       conditionalPanel(condition = "input.tails_negativebinomial1 == 'interval'",
                                                                        helpText("Number of failures before the \\(r^{th}\\) success"),
                                                                        numericInput(inputId = "a_negativebinomial1", label = "\\(a\\):",
                                                                                     min = 0, step = 1, value = 2),
                                                                        numericInput(inputId = "b_negativebinomial1", label = "\\(b:\\ (a \\leq b) \\)",
                                                                                     min = 0, step = 1, value = 2))              
                                      ),
                                      #Negative Binomial (II)
                                      conditionalPanel(condition = "input.negativebinomial == 'negativebinomial2'",
                                                       numericInput(inputId = "r_negativebinomial2", label = "Number of successes \\(r\\):",
                                                                    min = 1, step = 1, value = 5),
                                                       hr(),
                                                       numericInput(inputId = "p_negativebinomial2", label = "Probability of success \\(p\\):",
                                                                    min = 0, max = 1, step = 0.01, value = 0.5),
                                                       hr(),
                                                       radioButtons(inputId = "tails_negativebinomial2", label = NULL,
                                                                    choices = c(
                                                                      "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                                      "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                                      "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                                    )),
                                                       hr(),
                                                       conditionalPanel(condition = "input.tails_negativebinomial2 == 'lower_tail' || input.tails_negativebinomial2 == 'upper_tail'",
                                                                        helpText("The trial on which the \\(r^{th}\\) success occurs"),
                                                                        numericInput(inputId = "x_negativebinomial2", label = "\\(x\\): \\( (x \\geq r) \\)",
                                                                                     min = 1, step = 1, value = 6)),
                                                       conditionalPanel(condition = "input.tails_negativebinomial2 == 'interval'",
                                                                        helpText("The trial on which the \\(r^{th}\\) success occurs"),
                                                                        numericInput(inputId = "a_negativebinomial2", label = "\\(a\\): \\( (a \\geq r) \\)",
                                                                                     min = 1, step = 1, value = 6),
                                                                        numericInput(inputId = "b_negativebinomial2", label = "\\(b\\): \\( (b \\geq r) \\)",
                                                                                     min = 1, step = 1, value = 6))
                                      )
                     ),
                     
                     #Hyper-Geometric
                     conditionalPanel(condition = "input.discrete == 'Hyper-Geometric'",
                                      numericInput(inputId = "n_hypergeometric", label = "Sample size \\(n\\):",
                                                   min = 0, step = 1, value = 100),
                                      hr(),
                                      numericInput(inputId = "N_hypergeometric", label = "Total number of objects \\(N\\):",
                                                   min = 0, step = 1, value = 500),
                                      hr(),
                                      numericInput(inputId = "M_hypergeometric", label = "Number of successes \\(M\\):",
                                                   min = 0, step = 1, value = 50),
                                      hr(),
                                      radioButtons(inputId = "tails_hypergeometric", label = NULL,
                                                   choices = c(
                                                     "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                     "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                     "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                   )),
                                      hr(),
                                      conditionalPanel(condition = "input.tails_hypergeometric == 'lower_tail' || input.tails_hypergeometric == 'upper_tail'",
                                                       numericInput(inputId = "x_hypergeometric", label = "\\(x\\):",
                                                                    min = 0, step = 1, value = 8)),
                                      conditionalPanel(condition = "input.tails_hypergeometric == 'interval'",
                                                       numericInput(inputId = "a_hypergeometric", label = "\\(a\\):",
                                                                    min = 0, step = 1, value = 8),
                                                       numericInput(inputId = "b_hypergeometric", label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 0, step = 1, value = 8))
                     ),
                     
                     #Poisson
                     conditionalPanel(condition = "input.discrete == 'Poisson'",
                                      numericInput(inputId = "lambda_poisson", label = "Rate \\(\\lambda\\):",
                                                   min = 1, step = 1, value = 4),
                                      hr(),
                                      radioButtons(inputId = "tails_poisson", label = NULL,
                                                        choices = c(
                                                          "Lower tail: \\(P(X \\leq x)\\)" = "lower_tail",
                                                          "Upper tail: \\(P(X > x)\\)" = "upper_tail",
                                                          "Interval: \\(P(a \\leq X \\leq b)\\)" = "interval"
                                                        )),
                                      hr(),
                                      conditionalPanel(condition = "input.tails_poisson == 'lower_tail' || input.tails_poisson == 'upper_tail'",
                                                       numericInput(inputId = "x_poisson", label = "\\(x\\):",
                                                                    min = 0, step = 1, value = 6)),
                                      conditionalPanel(condition = "input.tails_poisson == 'interval'",
                                                       numericInput(inputId = "a_poisson", label = "\\(a\\):",
                                                                    min = 0, step = 1, value = 6),
                                                       numericInput(inputId = "b_poisson", label = "\\(b:\\ (a \\leq b) \\)",
                                                                    min = 0, step = 1, value = 6))
                     )
                     
                     
                     
                     
    ),
    
    ### Continous Distributions ##################
    
    conditionalPanel(condition = "input.leftbar == 'SDcontinous'",
                     selectInput(inputId = "continous", label = "Distribution",
                                 choices = c("Exponential", "Normal", "Gamma", 
                                             "Log-Normal", "Beta", "Weibull", "Chi-Square"),
                                 multiple = FALSE)
    )
    ####### Statistical Distributions ##########################
    
    
  )
)

