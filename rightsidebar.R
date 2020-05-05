
rightsidebar <- rightSidebar(
  

# Add the background of the right bar -------------------------------------

  background = "light",

# Add the labels of the right bar -------------------------------------------

  # Database Options  
  
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

  # Filters Options

  rightSidebarTabContent(
    id = 2,
    title = "Parameters",
    icon = "filter",
    active = FALSE,
    
    ## Statistical Distributions
    
    ### Discrete Distributions
    
    conditionalPanel(
      condition = "input.leftbar == 'SDiscrete'",
      selectInput(inputId = "discrete", label = "Distribution", 
                  choices = c("Binomial", "Geometric (I)", "Geometric (II)", 
                              "Negative Binomial (I)", "Negative Binomial (II)",
                              "Hyper-Geometric", "Poisson"),
                  multiple = FALSE),
      
      hr(), #Add a horizontal line for separation
      
      #Binomial
      
      conditionalPanel(
        condition = "input.discrete == 'Binomial'",
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
        conditionalPanel(
          condition = "input.tails_binomial == 'lower_tail' || input.tails_binomial == 'upper_tail'",
          numericInput(inputId = "x_binomial", label = "\\(x\\)",
                       min = 0, step = 1, value = 8)),
        conditionalPanel(
          condition = "input.tails_binomial == 'interval'",
          numericInput(inputId = "a_binomial", label = "\\(a\\)",
                       min = 0, step = 1, value = 8),
          numericInput(inputId = "b_binomial", label = "\\(b:\\ (a \\leq b) \\)",
                       min = 0, step = 1, value = 8)
        ))
      
      
      
      
    ),
    
    ### Continous Distributions
    
    conditionalPanel(
      condition = "input.leftbar == 'SDcontinous'",
      selectInput(inputId = "continous", label = "Distribution",
                  choices = c("Exponential", "Normal Distribution", "Gamma", 
                              "Log-Normal", "Beta", "Weibull", "Chi-Square"),
                  multiple = FALSE)
    )
    
  )
)

