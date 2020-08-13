
rightsidebar <- rightSidebar(
  
  # Add the background of the right bar -------------------------------------
  background = "light",
  
  # Add the labels of the right bar -------------------------------------------
  
  # Database Options --------------------------------------------------------
  rightSidebarTabContent(
    id = 1,
    title = "DataBases",
    icon = "database",
    active = FALSE, #Se pueda mover entre los temas
    
    # Raw Data ----------------------------------------------------------------
    conditionalPanel(condition = "input.leftbar == 'rawdata'",
                     raw_dataInput_rightsidebar("raw_data")
    ),
    # General Information -----------------------------------------------------
    ## General Report ---------------------------------------------------------
    conditionalPanel(condition = "input.leftbar == 'GR'",
                     pull_dataInput_rightsidebar(id = "pull_report") 
    )
    # # Descriptive Statistics ------------------------------------------------
    
    
  ),
  
  # Filter Options ----------------------------------------------------------
  rightSidebarTabContent(
    id = 2,
    title = "Parameters",
    icon = "filter",
    active = FALSE,
    
    # General Information -----------------------------------------------------
    ## General Report ---------------------------------------------------------
    conditionalPanel(condition = "input.leftbar == 'GR'",
                     reportInput_rightsidebar(id = "pull_report")
    ),
    # # Descriptive Statistics ------------------------------------------------
    
    
    # Statistical Distributions ------------------------------------------------
    # # Discrete Distributions ------------------------------------------------
    conditionalPanel(condition = "input.leftbar == 'SDiscrete'",
                     discrete_distInput_rightsidebar("discrete_distributions"),
    ),
    # # Continous Distributions -----------------------------------------------
    conditionalPanel(condition = "input.leftbar == 'SDcontinous'",
                     selectInput(inputId = "continous", label = "Distribution",
                                 choices = c("Exponential", "Normal", "Gamma", 
                                             "Log-Normal", "Beta", "Weibull", "Chi-Square"),
                                 multiple = FALSE)
    )
  )
  
)

