
rightsidebar <- rightSidebar(
  
  # Add the background of the right bar 
  background = "light",
  
  # Add the labels of the right bar 
  
  # Database Options (RIGHT)
  rightSidebarTabContent(
    id = 1,
    title = "DataBases",
    icon = "database",
    active = FALSE, # move between themes
    
    # Raw Data 
    conditionalPanel(condition = "input.leftbar == 'rawdata'",
                     raw_dataInput_rightsidebar("raw_data")
    ),
    # General Information 
    ## General Report 
    conditionalPanel(condition = "input.leftbar == 'GR'",
                     pull_dataInput_rightsidebar(id = "pull_report") 
    ),
    ## Descriptive Statistics 
    conditionalPanel(condition = "input.leftbar == 'DS'",
                     pull_dataInput_rightsidebar(id = "pull_desc_stats") 
    )
    
  ),
  
  # Filter Options (LEFT)
  rightSidebarTabContent(
    id = 2,
    title = "Parameters",
    icon = "filter",
    active = FALSE,
    
    # General Information 
    ## General Report 
    conditionalPanel(condition = "input.leftbar == 'GR'",
                     reportInput_rightsidebar(id = "pull_report")
    ),
    ## Descriptive Statistics 
    conditionalPanel(condition = "input.leftbar == 'DS'",
                     descriptStatsInput_rightsidebar(id = "pull_desc_stats")
    ),
    
    # Statistical Distributions 
    # # Discrete Distributions 
    conditionalPanel(condition = "input.leftbar == 'SDiscrete'",
                     distributionInput_rightsidebar(id = "discrete_distributions")
    ),
    # # Continous Distributions 
    conditionalPanel(condition = "input.leftbar == 'SDcontinous'",
                     distributionInput_rightsidebar(id = "continous_distributions")
    )
    
  )
  
)

