body <- dashboardBody(
  
  includeCSS("INFORMATION/STYLE.css"),
  
  withMathJax(), #Allow math latex formulas in the body
  
  # Add the theme to the paper ----------------------------------------------
  dashboardthemes:: shinyDashboardThemes(
    theme = "grey_dark"
  ),
  
  # Taggle sidebar information ----------------------------------------------
  tabItems(
    
    # Presentation Page -------------------------------------------------------
    tabItem(
      tabName = "presentation",
      HomemenuInput_body("menu")
    ),
    # Raw Data ----------------------------------------------------------------
    tabItem(
      tabName = "rawdata",
      raw_dataInput_body("raw_data")
    ),
    # General Information -----------------------------------------------------
    # # General Report --------------------------------------------------------
    tabItem(
      tabName = "GR",
      reportInput_body("pull_report")
    ),
    # # Descriptive Statistics ------------------------------------------------
    tabItem(
      tabName = "DS",
      "Modify 2"
    ),
    # Statistical Distributions -----------------------------------------------
    # # Discrete Distributions ------------------------------------------------
    tabItem(
      tabName = "SDiscrete",
      discrete_distInput_body("discrete_distributions")
    ),
    # # Continous Distributions -----------------------------------------------
    tabItem(tabName = "SDcontinous",
            "Modify2.1"
    ),
    
    # Unsupervised Learning ---------------------------------------------------
    tabItem(tabName = "ULclustering",
            "Modify3"
    ),
    tabItem(tabName = "ULpca",
            "Modify3.1"
    ),
    # Supervised Learning -----------------------------------------------------
    tabItem(tabName = "SLregression",
            "Modify4"
    ),
    tabItem(tabName = "SLk",
            "Modify4.1"
    ),
    tabItem(tabName = "SLbayes",
            "Modify4.2"
    ),
    tabItem(tabName = "SLtrees",
            "Modify4.3"
    ),
    # Forecast ----------------------------------------------------------------
    tabItem(tabName = "forecast",
            "Modify5"
    )
    
  )
  
)


