sidebar <- dashboardSidebar(
  
  # Add a search option 
  sidebarSearchForm(
    textId = "searchtext",
    buttonId = "searchbutton",
    label = "Search",
    icon = icon("search")
  ),
  
  # Add the labels of the sidebar 
  sidebarMenu(
    id = "leftbar",
    
    # Presentation Page 
    menuItem(
      text = "Presentation",
      tabName = "presentation",
      icon = icon(name = "vector-square")
    ),
    
    # Raw Data 
    menuItem(
      text = "Raw Data",
      tabName = "rawdata",
      icon = icon(name = "table")
    ),
    
    # Descriptive Statistics options 
    menuItem(
      text = "General Inforamation",
      icon = icon(name = "calculator", class = "fas fa-calculator", 
                  lib = "font-awesome"),
      ##General Reports
      menuSubItem(
        text = "General reports",
        tabName = "GR",
        icon = icon("angle-right")),
      ##Descriptive Statistics 
      menuSubItem(
        text = "Descriptive Statistics",
        tabName = "DS",
        icon = icon("angle-right"))
    ),
    
    # Statistical Distributions options 
    menuItem(
      text = "Statistical Distributions",
      icon = icon(name = "chart-bar", class = "fas fa-chart-bar", 
                  lib = "font-awesome"),
      # Discrete Distributions 
      menuSubItem(
        text = "Discrete Random Variable",
        tabName = "SDiscrete",
        icon = icon("angle-right")),
      # Continous Distributions 
      menuSubItem(
        text = "Continous Random Variable",
        tabName = "SDcontinous",
        icon = icon("angle-right"))
    ),
    
    # Unsupervised learning options 
    menuItem(
      text = "Unsupervised Learning",
      icon = icon("brain"),
      # Clustering Methods
      menuSubItem(
        text = "Clustering Methods",
        tabName = "ULclustering",
        icon = icon("angle-right")),
      # Dimensional Reduction
      menuSubItem(
        text = "Dimensional reduction",
        tabName = "ULpca",
        icon = icon("angle-right"))
    ),
    
    # Supervised learning options 
    menuItem(
      text = "Supervised Learning",
      icon = icon("code-branch"),
      # Regression Methods
      menuSubItem(
        text = "Regression Methods",
        tabName = "SLregression",
        icon = icon("angle-right")),
      # K Nearest Neighbors
      menuSubItem(
        text = "K-Nearest Neighbors",
        tabName = "SLk",
        icon = icon("angle-right")),
      # Naive Bayes
      menuSubItem(
        text = "Naive Bayes",
        tabName = "SLbayes",
        icon = icon("angle-right")),
      # Classification Trees
      menuSubItem(
        text = "Classification Trees",
        tabName = "SLtrees",
        icon = icon("angle-right"))
    ),
    
    # Forcast options ---------------------------------------------------------
    menuItem(
      text = "Forecast",
      tabName = "forecast",
      icon = icon("chart-line")
    )
  )
)




