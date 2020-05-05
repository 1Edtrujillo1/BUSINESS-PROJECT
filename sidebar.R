sidebar <- dashboardSidebar(
  
  
  # Add a search option -----------------------------------------------------
  
  sidebarSearchForm(
    textId = "searchtext",
    buttonId = "searchbutton",
    label = "Search",
    icon = icon("search")
  ),
  
  
  # Add the labels of the sidebar -------------------------------------------
  
  sidebarMenu(
    
    id = "leftbar",
    
    
    # Presentation ------------------------------------------------------------
    
    menuItem(
      text = "Presentation",
      tabName = "presentation",
      icon = icon(name = "vector-square")
    ),
    
    
    # Descriptive Statistics options
    
    menuItem(
      text = "Descriptive Statistics",
      tabName = "DS",
      icon = icon(name = "calculator", class = "fas fa-calculator", lib = "font-awesome")
    ),
    
    # Statistical Distributions options
    
    menuItem(
      text = "Statistical Distributions",
      icon = icon(name = "chart-bar", class = "fas fa-chart-bar", lib = "font-awesome"),
      menuSubItem(
        text = "Discrete Random Variable",
        tabName = "SDiscrete",
        icon = icon("angle-right")),
      menuSubItem(
        text = "Continous Random Variable",
        tabName = "SDcontinous",
        icon = icon("angle-right"))
    ),
    
    # Unsupervised learning options
    
    menuItem(
      text = "Unsupervised Learning",
      icon = icon("brain"),
      menuSubItem(
        text = "Clustering Methods",
        tabName = "ULclustering",
        icon = icon("angle-right")),
      menuSubItem(
        text = "Dimensional reduction",
        tabName = "ULpca",
        icon = icon("angle-right"))
    ),
    
    # Supervised learning options
    
    menuItem(
      text = "Supervised Learning",
      icon = icon("code-branch"),
      menuSubItem(
        text = "Regression Methods",
        tabName = "SLregression",
        icon = icon("angle-right")),
      menuSubItem(
        text = "K-Nearest Neighbors",
        tabName = "SLk",
        icon = icon("angle-right")),
      menuSubItem(
        text = "Naive Bayes",
        tabName = "SLbayes",
        icon = icon("angle-right")),
      menuSubItem(
        text = "Classification Trees",
        tabName = "SLtrees",
        icon = icon("angle-right"))
    ),
    
    # Forcast options
    
    menuItem(
      text = "Forcast",
      tabName = "forcast",
      icon = icon("chart-line")
    )
  )
)

#21bc70


