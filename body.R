body <- dashboardBody(
  
  withMathJax(),
  
  # Add the theme to the paper ----------------------------------------------
  
  dashboardthemes:: shinyDashboardThemes(
    theme = "grey_dark"
  ),
  
  
  # Add boxes for the home header options -----------------------------------
  
  conditionalPanel(
    condition = "input.basemenu == 'Home' && input.leftbar == 'presentation'",
    fluidRow(
      column(
        width = 6,
        offset = 3,
        boxPlus(
          title = "Home",
          closable = FALSE,
          collapsible = TRUE,
          width = NULL,
          status = "danger",
          solidHeader = TRUE,
          enable_dropdown = TRUE,
          dropdown_icon = "info",
          dropdown_menu = dropdownItemList(
            dropdownItem(url = "https://trello.com/jorgeeduardotrujillovelazquez/boards", name = "trello"),
            dropdownDivider(), #create a line space
            dropdownItem(url = "#", name = "item1"),
            dropdownItem(url = "#", name = "tem2"),
          ),
          box(
            width = NULL,
            title = "Pages Information",
            status = NULL,
            socialButton(
              url = "https://trello.com/",
              type = "trello"
            ),
            socialButton(
              url = "http://dropbox.com",
              type = "dropbox"
            ),
            socialButton(
              url = "http://github.com",
              type = "github"
            )
          ))
      ))),
  
  conditionalPanel(
    condition = "input.basemenu == 'Personal Information' && input.leftbar == 'presentation'",
    fluidRow(
      column(
        width = 6,
        offset = 3,# to aign to center
        shinydashboardPlus::flipBox(
          id = 3,
          main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
          header_img = "https://image.flaticon.com/icons/svg/119/119598.svg",
          front_title = "Eduardo Trujillo",
          back_title = "About Eduardo",
          hr(), #Line space
          "More than 2 years of experience in Data Science Areas",
          back_content = tagList(
            column(
              width = 6,
              offset = 3,# to aign to center
              "SUPER"))
        )
      ))),
  
  conditionalPanel(
    condition = "input.basemenu == 'Additional Information' && input.leftbar == 'presentation'",
    fluidRow(
      column(
        width = 6,
        offset = 3,
        box(
          title = "Additional Information",
          status = "primary"
      )
      ))),
  
  # Taggle sidebar information ----------------------------------------------
  
  tabItems(
    
    # Presentation Page
    
    tabItem(
      tabName = "presentation",
      "Presentation of the Page"
    ),
    
    # Descriptive Statistics
    
    tabItem(tabName = "DS",
            "Modify1"
    ),
    
    # Statistical Distributions 
    
    tabItem(tabName = "SDiscrete",
            "Modify2"
    ),
    tabItem(tabName = "SDcontinous",
            "Modify2.1"
    ),
    
    # Unsupervised Learning
    
    tabItem(tabName = "ULclustering",
            "Modify3"
    ),
    tabItem(tabName = "ULpca",
            "Modify3.1"
    ),
    
    # Supervised Learning
    
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
    
    # Forcast
    
    tabItem(tabName = "forcast",
            "Modify5"
    )
    
  )
  
)


