body <- dashboardBody(
  
  includeCSS("INFORMATION/STYLE.css"),
  
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
    
    ####### Statistical Distributions ##########################
    
    ### Discrete Distributions ##################
    
    tabItem(tabName = "SDiscrete",
            #Binomial
            conditionalPanel(condition = "input.discrete == 'Binomial'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_Binomial")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_Binomial"))
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
                                       uiOutput("distribution_Binomial")),
                                     accordionItem(
                                       id = 5,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_Binomial")),
                                     accordionItem(
                                       id = 6,
                                       title = "Summary",
                                       color = "info",
                                       collapsed = TRUE,
                                       uiOutput("summary_Binomial"))
                                   ))))
            ),
            #Geometric (I)
            conditionalPanel(condition = "input.discrete == 'Geometric' && input.geometric == 'geometric1'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_geometric1")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_geometric1"))
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
                                       uiOutput("distribution_geometric1")),
                                     accordionItem(
                                       id = 8,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_geometric1")),
                                     accordionItem(
                                       id = 9,
                                       title = "Summary",
                                       color = "info",
                                       collapsed = TRUE,
                                       uiOutput("summary_geometric1"))
                                   ))))
            ),
            #Geometric (II)
            conditionalPanel(condition = "input.discrete == 'Geometric' && input.geometric == 'geometric2'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_geometric2")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_geometric2"))
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
                                       uiOutput("distribution_geometric2")),
                                     accordionItem(
                                       id = 11,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_geometric2")),
                                     accordionItem(
                                       id = 12,
                                       title = "Summary",
                                       color = "info",
                                       collapsed = TRUE,
                                       uiOutput("summary_geometric2"))
                                   ))))
            ),
            #Negative Binomial (I)
            conditionalPanel(condition = "input.discrete == 'Negative Binomial' && input.negativebinomial == 'negativebinomial1'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_negativebinomial1")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_negativebinomial1"))
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
                                       uiOutput("distribution_negativebinomial1")),
                                     accordionItem(
                                       id = 14,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_negativebinomial1")),
                                     accordionItem(
                                       id = 15,
                                       title = "Summary",
                                       color = "info",
                                       collapsed = TRUE,
                                       uiOutput("summary_negativebinomial1"))
                                   ))))
            ),
            #Negative Binomial (II)
            conditionalPanel(condition = "input.discrete == 'Negative Binomial' && input.negativebinomial == 'negativebinomial2'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_negativebinomial2")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_negativebinomial2"))
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
                                       uiOutput("distribution_negativebinomial2")),
                                     accordionItem(
                                       id = 17,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_negativebinomial2")),
                                     accordionItem(
                                       id = 18,
                                       title = "Summary",
                                       color = "info",
                                       uiOutput("summary_negativebinomial2"))
                                   ))))
            ),
            #Hyper-Geometric
            conditionalPanel(condition = "input.discrete == 'Hyper-Geometric'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_HyperGeometric")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_HyperGeometric"))
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
                                       uiOutput("distribution_HyperGeometric")),
                                     accordionItem(
                                       id = 20,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_HyperGeometric")),
                                     accordionItem(
                                       id = 21,
                                       title = "Summary",
                                       color = "info",
                                       uiOutput("summary_HyperGeometric"))
                                   ))))
            ),
            #Poisson
            conditionalPanel(condition = "input.discrete == 'Poisson'",
                             tabsetPanel(
                               tabPanel(title = "Dataset",
                                        DT::DTOutput("dt_Poisson")),
                               tabPanel(title = "Plot",
                                        plotly::plotlyOutput("plot_Poisson"))
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
                                       uiOutput("distribution_Poisson")),
                                     accordionItem(
                                       id = 23,
                                       title = "Probability Function",
                                       color = "warning",
                                       collapsed = TRUE,
                                       uiOutput("probability_Poisson")),
                                     accordionItem(
                                       id = 24,
                                       title = "Summary",
                                       color = "info",
                                       collapsed = TRUE,
                                       uiOutput("summary_Poisson"))
                                   ))))
            )
            
    ),
    
    ### Continous Distributions ##################
    
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


