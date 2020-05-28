body <- dashboardBody(
  
  includeCSS("INFORMATION/STYLE.css"),
  
  withMathJax(), #Allow math latex formulas in the body
  
  # Add the theme to the paper ----------------------------------------------
  dashboardthemes:: shinyDashboardThemes(
    theme = "grey_dark"
  ),
  
  # Taggle sidebar information ----------------------------------------------
  
  tabItems(
    
    # Presentation Page
    tabItem(
      tabName = "presentation",
      HomemenuInput_body("menu")
    ),
    
    # Raw Data
    tabItem(
      tabName = "rawdata",
      raw_dataInput_body("raw_data")
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


