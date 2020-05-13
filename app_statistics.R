
library(purrr)

map(c("shiny", "shinydashboard", "dashboardthemes", "shinydashboardPlus", "shinyWidgets", "mixdist","DT",
      "jsonlite", "data.table", "dplyr", "bit64", "stringr", "scales", "ggplot2", "plotly"), 
    require, character.only = TRUE)

map(c("header.R", "sidebar.R", "body.R", "rightsidebar.R",
      "INFORMATION/utils.R", "INFORMATION/reactive_expressions.R"), 
    source)

# Define the UI -----------------------------------------------------------

ui <- dashboardPagePlus(header = header,  sidebar = sidebar, 
                        body = body, rightsidebar = rightsidebar)

# Define server -----------------------------------------------------------

server <- function(input, output) {
  
  # Create a Dynamic notifications for the header
  
  output$notificationMenu <- renderMenu({
    
    ntfcation <- apply(notifications_list(path = "INFORMATION/notifications.json"), 1,
                       function(row){
                         notificationItem(text = row[["TEXT"]],
                                          href = row[["HREF"]],
                                          status = row[["STATUS"]],
                                          icon = icon(name = row[["ICON"]]))
                       })
    dropdownMenu(type = "notifications", .list = ntfcation)
    
  })
  
  ####### Statistical Distributions ##########################
  
  ### Discrete Distributions ##################
  #Binomial
  my_binomial_reactive <- callModule(module = binomial_reactive, id = "bin_react",
                                     tails = reactive(input$tails_binomial), n = reactive(input$n_binomial), 
                                     p = reactive(input$p_binomial), x = reactive(input$x_binomial), 
                                     a = reactive(input$a_binomial), b = reactive(input$b_binomial))
  
  my_binomial_information_reactive <- callModule(module = binomial_information_reactive, id = "bin_info_react", 
                                                 tails = reactive(input$tails_binomial), n = reactive(input$n_binomial), 
                                                 p = reactive(input$p_binomial), x = reactive(input$x_binomial), 
                                                 a = reactive(input$a_binomial), b = reactive(input$b_binomial))
  
  output$dt_Binomial <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_binomial_reactive())})
  output$plot_Binomial <- plotly::renderPlotly({discrete_plot(df_discrete = my_binomial_reactive())})
  output$distribution_Binomial <- renderUI({my_binomial_information_reactive()})
  
  output$probability_Binomial <- renderUI({
    withMathJax(
      helpText("Probability density function: $$ f(x) = P(X = x) = \\binom{n}{x}p^x(1-p)^{n-x}$$"),
      br(),
      helpText("where \\( x = 0, 1, \\dots, n\\) and \\( 0 \\leq p \\leq 1 \\)")
    )})
  output$summary_Binomial <- renderUI({
    withMathJax(
      helpText("\\(\\mu = E(X) = np = \\)", round(input$n_binomial * input$p_binomial, 3)),
      br(),
      helpText("\\(\\sigma = SD(X) = \\sqrt{np(1-p)} = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 3)),
      br(),
      helpText("\\(\\sigma^2 = Var(X) = np(1-p) = \\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 3))
    )
  })
  
  
  #Geometric (I)
  my_geometric1_reactive <- callModule(module = geometric1_reactive, id = "geo1_react", tails = reactive(input$tails_geometric1),
                                       p = reactive(input$p_geometric1), x = reactive(input$x_geometric1), 
                                       a = reactive(input$a_geometric1), b = reactive(input$b_geometric1))
  
  output$dt_geometric1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric1_reactive())})
  output$plot_geometric1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric1_reactive())})
  
  #Geometric (II)
  my_geometric2_reactive <- callModule(module = geometric2_reactive, id = "geo2_react", tails = reactive(input$tails_geometric2),
                                       p = reactive(input$p_geometic2), x = reactive(input$x_geometric2), 
                                       a = reactive(input$a_geometric2), b = reactive(input$b_geometric2))
  
  output$dt_geometric2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric2_reactive())})
  output$plot_geometric2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric2_reactive())})
  
  #Negative Binomial (I)
  my_negativebinomial1_reactive <- callModule(module = negativebinomial1_reactive, id = "neg1_react", 
                                              tails = reactive(input$tails_negativebinomial1), n = reactive(input$r_negativebinomial1),
                                              p = reactive(input$p_negativebinomial1), x = reactive(input$x_negativebinomial1), 
                                              a = reactive(input$a_negativebinomial1), b = reactive(input$b_negativebinomial1))
  
  output$dt_negativebinomial1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial1_reactive())})
  output$plot_negativebinomial1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial1_reactive())}) 
  
  #Negative Binomial (II)
  my_negativebinomial2_reactive <- callModule(module = negativebinomial2_reactive, id = "neg2_react",
                                              tails = reactive(input$tails_negativebinomial2), n = reactive(input$r_negativebinomial2), 
                                              p = reactive(input$p_negativebinomial2), x = reactive(input$x_negativebinomial2), 
                                              a = reactive(input$a_negativebinomial2), b = reactive(input$b_negativebinomial2))
  
  output$dt_negativebinomial2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial2_reactive())})
  output$plot_negativebinomial2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial2_reactive())})
  
  #Hyper-Geometric
  my_hypergeometric_reactive <- callModule(module = hypergeometric_reactive, id = "hyper_react", tails = reactive(input$tails_hypergeometric), 
                                           M = reactive(input$M_hypergeometric), N = reactive(input$N_hypergeometric-input$M_hypergeometric), 
                                           n = reactive(input$n_hypergeometric), x = reactive(input$x_hypergeometric), 
                                           a = reactive(input$a_hypergeometric), b = reactive(input$b_hypergeometric))
  
  output$dt_HyperGeometric <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_hypergeometric_reactive())})
  output$plot_HyperGeometric <- plotly::renderPlotly({discrete_plot(df_discrete = my_hypergeometric_reactive())})
  
  #Poisson
  my_poisson_reactive <- callModule(module = poisson_reactive, id = "poi_react", tails = reactive(input$tails_poisson), 
                                    lambda = reactive(input$lambda_poisson), x = reactive(input$x_poisson), 
                                    a = reactive(input$a_poisson), b = reactive(input$b_poisson))
  
  output$dt_Poisson <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_poisson_reactive())})
  output$plot_Poisson <- plotly::renderPlotly({discrete_plot(df_discrete = my_poisson_reactive())})
  
  
  
  
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
