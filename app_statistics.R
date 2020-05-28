
library(purrr)

map(c("shiny", "shinydashboard", "dashboardthemes", "shinydashboardPlus", "shinyWidgets", "mixdist","DT",
      "jsonlite", "data.table", "dplyr", "bit64", "stringr", "scales", "ggplot2", "plotly",
      "haven", "readxl", "lubridate"), 
    require, character.only = TRUE)

source("INFORMATION/1.MENU/reactive_menu.R")

map(c("INFORMATION/2.RAW_DATA/raw_data.R", "INFORMATION/2.RAW_DATA/reactive_raw_data.R", 
      "INFORMATION/2.RAW_DATA/additional_raw_data.R"),
    source)

map(c("header.R", "sidebar.R", "body.R", "rightsidebar.R",
      "INFORMATION/utils.R", "INFORMATION/reactive_expressions.R"), 
    source)

# Define the UI -----------------------------------------------------------

ui <- dashboardPagePlus(header = header,  sidebar = sidebar, 
                        body = body, rightsidebar = rightsidebar)

# Define server -----------------------------------------------------------

server <- function(input, output) {
  
  options(shiny.maxRequestSize=100*1024^2) #accept files of MB bigger
  
  # Sever of the header -----------------------------------------------------
  notifications <- callModule(module = NotificationmenuOutput, id = "notifications")
  menu <- callModule(module = HomemenuOutput, id = "menu")
  # Server of the Raw Data --------------------------------------------------
  raw_data <- callModule(module = raw_dataOutput, id = "raw_data")
  
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
  output$probability_Binomial <- renderUI({prob_representation (distribution_choice = "binomial")})
  output$summary_Binomial <- renderUI({summary_representation(distribution_choice = "binomial", n_dist = input$n_binomial, p_dist = input$p_binomial)})
  
  #Geometric (I)
  my_geometric1_reactive <- callModule(module = geometric1_reactive, id = "geo1_react", tails = reactive(input$tails_geometric1),
                                       p = reactive(input$p_geometric1), x = reactive(input$x_geometric1), 
                                       a = reactive(input$a_geometric1), b = reactive(input$b_geometric1))
  
  my_geometric1_information_reactive <- callModule(module = geometric1_information_reactive, id = "geo1_info_react", tails = reactive(input$tails_geometric1),
                                                   p = reactive(input$p_geometric1), x = reactive(input$x_geometric1), 
                                                   a = reactive(input$a_geometric1), b = reactive(input$b_geometric1))
  
  output$dt_geometric1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric1_reactive())})
  output$plot_geometric1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric1_reactive())})
  output$distribution_geometric1 <- renderUI({my_geometric1_information_reactive()})
  output$probability_geometric1 <- renderUI({prob_representation (distribution_choice = "geometric1")})
  output$summary_geometric1 <- renderUI({summary_representation(distribution_choice = "geometric1", p_dist = input$p_geometric1)})
  
  #Geometric (II)
  my_geometric2_reactive <- callModule(module = geometric2_reactive, id = "geo2_react", tails = reactive(input$tails_geometric2),
                                       p = reactive(input$p_geometic2), x = reactive(input$x_geometric2), 
                                       a = reactive(input$a_geometric2), b = reactive(input$b_geometric2))
  
  my_geometric2_information_reactive <- callModule(module = geometric2_information_reactive, id = "geo2_info_react", tails = reactive(input$tails_geometric2),
                                                   p = reactive(input$p_geometic2), x = reactive(input$x_geometric2), 
                                                   a = reactive(input$a_geometric2), b = reactive(input$b_geometric2))
  
  output$dt_geometric2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_geometric2_reactive())})
  output$plot_geometric2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_geometric2_reactive())})
  output$distribution_geometric2 <- renderUI({my_geometric2_information_reactive()})
  output$probability_geometric2 <- renderUI({prob_representation (distribution_choice = "geometric2")})
  output$summary_geometric2 <- renderUI({summary_representation(distribution_choice = "geometric2", p_dist = input$p_geometic2)})
  
  
  #Negative Binomial (I)
  my_negativebinomial1_reactive <- callModule(module = negativebinomial1_reactive, id = "neg1_react", 
                                              tails = reactive(input$tails_negativebinomial1), n = reactive(input$r_negativebinomial1),
                                              p = reactive(input$p_negativebinomial1), x = reactive(input$x_negativebinomial1), 
                                              a = reactive(input$a_negativebinomial1), b = reactive(input$b_negativebinomial1))
  
  my_negativebinomial1_information_reactive <- callModule(module = negativebinomial1_information_reactive, id = "neg1_info_react", 
                                                          tails = reactive(input$tails_negativebinomial1), n = reactive(input$r_negativebinomial1),
                                                          p = reactive(input$p_negativebinomial1), x = reactive(input$x_negativebinomial1), 
                                                          a = reactive(input$a_negativebinomial1), b = reactive(input$b_negativebinomial1))
  
  output$dt_negativebinomial1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial1_reactive())})
  output$plot_negativebinomial1 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial1_reactive())}) 
  output$distribution_negativebinomial1 <- renderUI({my_negativebinomial1_information_reactive()})
  output$probability_negativebinomial1 <- renderUI({prob_representation (distribution_choice = "negative1")})
  output$summary_negativebinomial1 <- renderUI({summary_representation(distribution_choice = "negative1",
                                                                       n_dist = input$r_negativebinomial1, p_dist = input$p_negativebinomial1)})
  
  #Negative Binomial (II)
  my_negativebinomial2_reactive <- callModule(module = negativebinomial2_reactive, id = "neg2_react",
                                              tails = reactive(input$tails_negativebinomial2), n = reactive(input$r_negativebinomial2), 
                                              p = reactive(input$p_negativebinomial2), x = reactive(input$x_negativebinomial2), 
                                              a = reactive(input$a_negativebinomial2), b = reactive(input$b_negativebinomial2))
  
  my_negativebinomial2_information_reactive <- callModule(module = negativebinomial2_information_reactive, id = "neg2_info_react",
                                                          tails = reactive(input$tails_negativebinomial2), n = reactive(input$r_negativebinomial2), 
                                                          p = reactive(input$p_negativebinomial2), x = reactive(input$x_negativebinomial2), 
                                                          a = reactive(input$a_negativebinomial2), b = reactive(input$b_negativebinomial2))
  
  output$dt_negativebinomial2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_negativebinomial2_reactive())})
  output$plot_negativebinomial2 <- plotly::renderPlotly({discrete_plot(df_discrete = my_negativebinomial2_reactive())})
  output$distribution_negativebinomial2 <- renderUI({my_negativebinomial2_information_reactive()})
  output$probability_negativebinomial2 <- renderUI({prob_representation (distribution_choice = "negative2")})
  output$summary_negativebinomial2 <- renderUI({summary_representation(distribution_choice ="negative2", 
                                                                       n_dist = input$r_negativebinomial2, p_dist = input$p_negativebinomial2)})
  
  #Hyper-Geometric
  my_hypergeometric_reactive <- callModule(module = hypergeometric_reactive, id = "hyper_react", tails = reactive(input$tails_hypergeometric), 
                                           M = reactive(input$M_hypergeometric), N = reactive(input$N_hypergeometric-input$M_hypergeometric), 
                                           n = reactive(input$n_hypergeometric), x = reactive(input$x_hypergeometric), 
                                           a = reactive(input$a_hypergeometric), b = reactive(input$b_hypergeometric))
  
  my_hypergeometric_information_reactive <- callModule(module = hypergeometric_information_reactive, id = "hyper_info_react", tails = reactive(input$tails_hypergeometric), 
                                                       M = reactive(input$M_hypergeometric), N = reactive(input$N_hypergeometric-input$M_hypergeometric), 
                                                       n = reactive(input$n_hypergeometric), x = reactive(input$x_hypergeometric), 
                                                       a = reactive(input$a_hypergeometric), b = reactive(input$b_hypergeometric))
  
  output$dt_HyperGeometric <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_hypergeometric_reactive())})
  output$plot_HyperGeometric <- plotly::renderPlotly({discrete_plot(df_discrete = my_hypergeometric_reactive())})
  output$distribution_HyperGeometric <- renderUI({my_hypergeometric_information_reactive()})
  output$probability_HyperGeometric <- renderUI({prob_representation (distribution_choice = "hypergeometric")})
  output$summary_HyperGeometric <- renderUI({summary_representation(distribution_choice = "hypergeometric", n_dist = input$n_hypergeometric,
                                                                    M_dist = input$M_hypergeometric, N_dist = input$N_hypergeometric)})
  
  #Poisson
  my_poisson_reactive <- callModule(module = poisson_reactive, id = "poi_react", tails = reactive(input$tails_poisson), 
                                    lambda = reactive(input$lambda_poisson), x = reactive(input$x_poisson), 
                                    a = reactive(input$a_poisson), b = reactive(input$b_poisson))
  
  my_poisson_information_reactive <- callModule(module = poisson_information_reactive, id = "poi_info_react", tails = reactive(input$tails_poisson), 
                                                lambda = reactive(input$lambda_poisson), x = reactive(input$x_poisson), 
                                                a = reactive(input$a_poisson), b = reactive(input$b_poisson))
  
  
  output$dt_Poisson <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = my_poisson_reactive())})
  output$plot_Poisson <- plotly::renderPlotly({discrete_plot(df_discrete = my_poisson_reactive())})
  output$distribution_Poisson <- renderUI({my_poisson_information_reactive()})
  output$probability_Poisson <- renderUI({prob_representation (distribution_choice = "poisson")})
  output$summary_Poisson <- renderUI({summary_representation(distribution_choice = "poisson", lambda_dist = input$lambda_poisson)})
  
  
  
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
