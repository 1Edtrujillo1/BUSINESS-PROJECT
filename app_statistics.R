
library(purrr)

map(c("shiny", "shinydashboard", "dashboardthemes", "shinydashboardPlus", "shinyWidgets", "mixdist","DT",
      "jsonlite", "data.table", "dplyr", "bit64", "stringr", "scales", "ggplot2", "plotly"), 
    require, character.only = TRUE)

map(c("header.R", "sidebar.R", "body.R", "rightsidebar.R",
      "INFORMATION/utils.R"), 
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
  
  #'@description Create a beautiful datatable from a discrete distribution
  #'@param discrete_reactive reactive discrete datasets
  #'@return beautiful DT with specific arguments 
  dataset_discrete_reactive <- function(discrete_reactive){
      
      df <- copy(discrete_reactive) %>% setnames(old = "RANDOM_VARIABLE", new = "RANDOM VARIABLE")
      
      factors_vars <- c("RANDOM VARIABLE", "ID")
      
      df[,(factors_vars):=lapply(.SD, as.factor), .SDcols = factors_vars] %>% 
        .[,PROBABILITY := scales::percent(x = PROBABILITY, accuracy = 0.00000001)]
      
      DT::datatable(data = df, 
                    style = 'bootstrap', #theme of the datatable
                    
                    filter = list(position = 'top', clear = FALSE),
                    
                    options = list(
                      
                      autoWidth = TRUE,
                      
                      pageLength = binomial_reactive()[,.N, by = "ID"] %>% 
                        .[,.(mean(N, na.rm = TRUE))] %>% as.integer())
      )  
  }
  
  #Binomial
  binomial_reactive <- reactive({
    if(input$tails_binomial == "lower_tail") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "under_x",
                            n_dist = input$n_binomial, p_dist = input$p_binomial, 
                            x_dist = input$x_binomial) %>% return()
    
    else if(input$tails_binomial == "upper_tail") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "above_x",
                            n_dist = input$n_binomial, p_dist = input$p_binomial, 
                            x_dist = input$x_binomial) %>% return()
    
    else if(input$tails_binomial == "interval") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "interval",
                            n_dist = input$n_binomial, p_dist = input$p_binomial, 
                            a_dist = input$a_binomial, b_dist = input$b_binomial) %>% return()
  })
  
  output$dt_Binomial <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = binomial_reactive())})
  output$plot_Binomial <- plotly::renderPlotly({discrete_plot(df_discrete = binomial_reactive())})
  
  #Geometric (I)
  geometric1_reactive <- reactive({
    if(input$tails_geometric1 == "lower_tail") 
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "under_x",
                            p_dist = input$p_geometric1,
                            x_dist = input$x_geometric1)
    
    else if(input$tails_geometric1 == "upper_tail")
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "above_x",
                            p_dist = input$p_geometric1,
                            x_dist = input$x_geometric1)
    
    else if(input$tails_geometric1 == "interval")
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "interval",
                            p_dist = input$p_geometric1,
                            a_dist = input$a_geometric1, b_dist = input$b_geometric1)
  })
  output$dt_geometric1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = geometric1_reactive())})
  output$plot_geometric1 <- plotly::renderPlotly({discrete_plot(df_discrete = geometric1_reactive())})
  
  #Geometric (II)
  geometric2_reactive <- reactive({
    if(input$tails_geometric2 == "lower_tail")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "under_x",
                            p_dist = input$p_geometic2,
                            x_dist = input$x_geometric2)
    
    else if(input$tails_geometric2 == "upper_tail")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "above_x",
                            p_dist = input$p_geometic2,
                            x_dist = input$x_geometric2)
    
    else if(input$tails_geometric2 == "interval")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "interval",
                            p_dist = input$p_geometic2,
                            a_dist = input$a_geometric2, b_dist = input$b_geometric2)
  })
  output$dt_geometric2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = geometric2_reactive())})
  output$plot_geometric2 <- plotly::renderPlotly({discrete_plot(df_discrete = geometric2_reactive())})
  
  #Negative Binomial (I)
  negativebinomial1_reactive <- reactive({
    if(input$tails_negativebinomial1 == "lower_tail")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "under_x",
                            n_dist = input$r_negativebinomial1, p_dist = input$p_negativebinomial1,
                            x_dist = input$x_negativebinomial1)
    
    else if(input$tails_negativebinomial1 == "upper_tail")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "above_x",
                            n_dist = input$r_negativebinomial1, p_dist = input$p_negativebinomial1,
                            x_dist = input$x_negativebinomial1)
    
    else if(input$tails_negativebinomial1 == "interval")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "interval",
                            n_dist = input$r_negativebinomial1, p_dist = input$p_negativebinomial1,
                            a_dist = input$a_negativebinomial1, b_dist = input$b_negativebinomial1)
  })
  output$dt_negativebinomial1 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = negativebinomial1_reactive())})
  output$plot_negativebinomial1 <- plotly::renderPlotly({discrete_plot(df_discrete = negativebinomial1_reactive())}) 
  
  #Negative Binomial (II)
  negativebinomial2_reactive <- reactive({
    if(input$tails_negativebinomial2 == "lower_tail")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "under_x",
                            n_dist = input$r_negativebinomial2, p_dist = input$p_negativebinomial2,
                            x_dist = input$x_negativebinomial2)
    
    else if(input$tails_negativebinomial2 == "upper_tail")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "above_x",
                            n_dist = input$r_negativebinomial2, p_dist = input$p_negativebinomial2,
                            x_dist = input$x_negativebinomial2)
    
    else if(input$tails_negativebinomial2 == "interval")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "interval",
                            n_dist = input$r_negativebinomial2, p_dist = input$p_negativebinomial2,
                            a_dist = input$a_negativebinomial2, b_dist = input$b_negativebinomial2)
  })
  output$dt_negativebinomial2 <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = negativebinomial2_reactive())})
  output$plot_negativebinomial2 <- plotly::renderPlotly({discrete_plot(df_discrete = negativebinomial2_reactive())})
  
  #Hyper-Geometric
  hypergeometric_reactive <- reactive({
    if(input$tails_hypergeometric == "lower_tail")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "under_x",
                            M_dist = input$M_hypergeometric, N_dist = input$N_hypergeometric-input$M_hypergeometric,
                            n_dist = input$n_hypergeometric,
                            x_dist = input$x_hypergeometric)
    
    else if(input$tails_hypergeometric == "upper_tail")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "above_x",
                            M_dist = input$M_hypergeometric, N_dist = input$N_hypergeometric-input$M_hypergeometric,
                            n_dist = input$n_hypergeometric,
                            x_dist = input$x_hypergeometric)
    
    else if(input$tails_hypergeometric == "interval")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "interval",
                            M_dist = input$M_hypergeometric, N_dist = input$N_hypergeometric-input$M_hypergeometric,
                            n_dist = input$n_hypergeometric,
                            a_dist = input$a_hypergeometric, b_dist = input$b_hypergeometric)
  })
  output$dt_HyperGeometric <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = hypergeometric_reactive())})
  output$plot_HyperGeometric <- plotly::renderPlotly({discrete_plot(df_discrete = hypergeometric_reactive())})
  
  #Poisson
  poisson_reactive <- reactive({
    if(input$tails_poisson == "lower_tail")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "under_x",
                            lambda_dist = input$lambda_poisson, 
                            x_dist = input$x_poisson)
    
    else if(input$tails_poisson == "upper_tail")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "above_x",
                            lambda_dist = input$lambda_poisson, 
                            x_dist = input$x_poisson)
    
    else if(input$tails_poisson == "interval")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "interval",
                            lambda_dist = input$lambda_poisson, 
                            a_dist = input$a_poisson, b_dist = input$b_poisson)
  })
  output$dt_Poisson <- DT::renderDT({dataset_discrete_reactive(discrete_reactive = poisson_reactive())})
  output$plot_Poisson <- plotly::renderPlotly({discrete_plot(df_discrete = poisson_reactive())})
  
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
