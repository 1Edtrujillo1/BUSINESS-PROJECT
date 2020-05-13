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
                  
                  pageLength = df[,.N, by = "ID"] %>% 
                    .[,.(mean(N, na.rm = TRUE))] %>% as.integer())
  )  
}


#Binomial
binomial_reactive <- function(input, output, session, tails, n, p, x, a, b){
  reactive({
    if(tails() == "lower_tail") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "under_x",
                            n_dist = n(), p_dist = p(), 
                            x_dist = x()) %>% return()
    
    else if(tails() == "upper_tail") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "above_x",
                            n_dist = n(), p_dist = p(), 
                            x_dist = x()) %>% return()
    
    else if(tails() == "interval") 
      discrete_distribution(distribution_choice = "binomial",
                            tail_distribution = "interval",
                            n_dist = n(), p_dist = p(), 
                            a_dist = a(), b_dist = b()) %>% return()
  })
}

binomial_information_reactive <- function(input, output, session, tails, n, p, x, a, b){
  reactive({
    if(tails() == "lower_tail") 
      name(distribution_choice = "binomial",
           tail_distribution = "under_x",
           n_dist = n(), p_dist = p(), 
           x_dist = x()) %>% return()
    
    else if(tails() == "upper_tail") 
      name(distribution_choice = "binomial",
           tail_distribution = "above_x",
           n_dist = n(), p_dist = p(), 
           x_dist = x()) %>% return()
    
    else if(tails() == "interval") 
      name(distribution_choice = "binomial",
           tail_distribution = "interval",
           n_dist = n(), p_dist = p(), 
           a_dist = a(), b_dist = b()) %>% return()
  })
}

#Geometric (I)
geometric1_reactive <- function(input, output, session, tails, p, x, a, b){
  reactive({
    if(tails() == "lower_tail") 
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "under_x",
                            p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "above_x",
                            p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "geometric1",
                            tail_distribution = "interval",
                            p_dist = p(),
                            a_dist = a(), b_dist = b())
  })
}

#Geometric (II)
geometric2_reactive <- function(input, output, session, tails, p, x, a, b){
  reactive({
    if(tails() == "lower_tail")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "under_x",
                            p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "above_x",
                            p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "geometric2",
                            tail_distribution = "interval",
                            p_dist = p(),
                            a_dist = a(), b_dist = b())
  })
}

#Negative Binomial (I)
negativebinomial1_reactive <- function(input, output, session, tails, n, p, x, a, b){
  reactive({
    if(tails() == "lower_tail")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "under_x",
                            n_dist = n(), p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "above_x",
                            n_dist = n(), p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "negative1",
                            tail_distribution = "interval",
                            n_dist = n(), p_dist = p(),
                            a_dist = a(), b_dist = b())
  })
}

#Negative Binomial (II)
negativebinomial2_reactive <- function(input, output, session, tails, n, p, x, a, b){
  reactive({
    if(tails() == "lower_tail")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "under_x",
                            n_dist = n(), p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "above_x",
                            n_dist = n(), p_dist = p(),
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "negative2",
                            tail_distribution = "interval",
                            n_dist = n(), p_dist = p(),
                            a_dist = a(), b_dist = b())
  })
}

#Hyper-Geometric
hypergeometric_reactive <- function(input, output, session, tails, M, N, n, x, a, b){
  reactive({
    if(tails() == "lower_tail")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "under_x",
                            M_dist = M(), N_dist = N(),
                            n_dist = n(),
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "above_x",
                            M_dist = M(), N_dist = N(),
                            n_dist = n(),
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "hypergeometric",
                            tail_distribution = "interval",
                            M_dist = M(), N_dist = N(),
                            n_dist = n(),
                            a_dist = a(), b_dist = b())
  })
}

#Poisson
poisson_reactive <- function(input, output, session, tails, lambda, x, a, b){
  reactive({
    if(tails() == "lower_tail")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "under_x",
                            lambda_dist = lambda(), 
                            x_dist = x())
    
    else if(tails() == "upper_tail")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "above_x",
                            lambda_dist = lambda(), 
                            x_dist = x())
    
    else if(tails() == "interval")
      discrete_distribution(distribution_choice = "poisson",
                            tail_distribution = "interval",
                            lambda_dist = lambda(), 
                            a_dist = a(), b_dist = b())
  })
}