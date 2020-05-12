
#'@description Obtain a dataset of notifications for the header shiny
#'@param path path of the json file
#'@return Data.table obtained from a JSON file

notifications_list <- function(path){
  
  json_file <- jsonlite::fromJSON(path)
  
  df <- data.table(
    
    text = json_file[["Text"]],
    
    status = json_file[["Status"]],
    
    href = json_file[["Href"]],
    
    icon = json_file[["Icon"]]) %>%
    
    .[,lapply(.SD, as.character)] %>% 
    .[,"text":= toupper(get("text"))]
  
  df <- df %>% setnames(toupper(names(df)))
  
}


#'@description Obtain a dataset of the distribution of a discrete Random Variable
#'@param path argumnents of each quantile and probability function from a discrete random variable
#'@return Data.table with three columns, one, is the observations of the random variable, the next column 
#'indicate if the values are under, upper given a number x or inside an interval [a,b], and the last column
#'is the probability function this is the probability of each x

discrete_distribution <- function(distribution_choice = c("binomial", "hypergeometric", "poisson", 
                                                          "geometric1", "geometric2", "negative1", 
                                                          "negative2"),
                                  tail_distribution = c("under_x", "above_x", "interval"),
                                  n_dist = NULL, p_dist = NULL, 
                                  lambda_dist = NULL, M_dist = NULL,
                                  N_dist = NULL, x_dist = NULL, 
                                  a_dist = NULL, b_dist = NULL){
  
  distribution_args <- list(binomial_args = list(size = n_dist, prob = p_dist),
                            
                            hypergeometric_args = list(m = M_dist, n = N_dist, 
                                                       k = n_dist),
                            
                            poisson_args = list(lambda = lambda_dist),
                            
                            geometric_args = list(prob = p_dist),
                            
                            negative_args = list(size = n_dist, prob = p_dist)
  )
  
  quantile_values_list <- map(c(FALSE, TRUE), 
                              function(under_x = c(TRUE, FALSE)){
                                
                                invoke_map(list(binomial = safely(qbinom), hypergeometric = safely(qhyper),  
                                                poisson = safely(qpois), geometric = safely(qgeom), negative = safely(qnbinom)),
                                           
                                           list(binomial_args = list(p = 0.99999, lower.tail = under_x) %>% 
                                                  append(distribution_args$binomial_args),
                                                
                                                hypergeometric_args = list(p = 0.99999, lower.tail = under_x) %>% 
                                                  append(distribution_args$hypergeometric_args),
                                                
                                                poisson_args = list(p = 0.99999, lower.tail = under_x) %>% 
                                                  append(distribution_args$poisson_args),
                                                
                                                geometric_args = list(p = 0.99999, lower.tail = under_x) %>% 
                                                  append(distribution_args$geometric_args),
                                                
                                                negative_args = list(p = 0.99999, lower.tail = under_x) %>% 
                                                  append(distribution_args$negative_args)
                                           )
                                ) %>% map("result") %>% discard(~is.null(.x))
                              }
  ) %>% set_names(c("upper_tail", "under_tail")) %>% map(~.x[[str_extract(string = distribution_choice, pattern = "[^\\d]*")]]) #everything except a 0 or more digit
  
  
  if(distribution_choice %in% c("binomial", "hypergeometric", "poisson", "negative1"))
    quantile_values <- quantile_values_list$upper_tail:quantile_values_list$under_tail
  else if(distribution_choice == "geometric1")
    quantile_values <- 0:quantile_values_list$under_tail
  else if(distribution_choice == "geometric2") #success
    quantile_values <- 1:(quantile_values_list$under_tail+1)
  else if(distribution_choice == "negative2") #success
    quantile_values <- n_dist:(quantile_values_list$under_tail+n_dist)
  
  quantile_values <- data.table(random_variable = quantile_values)
  
  #copy() to no affect the original dataset and use the if conditions to define the different form of the variable
  
  if(tail_distribution == "under_x") 
    df_func_probability <- 
    copy(quantile_values)[,id:=ifelse(random_variable <= x_dist, "inside", "outside")]
  else if(tail_distribution == "above_x")
    df_func_probability <- 
    copy(quantile_values)[,id:=ifelse(random_variable > x_dist, "inside", "outside")]
  else if(tail_distribution == "interval") 
    df_func_probability <- 
    copy(quantile_values)[,id:=ifelse(random_variable >= a_dist & random_variable <= b_dist,
                                      "inside", "outside")]
  
  #do.call pass list to argument function 
  options(scipen = 999) #avoid scientific notation for the probability
  
  if(distribution_choice == "binomial") 
    df_func_probability[,probability:= do.call(dbinom, args = list(x = random_variable) %>% append(distribution_args$binomial_args))] %>% 
    return()
  else if(distribution_choice == "hypergeometric")
    df_func_probability[,probability:= do.call(dhyper, args = list(x = random_variable) %>% append(distribution_args$hypergeometric_args))] %>% 
    return()
  else if(distribution_choice == "poisson") 
    df_func_probability[,probability:= do.call(dpois, args = list(x = random_variable) %>% append(distribution_args$poisson_args))] %>% 
    return()
  else if(distribution_choice == "geometric1")
    df_func_probability[,probability:= do.call(dgeom, args = list(x = random_variable) %>% append(distribution_args$geometric_args))] %>% 
    return()
  else if(distribution_choice == "geometric2")
    df_func_probability[,probability:= do.call(dgeom, args = list(x = 0:quantile_values_list$under_tail) %>% append(distribution_args$geometric_args))] %>% 
    return()
  else if(distribution_choice == "negative1")
    df_func_probability[,probability:= do.call(dnbinom, args = list(x = random_variable) %>% append(distribution_args$negative_args))] %>% 
    return()
  else if(distribution_choice  == "negative2")
    df_func_probability[,probability:=do.call(dnbinom, args = list(x = 0:quantile_values_list$under_tail) %>% append(distribution_args$negative_args))] %>% 
    return()
  
  df_func_probability <- df_func_probability[,id := toupper(id)] %>% 
    set_names(toupper(names(df_func_probability))) %>% 
    return()
  
}

#'@description Create a plot of the probability of each x of X (funcion de masa de probabilidad)
#'to observe the behavior of the Distribution Function F(x)
#'@param df_discrete
#'@return plot of the discrete distribution

discrete_plot <- function(df_discrete){
  
  plot <- df_discrete %>% ggplot(aes(x = as.factor(RANDOM_VARIABLE), y = PROBABILITY, fill = ID)) + 
    geom_col(alpha = 0.8) + 
    geom_text(aes(label = round(PROBABILITY, digits = 3), #text the probability in each bar
                  y = PROBABILITY + 0.005,),
              colour = "white",
              position = position_dodge(0.9),
              size = 3, # size of text
              vjust = 0) + 
    theme_minimal() +
    labs(x = "X", 
         y = "Probability Function", 
         title = toupper(str_extract(string = deparse(substitute(df_discrete)), 
                                     pattern = "(\\w*)(?=\\d\\_)|(\\w*)(?=\\_)"))) + #all before digit_ or _
    theme(plot.title = element_text(face = "italic", hjust = 0.5),
          axis.title = element_text(face = "italic"), 
          axis.ticks = element_line(color = "red"),
          axis.text = element_text(colour = "white"),
          text = element_text(colour = "white")) 
  
  plotly::ggplotly(plot) %>% 
    layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>% 
    return()
  
}







