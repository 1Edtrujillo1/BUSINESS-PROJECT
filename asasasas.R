distrib_representation <- function(distribution_choice = c("binomial", "hypergeometric", "poisson", 
                                                           "geometric1", "geometric2", "negative1", 
                                                           "negative2"), 
                                   tail_distribution = c("under_x", "above_x", "interval"), 
                                   n_dist = NULL, p_dist = NULL, 
                                   lambda_dist = NULL, M_dist = NULL,
                                   N_dist = NULL, x_dist = NULL, 
                                   a_dist = NULL, b_dist = NULL) {  
  
  distribution_args <- list(binomial_args = list(size = n_dist, prob = p_dist),
                            hypergeometric_args = list(m = M_dist, n = N_dist, 
                                                       k = n_dist),
                            poisson_args = list(lambda = lambda_dist),
                            geometric_args = list(prob = p_dist),
                            negative_args = list(size = n_dist, prob = p_dist)
  )
  
  distribution_values_function <- function(quantile_value){
    
    map(c(FALSE, TRUE),
        function(under_x){
          
          invoke_map(list(binomial = safely(pbinom), hypergeometric = safely(phyper),
                          poisson = safely(ppois), geometric = safely(pgeom), negative = safely(pnbinom)),
                     
                     list(binomial_args = list(q = quantile_value, lower.tail = under_x) %>% 
                            append(distribution_args$binomial_args),
                          
                          hypergeometric_args = list(q = quantile_value, lower.tail = under_x) %>% 
                            append(distribution_args$hypergeometric_args),
                          
                          poisson_args = list(q = quantile_value, lower.tail = under_x) %>% 
                            append(distribution_args$poisson_args),
                          
                          geometric_args = list(q = quantile_value, lower.tail = under_x) %>% 
                            append(distribution_args$geometric_args),
                          
                          negative_args = list(q = quantile_value, lower.tail = under_x) %>% 
                            append(distribution_args$negative_args)
                          
                     )
          ) %>% map("result") %>% discard(~is.null(.x))
        }
        
    ) %>% set_names(c("upper_tail", "under_tail")) %>% map(~.x[[str_extract(string = distribution_choice, pattern = "[^\\d]*")]])
    
  }
  
  interval_values <- function(b_a){
    value <- map(b_a, distribution_values_function) %>% set_names(c("b", "a")) %>% 
      map("under_tail") 
    ifelse(a_dist <= b_dist, value$b-value$a, "a must be less than or equal to b") %>% return()
  }
  
  if(distribution_choice %in% c("binomial", "hypergeometric", "poisson", "geometric1", "negative1")){
    if(tail_distribution == "under_x") distribution_values <- distribution_values_function(x_dist)$under_tail
    else if(tail_distribution == "above_x") distribution_values <- distribution_values_function(x_dist)$upper_tail
    else if(tail_distribution == "interval")
      distribution_values <- interval_values(b_a = list(b_dist, a_dist-1))
  }
  else if(distribution_choice == "geometric2"){
    if(tail_distribution == "under_x") distribution_values <- distribution_values_function(x_dist-1)$under_tail
    else if(tail_distribution == "above_x") distribution_values <- distribution_values_function(x_dist-1)$upper_tail
    else if(tail_distribution == "interval")
      distribution_values <- interval_values(b_a = list(b_dist-1, a_dist-2))
  }
  else if(distribution_choice == "negative2"){
    if(tail_distribution == "under_x") distribution_values <- distribution_values_function(x_dist-n_dist)$under_tail
    else if(tail_distribution == "above_x") distribution_values <- distribution_values_function(x_dist-n_dist)$upper_tail
    else if(tail_distribution == "interval")
      distribution_values <- interval_values(b_a = list(b_dist-n_dist, a_dist-n_dist-1))
  }
  
  distribution_values <- distribution_values %>% round(4)
  
  
}











n_binomial <- 20
p_binomial <- 0.5


x_distribution <- 8
a_distribution <- 8
b_distribution <- 12


a <- distrib_representation(distribution_choice = "binomial",
                            tail_distribution = "interval",
                            n_dist = n_binomial, p_dist = p_binomial, 
                            a_dist = a_distribution, b_dist = b_distribution)


