####### Statistical Distributions ##########################

### Discrete Distributions ##################

#'@description Obtain a dataset of the distribution of a discrete Random Variable
#'@param argumnents of each quantile and probability function from a discrete random variable
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
                              function(under_x){
                                
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


#'@description Create a beautiful datatable from a discrete distribution
#'@param discrete_reactive reactive discrete datasets
#'@return beautiful DT with specific arguments 
dataset_discrete_reactive <- function(discrete_reactive){
  
  df <- copy(discrete_reactive) %>% setnames(old = "RANDOM_VARIABLE", new = "RANDOM VARIABLE") %>% 
    .[,PROBABILITY:=round(PROBABILITY, 5)]
  
  factors_vars <- c("RANDOM VARIABLE", "ID")
  
  df[,(factors_vars):=lapply(.SD, as.factor), .SDcols = factors_vars] %>% 
    .[,PROBABILITY := scales::percent(x = PROBABILITY, accuracy = 0.00001)]
  
  DT::datatable(data = df, 
                style = 'bootstrap', #theme of the datatable
                
                filter = list(position = 'top', clear = FALSE),
                
                options = list(
                  
                  autoWidth = TRUE,
                  
                  pageLength = df[,.N, by = "ID"] %>% 
                    .[,.(mean(N, na.rm = TRUE))] %>% as.integer())
  )  
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
         title = toupper(str_extract(string = deparse(substitute(df_discrete)), #deparse(substitute()) nonestandar evaluation to bring the name of the dataset as a character vector
                                     pattern = "(?<=my_)\\w*(\\w*)(?=\\d\\_)|(?<=my_)\\w*(\\w*)(?=\\_)"))) + #all after my_ and before digit_ or _
    theme(plot.title = element_text(face = "italic", hjust = 0.5),
          axis.title = element_text(face = "italic"), 
          axis.ticks = element_line(color = "red"),
          axis.text = element_text(colour = "white"),
          text = element_text(colour = "white")) 
  
  plotly::ggplotly(plot) %>% 
    layout(plot_bgcolor='transparent', paper_bgcolor='transparent') %>% 
    return()
  
}


#'@description Create latex formula adding the value of the distribution function for a value x, a or b of 
#'the distribution
#'@param argumnents of each distribution 
#'@return a withMathJax formula latex of the information of each distribution
distrib_representation <- function(distribution_choice = c("binomial", "hypergeometric", "poisson", 
                                                           "geometric1", "geometric2", "negative1", 
                                                           "negative2"), 
                                   tail_distribution = c("under_x", "above_x", "interval"), 
                                   n_dist = NULL, p_dist = NULL, 
                                   lambda_dist = NULL, M_dist = NULL,
                                   N_dist = NULL, x_dist = NULL, 
                                   a_dist = NULL, b_dist = NULL) {  
  
  if(distribution_choice == "binomial"){
    distribution <- "\\(Bin \\)"
    arguments <- paste0("\\(n= \\)", n_dist, "\\(,p= \\)" , p_dist, sep = " ")
  }
  
  else if(distribution_choice == "hypergeometric"){
    distribution <- "\\(HG \\)"
    arguments <- paste0("\\(n= \\)", n_dist, "\\(,N= \\)", N_dist, "\\(,M= \\)", M_dist, sep = " ")
  }
  
  else if(distribution_choice == "poisson"){
    distribution <- "\\(Pois \\)"
    arguments <- paste0("\\(\\lambda= \\)", lambda_dist, sep = " ")
  }
  
  else if(distribution_choice %in% c("geometric1", "geometric2")){
    distribution <- "\\(Geom \\)"
    arguments <- paste0("\\(p= \\)", p_dist, sep = " ")
  }
  
  else if(distribution_choice %in% c("negative1", "negative2")){
    distribution <- "\\(NG \\)"
    arguments <- paste0("\\(r= \\)", n_dist, "\\(,p= \\)", p_dist, sep = " ")
  }
  
  explanation <- paste0("\\(X \\sim \\)", distribution, "\\(( \\)", arguments, "\\()\\)", "\\(and\\)", sep = " ")
  
  if(tail_distribution == "under_x")
    distrib <- paste0(explanation, "\\(P(X \\leq \\)", x_dist, "\\() \\)", "\\(= \\)", sep = " ")
  else if(tail_distribution == "above_x")
    distrib <- paste0(explanation, "\\(P(X > \\)", x_dist, "\\() \\)", "\\(= \\)", sep = " ")  
  else if(tail_distribution == "interval")
    distrib <- paste0(explanation, "\\(P(\\)", a_dist, "\\(\\leq X \\leq \\)", b_dist, "\\()= \\)",  sep = " ") 
  
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
  
  distribution_values <- ifelse(is.numeric(distribution_values), 
                                distribution_values %>% round(5) %>% scales::percent(accuracy = 0.00001),
                                distribution_values)
  
  paste0(distrib, distribution_values, sep = " ") %>% withMathJax() %>% return()
  
}


#'@description Write the respective probability function of each distribution
#'@param distribution_choice 
#'@return Latex of a formula
prob_representation <- function(distribution_choice = c("binomial", "hypergeometric", "poisson", 
                                                        "geometric1", "geometric2", "negative1", 
                                                        "negative2")){
  if(distribution_choice == "binomial")
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X = x) = \\binom{n}{x}p^x(1-p)^{n-x}$$"),
      br(),
      helpText("where \\( x = 0, 1, \\dots, n\\) and \\( 0 \\leq p \\leq 1 \\)")
    )
  else if(distribution_choice == "hypergeometric") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X = x) = \\dfrac{\\binom{M}{x} \\binom{N-M}{n-x}}{\\binom{N}{n}}  $$"),
      br(),
      helpText("for \\( x = 0, 1, \\dots , n\\)"),
      br(),
      helpText("where \\( x \\leq M \\) and \\( n - x \\leq N - M \\)")
    )
  else if(distribution_choice == "poisson") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X = x) = \\dfrac{e^{-\\lambda}\\lambda^x}{x!} $$"),
      br(),
      helpText("for \\( x = 0, 1, 2, \\dots\\)"),
      br(),
      helpText("where \\( \\lambda > 0 \\)")
    )
  else if(distribution_choice == "geometric1") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X=x) = (1-p)^x p $$"),
      br(),
      helpText("where \\( x = 0, 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)")
    )
  else if(distribution_choice == "geometric2") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X=x) = (1-p)^{x-1} p $$"),
      br(),
      helpText("where \\( x = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)")
    )
  
  else if(distribution_choice == "negative1") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X = x) = \\binom{x+r-1}{r-1} (1-p)^x p^r $$"),
      br(),
      helpText("where \\( x = 0, 1, 2, \\dots, r = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)")
    )
  else if(distribution_choice == "negative2") 
    withMathJax(
      helpText("Probability mass function: $$ f(x) = P(X = x) = \\binom{x-1}{r-1}p^r (1-p)^{x-r} $$"),
      br(),
      helpText("where \\( x = r, r+1, \\dots \\) and \\( 0 < p \\leq 1 \\)")
    )
}


#'@description  Create latex formula adding the calculation of the mean, variance, sd for each distribution
#'@param distribution_choice and arguments of each distribution
#'@return a WithMathJax formula latex join with a calculated value (mean, variance, sd)
summary_representation <- function(distribution_choice = c("binomial", "hypergeometric", "poisson", 
                                                           "geometric1", "geometric2", "negative1", 
                                                           "negative2"),
                                   n_dist = NULL, p_dist = NULL, 
                                   lambda_dist = NULL, M_dist = NULL, N_dist = NULL){
  if(distribution_choice == "binomial"){
    mean <- "\\(np = \\)"
    value_mean <- n_dist*p_dist
    variance <- "\\(np(1-p) = \\)"
    value_variance <- n_dist*p_dist*(1-p_dist)
    standardesv <- "\\(\\sqrt{np(1-p)} = \\)"
  }
  else if(distribution_choice == "hypergeometric"){
    mean <- "\\(n\\dfrac{M}{N} = \\)"
    value_mean <- n_dist*(M_dist/N_dist)
    variance <- "\\(n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big) =  \\)"
    value_variance <- n_dist*(M_dist/N_dist)*(1-(M_dist/N_dist))*((N_dist-n_dist)/(N_dist-1))
    standardesv <- "\\(\\sqrt{n\\dfrac{M}{N}\\Big(1 - \\dfrac{M}{N}\\Big)\\Big(\\dfrac{N-n}{N-1}\\Big)} = \\)"
  }
  else if(distribution_choice == "poisson"){
    mean <- "\\(\\lambda = \\)"
    value_mean <- lambda_dist
    variance <- "\\(\\lambda = \\)"
    value_variance <- lambda_dist
    standardesv <- "\\(\\sqrt{\\lambda} = \\)"
  }
  else if(distribution_choice == "geometric1"){
    mean <- "\\(\\dfrac{1-p}{p} = \\)"
    value_mean <- (1-p_dist)/p_dist
    variance <- "\\(\\dfrac{1-p}{p^2} = \\)"
    value_variance <- (1-p_dist)/(p_dist^2)
    standardesv <- "\\(\\sqrt{\\dfrac{1-p}{p^2}} = \\)"
  }
  else if(distribution_choice == "geometric2"){
    mean <- "\\(\\dfrac{1}{p} = \\)"
    value_mean <- 1/p_dist
    variance <- "\\(\\dfrac{1-p}{p^2} = \\)"
    value_variance <- (1-p_dist)/(p_dist^2)
    standardesv <- "\\(\\sqrt{\\dfrac{1-p}{p^2}} = \\)"
  }
  else if(distribution_choice == "negative1"){
    mean <- "\\(\\dfrac{r(1-p)}{p} = \\)"
    value_mean <- (n_dist*(1-p_dist))/p_dist
    variance <- "\\(\\dfrac{r(1-p)}{p^2} = \\)"
    value_variance <- (n_dist*(1-p_dist))/(p_dist^2)
    standardesv <- "\\(\\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)"
  }
  else if(distribution_choice == "negative2"){
    mean <- "\\(\\dfrac{r}{p} = \\)"
    value_mean <- n_dist/p_dist
    variance <- "\\(\\dfrac{r(1-p)}{p^2} = \\)"
    value_variance <- (n_dist*(1-p_dist))/(p_dist^2)
    standardesv <- "\\(\\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)"
  }
  
  values_summary <- map(list(value_mean = value_mean, value_variance = value_variance, 
                             value_standardesv = sqrt(value_variance)), round, 3) 
  
  summary <- map2(
    list(mean, variance, standardesv),
    values_summary,
    ~paste0(.x, .y, sep = " ")
  ) %>% 
    set_names(c("mean", "variance", "standardesv"))
  
  withMathJax(
    helpText(paste0("\\(\\mu = E(X) = \\)", summary$mean, sep = " ")),
    br(),
    helpText(paste0("\\(\\sigma = SD(X) = \\)", summary$standardesv, sep = " ")),
    br(),
    helpText(paste0("\\(\\sigma^2 = Var(X) = \\)", summary$variance, sep = " "))
  )
  
}




