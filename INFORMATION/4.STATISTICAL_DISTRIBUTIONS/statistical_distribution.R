# x_mean = 0
# x_sd = 1
# rate = 1
# scale = 1
# location = 1
# shape = 3
# deg_free = 6
# deg_free2 = 2
# x_dist = 2.24
# a_dist = 1
# #0.25
# #2.24
# b_dist = 2
# #0.45
# #3.36
# interval_distribution <- "interval"
# #distribution_choice <- "exponential"
# n_dist = 10
# p_dist = 0.5
# M_dist = 50
# N_dist = 500
# lambda_dist = 4
# distribution_choice <- "hypergeometric"

#' @description Dictionary of Distributions _important order of each_
dist_types <- list(
  DISCRETE = c("binomial", "hypergeometric", "poisson", 
               "geometric1", "geometric2", "negative1", 
               "negative2"),
  CONTINOUS = c("exponential", "gamma", "chi_square", 
                "beta", "weibull", "student", 
                "logistic", "normal","log_normal", 
                "cauchy", "fisher")
)

# Table & Plot ------------------------------------------------------------
#' @description 
#' 1. list the arguments and other with the functions
#' 2. simulate the quantile values for each distribution
#' 3. calculate the probability of each simulated in each distribution
#' 4. obtain a dataset with 2. and 3. and determine the type of interval
#' 5. return a table or a plot of a distribution
#' @param distribution_choice type of distribution (discrete or continous)
#' @param arguments of each distribution
#' @param interval_distribution type of tail where the output is going to be
#' @param x_dist,a_dist,b_dist x element of X, represent the inflection point(s)
#' @param return type of output to return
#' @return 
#' return = table table of simulations of the distribution with its probabilities
#' return = plot plot of the simulations of the distribution
table_plot_distribution <- function(
  distribution_choice = dist_types %>% flatten_chr(),
  interval_distribution = c("under_x", "above_x", "interval"),
  return = c("table", "plot"),
  n_dist = NULL, p_dist = NULL, 
  M_dist = NULL, N_dist = NULL, 
  lambda_dist = NULL, rate = NULL,
  x_mean = NULL, x_sd = NULL, 
  scale = NULL, shape = NULL, 
  deg_free = NULL, deg_free2 = NULL,
  location = NULL, x_dist = NULL,
  a_dist = NULL, b_dist = NULL){
  
  options(scipen = 999, warn = -1) #avoid scientific notation and warnings
  
  distribution_args <- list(
    DISCRETE = list(list(size = n_dist, prob = p_dist),
                    list(m = M_dist, n = (N_dist-M_dist), k = n_dist),
                    list(lambda = lambda_dist)) %>% 
      append(list_of_lists(no_sublists = 2, element_sublists = list(prob = p_dist))) %>% 
      append(list_of_lists(no_sublists = 2, element_sublists = list(size = n_dist, 
                                                                    prob = p_dist))),
    CONTINOUS = list(list(rate = rate), 
                     list(shape = shape, rate = rate),
                     list(df = deg_free), 
                     list(shape1 = scale, shape2 = shape),
                     list(shape = shape, scale = scale), 
                     list(df = deg_free),
                     list(location = location, scale = scale), 
                     list(mean = x_mean, sd = x_sd),
                     list(meanlog = x_mean, sdlog = x_sd), 
                     list(location = location, scale = scale),
                     list(df1 = deg_free, df2 = deg_free2))
  )
  distribution_functions <- list(
    DISCRETE = list(QUANTILE = list(safely(qbinom), safely(qhyper), safely(qpois),
                                    safely(qgeom), safely(qgeom), safely(qnbinom),
                                    safely(qnbinom)),
                    DENSITY = list(dbinom, dhyper, dpois,
                                   dgeom, dgeom, dnbinom,
                                   dnbinom)),
    CONTINOUS = list(QUANTILE = list(safely(qexp), safely(qgamma), safely(qchisq),
                                     safely(qbeta), safely(qweibull), safely(qt),
                                     safely(qlogis), safely(qnorm)),
                     DENSITY = list(dexp, dgamma, dchisq,
                                    dbeta, dweibull, dt,
                                    dlogis, dnorm, dlnorm,
                                    dcauchy, df))
  )
  
  quantile_args <- copy(distribution_args)
  pluck(quantile_args, "CONTINOUS") <- pluck(quantile_args, "CONTINOUS")[1:8] #not for log_normal, cauchy, fisher
  quantile_args <- map(quantile_args,
                       ~map(..1, ~.x %>% append(list(p = 0.99999)))
  )
  pluck(quantile_args[["DISCRETE"]][[7]], "p") <- 0.999 #for negative 2
  
  quantile_values <- map(c("DISCRETE", "CONTINOUS"), function(i){
    map(c(FALSE, TRUE), 
        ~invoke_map(pluck(distribution_functions[[i]], "QUANTILE"),
                    pluck(quantile_args, i),
                    lower.tail = .x)
    ) %>% map(~map(.x, "result")) %>% set_names("upper_tail", "under_tail")
  }) %>% set_names("DISCRETE", "CONTINOUS")
  
  quantile_values <- map(c("DISCRETE", "CONTINOUS"), function(i){
    each <- pluck(quantile_values, i)
    map(1:length(pluck(each, 1)), function(j){map(each, j)})#upper or under that is pluck(,1) to make map(quantile_values, 1) and get a sublist of upper_tail and under_tail
  }) %>% set_names("DISCRETE", "CONTINOUS")
  
  specific_quantile_values <- copy(quantile_values)
  pluck(specific_quantile_values, "DISCRETE") <- 
    pluck(specific_quantile_values, "DISCRETE")[c(1:3, 6)] 
  
  specific_quantile_values <- 
    map(specific_quantile_values,
        ~map(..1, ~general_sequence(from = .x$upper_tail, to = .x$under_tail)) 
    ) %>% map2(list(pluck(dist_types, "DISCRETE")[c(1:3, 6)], #"binomial"       "hypergeometric" "poisson"        "negative1" 
                    pluck(dist_types, "CONTINOUS")[1:8]),
               ~ .x %>% set_names(.y)) %>% unname() %>% unlist(recursive = FALSE)
  
  specific_quantile_values2 <- pluck(quantile_values, "DISCRETE")  
  specific_quantile_values2 <- map(c(4,5,7), ~
                                     pluck(specific_quantile_values2, .x))
  specific_quantile_values2 <- 
    map2(list(0, 1, n_dist),
         list(specific_quantile_values2[[1]]$under_tail, 
              ((5 * sqrt((1 - p_dist) / (p_dist^2)))+1),
              (specific_quantile_values2[[3]]$under_tail+n_dist)),
         ~ (if(length(.y) == 0) NA else general_sequence(from = .x, to = .y))
    ) %>% set_names("geometric1", "geometric2", "negative2") 
  
  specific_quantile_values3 <- list(
    log_normal = 
      general_sequence(from = 0, 
                       to = do.call(what = qlnorm, 
                                    args = pluck(distribution_args, "CONTINOUS")[[9]] %>% 
                                      append(list(p = 0.9)))),
    cauchy = general_sequence(from = location - (6 * scale), to = location + (6 * scale)),
    fisher = general_sequence(from = 0, to = 5)
  )
  
  quantile_values <- specific_quantile_values %>% append(specific_quantile_values2) %>% 
    append(specific_quantile_values3) 
  quantile_values <- map(c("DISCRETE", "CONTINOUS"),#divide in order dists of DISCRETE and other with dists of CONTINOUS
                         ~  map(pluck(dist_types, ..1), 
                                ~ pluck(quantile_values, .x)) %>% 
                           set_names(pluck(dist_types, ..1))
  ) %>% set_names("DISCRETE", "CONTINOUS") 
  
  probability_args <- map(c("DISCRETE", "CONTINOUS"), 
                          ~ map2(pluck(quantile_values, ..1),
                                 pluck(distribution_args, ..1),
                                 ~ .y %>% append(list(x = .x))
                          )
  ) %>% set_names("DISCRETE", "CONTINOUS")
  
  probability_values <- map(c("DISCRETE", "CONTINOUS"), function(i){
    invoke_map(map(pluck(distribution_functions[[i]], "DENSITY"), ~ safely(.x)),
               pluck(probability_args, i) 
    ) %>% map("result")}) %>% set_names("DISCRETE", "CONTINOUS") %>% 
    map2(dist_types, ~.x %>% set_names(.y)) 
  
  pluck(probability_values[["DISCRETE"]], "geometric2") <- 
    pluck(probability_values[["DISCRETE"]], "geometric2")*2
  
  if(length(n_dist) == 0 | length(p_dist) == 0){NULL}
  else {pluck(probability_values[["DISCRETE"]], "negative2") <- 
    dnbinom(x = 0:qnbinom(0.999, size = n_dist, prob = p_dist, lower.tail = TRUE), 
            size = n_dist, prob = p_dist)}
  
  df_probability <- map(c("DISCRETE", "CONTINOUS"), 
                        ~map2(pluck(quantile_values, ..1),
                              pluck(probability_values, ..1), 
                              ~data.table(RANDOM_VARIABLE = .x,
                                          PROBABILITY = .y)
                        )
  ) %>% set_names("DISCRETE", "CONTINOUS")
  
  df_probability <-  
    map(c("DISCRETE", "CONTINOUS"),
        ~ map(pluck(copy(df_probability), ..1), 
              safely(~ copy(.x)[, ID:=map_chr(RANDOM_VARIABLE, function(X){
                if(interval_distribution == "under_x"){
                  ifelse(X <= x_dist, "INSIDE", "OUTSIDE")
                }else if(interval_distribution == "above_x"){
                  ifelse(X > x_dist, "INSIDE", "OUTSIDE")
                }else if(interval_distribution == "interval"){
                  ifelse(X >= a_dist & X <= b_dist, "INSIDE", "OUTSIDE")
                }
              })]
              )) %>% map("result") %>% map(safely(~.x[,ID:=as.factor(ID)])) %>% 
          map("result")
    ) %>% set_names("DISCRETE", "CONTINOUS") 
  
  if(return == "table"){
    result <- map(c("DISCRETE", "CONTINOUS"),
                  ~ map(pluck(df_probability, ..1), desing_DT) 
    ) %>% set_names("DISCRETE", "CONTINOUS") 
    
  }else if(return == "plot"){
    result <- list(
      CONTINOUS = continous_distribution_plot(df_probability = df_probability,
                                              distribution_functions = distribution_functions, 
                                              distribution_args = distribution_args,
                                              interval_distribution = interval_distribution,
                                              x_dist = x_dist, 
                                              a_dist = a_dist, 
                                              b_dist = b_dist),
      DISCRETE = discrete_distribution_plot(df_probability = df_probability)
    )
  }
  result %>% unname() %>% unlist(recursive = FALSE) %>% 
    pluck(distribution_choice) %>% return()
}
# table_plot_distribution(distribution_choice = "hypergeometric",
#                         interval_distribution = "above_x",
#                         return = "table",
#                         M_dist = 50,
#                         N_dist = 500,
#                         n_dist = 10,
#                         x_dist = 2.4)
# table_plot_distribution(distribution_choice = "hypergeometric",
#                         interval_distribution = "interval",
#                         return = "plot",
#                         M_dist = 50,
#                         N_dist = 500,
#                         n_dist = 10,
#                         a_dist = 1,
#                         b_dist = 2)
# table_plot_distribution(distribution_choice = "gamma",
#                         interval_distribution = "above_x",
#                         return = "plot",
#                         shape = 3,
#                         rate = 1,
#                         x_dist = 2.4)
# table_plot_distribution(distribution_choice = "gamma",
#                         interval_distribution = "interval",
#                         return = "plot",
#                         shape = 3,
#                         rate = 1,
#                         a_dist = 1,
#                         b_dist = 2)
# table_plot_distribution(distribution_choice = "geometric2",
#                         interval_distribution = "above_x",
#                         return = "table",
#                         p_dist = 0.5,
#                         x_dist = 2.4)
# table_plot_distribution(distribution_choice = "geometric2",
#                         interval_distribution = "interval",
#                         return = "plot",
#                         p_dist = 0.5,
#                         a_dist = 1,
#                         b_dist = 2)
# table_plot_distribution(distribution_choice = "cauchy",
#                         interval_distribution = "above_x",
#                         return = "plot",
#                         location = 1,
#                         scale = 1,
#                         x_dist = 2.4)
# table_plot_distribution(distribution_choice = "cauchy",
#                         interval_distribution = "interval",
#                         return = "plot",
#                         location = 1,
#                         scale = 1,
#                         a_dist = 1,
#                         b_dist = 2)

#' @SUBFUNCTION
#' @description 
#' 1. filter from the list only the discrete distributions
#' 2. Consider mistakes 
#' 3. Plot each discrete distribution
#' @param df_probability dataset with all the distributions
#' @return the plot of each discrete distribution
discrete_distribution_plot <- possibly(function(df_probability){
  
  options(warn = -1) 
  
  names_discretes <- pluck(dist_types, "DISCRETE")
  
  arguments_discrete <- pluck(df_probability, "DISCRETE")
  arguments_discrete <- map(names_discretes, 
                            ~(
                              if(is.null(pluck(arguments_discrete, .x)$PROBABILITY)) 
                                NULL
                              else pluck(arguments_discrete, .x)
                            )) %>% set_names(names_discretes)
  map(arguments_discrete,
      ~ (
        if(is.null(.x)) NULL
        else{
          if(any(is.na(.x[,ID]))) NULL
          else
            .x %>% ggplot(aes(x = as.factor(RANDOM_VARIABLE),
                              y = PROBABILITY,
                              fill = ID)) +
            geom_col(alpha = 0.8) + 
            geom_text(aes(label = round(PROBABILITY, digits = 3), #text the probability in each bar
                          y = PROBABILITY + 0.005),
                      colour = "white",
                      position = position_dodge(0.9),
                      size = 3, # size of text
                      vjust = 0)
        }
      )) %>% map2(names_discretes, 
                  ~(
                    if(is.null(.x)) .x
                    else{
                      plot <- .x + labs(x = "X",
                                        y = "MASS FUNCTION",
                                        title = glue("PLOT OF {toupper(.y)}"))
                      plot %>% design_plot()
                    }
                  )) %>% return() 
}, otherwise = "NO DISCRETE DISTRIBUTION")

#' @SUBFUNCTION
#' @description 
#' 1. filter only continous distribution information 
#' 2. consider mistakes
#' 3. Plot of each continous distribution
#' 4. Plot the area under the curve of each continous distribution
#' 5. Define the labels names of each continous distribution
#' @param df_probability dataset with all the distributions
#' @param x_dist,a_dist,b_dist elements of X to delimit the area under the curve of  X
#' @param distribution_functions,distribution_args arguments to make calculations
#' @param interval_distribution type of tail where the output is going to be
#' @return the plot of each continous distribution
continous_distribution_plot <- function(df_probability,
                                        distribution_functions, 
                                        distribution_args,
                                        interval_distribution,
                                        x_dist, 
                                        a_dist, 
                                        b_dist){
  options(warn = -1) 
  
  arguments_continous <- 
    map(list(df_probability, distribution_functions, distribution_args),
        ~pluck(.x, "CONTINOUS")
    )
  pluck(arguments_continous, 2) <- pluck(pluck(arguments_continous, 2), "DENSITY")
  
  plot_continous <- pmap(arguments_continous, 
                         safely(function(data, f, args){
                           if(is.null(data$RANDOM_VARIABLE)) NULL
                           else{
                             ggplot(data, aes(x = RANDOM_VARIABLE)) + 
                               stat_function(fun = f,
                                             args = args,
                                             colour = "orange") + 
                               theme_minimal() +
                               theme(
                                 plot.title = element_text(face = "italic", 
                                                           hjust = 0.5),
                                 axis.title = element_text(face = "italic"),
                                 axis.ticks = element_line(color = "red"),
                                 axis.line = element_line(color = "red"),
                                 axis.text = element_text(colour = "white"),
                                 text = element_text(colour = "white"),
                                 plot.background = element_rect(fill = "#2D3741",
                                                                colour = NA)
                               ) 
                           }
                         })) %>% map("result")
  
  pmap(list(plot_continous,
            arguments_continous[[2]],
            arguments_continous[[3]]),
       safely(function(plot, f, args){
         
         area_under_curve <- function(x){
           conditions_area(interval_distribution = interval_distribution,
                           density = do.call(what = f, 
                                             args = args %>% 
                                               append(list(x = x))),
                           x = x, 
                           x_dist = x_dist,
                           a_dist = a_dist,
                           b_dist = b_dist)}
         plot + stat_function(fun = area_under_curve, 
                              geom = "area", 
                              alpha = 0.8, 
                              fill = "#f8766d")
       })) %>% map("result") %>% 
    map2(pluck(dist_types, "CONTINOUS"), 
         ~ (if(is.null(.x)) NULL
            else
              .x + labs(x = "X",
                        y = "DENSITY",
                        title = glue("PLOT OF {toupper(.y)}"))
         )) %>% return()
}
#' @SUBFUNCTION
#' @description Define the conditions of the interval to generate the area under the curve
#' @param interval_distribution type of tail where the output is going to be
#' @param density type of density function 
#' @param x the random variable 
#' @param x_dist,a_dist,b_dist elements of X to delimit the area under the curve of  X
#' @return the conditions to make the area under the curve in *continous_distribution_plot*
conditions_area <- function(interval_distribution, density, x, 
                            x_dist = NULL, a_dist = NULL, b_dist = NULL){
  if(interval_distribution == "under_x"){
    density[x > x_dist] <- NA
  }else if(interval_distribution == "above_x"){
    density[x < x_dist] <- NA
  }else if(interval_distribution == "interval"){
    density[x < a_dist | x > b_dist] <- NA
  }
  return(density)
}
# conditions_area(interval_distribution = "interval",
#                 x = 3, density = dexp(x = 3, rate = 1))
# conditions_area(interval_distribution = "interval",
#                 x = 3,
#                 density = do.call(what = dexp, args = list(x = 3, rate = 1)))

# Distribution Function ---------------------------------------------------
#' @description 
#' 1. list the arguments and other with the functions
#' 2. Define the first part of the representation of the distribution and the probability in a string of each distribution
#' 3. Calculate the value of the distribution function of each random variable
#' 4. paste 2. with 3. and represent it with mathjax of an html representation.
#' @param distribution_choice type of distribution (discrete or continous)
#' @param interval_distribution type of tail where the output is going to be
#' @param arguments of each distribution
#' @param x_dist,a_dist,b_dist x element of X, represent the inflection point(s)
#' @return html representation of the discrete or continous distribution 
distribution_representation <- function(
  distribution_choice = dist_types %>% flatten_chr(),
  interval_distribution = c("under_x", "above_x", "interval"),
  n_dist = NULL, p_dist = NULL, 
  M_dist = NULL, N_dist = NULL, 
  lambda_dist = NULL, rate = NULL,
  x_mean = NULL, x_sd = NULL, 
  scale = NULL, shape = NULL, 
  deg_free = NULL, deg_free2 = NULL,
  location = NULL, x_dist = NULL,
  a_dist = NULL, b_dist = NULL){
  
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  distribution_representation <- map(c("DISCRETE", "CONTINOUS"),
                                     ~ map(pluck(distribution_json, .x), 
                                           "DISTRIBUTION"))
  distribution_args <- list(
    DISCRETE = list(list(size = n_dist, prob = p_dist),
                    list(m = M_dist, n = (N_dist-M_dist), k = n_dist),
                    list(lambda = lambda_dist)) %>% 
      append(list_of_lists(no_sublists = 2, element_sublists = list(prob = p_dist))) %>% 
      append(list_of_lists(no_sublists = 2, element_sublists = list(size = n_dist, 
                                                                    prob = p_dist))),
    CONTINOUS = list(list(rate = rate), 
                     list(shape = shape, rate = rate),
                     list(df = deg_free), 
                     list(shape1 = scale, shape2 = shape),
                     list(shape = shape, scale = scale), 
                     list(df = deg_free),
                     list(location = location, scale = scale), 
                     list(mean = x_mean, sd = x_sd),
                     list(meanlog = x_mean, sdlog = x_sd), 
                     list(location = location, scale = scale),
                     list(df1 = deg_free, df2 = deg_free2))
  )
  distribution_representation <- 
    map(1:length(distribution_representation), 
        function(i){
          repre <- pluck(distribution_representation, i)
          args <- pluck(distribution_args, i)
          
          map2(repre, args,
               ~  map2(..1[1:(length(..1)-1)], 
                       ..2,
                       ~glue("{.x} {.y}")) %>% unlist() %>% 
                 str_c(collapse = " , ") %>% 
                 str_c(tail(..1, n = 1))
          )
        }) %>% map2(dist_types, ~.x %>% set_names(.y))
  
  probability_representation <- pluck(distribution_json, "TAIL") %>% 
    map(interval_distribution)
  probability_representation <- 
    pmap(list(
      probability_representation,
      list(x_dist, round((x_dist-x_mean)/x_sd, digits = 2)),
      list(a_dist, round((a_dist-x_mean)/x_sd, digits = 2)),
      list(b_dist, round((b_dist-x_mean)/x_sd, digits = 2))),
      
      function(representation, value_x, value_a, value_b){
        paste(representation[1],
              if(!is.null(x_dist)) value_x else value_a,
              representation[2],
              if(!is.null(b_dist)) value_b else "",
              representation[3],
              ifelse(is.na(representation[4]), "", representation[4]))
      })
  
  representation <-   
    map(distribution_representation, 
        ~map(..1, 
             ~glue("{.x} and {pluck(probability_representation, 'GENERAL')}")
        )
    ) %>% unlist(recursive = FALSE)
  pluck(representation, "normal") <- paste(pluck(representation, "normal"),
                                           pluck(probability_representation, 'Z'))
  
  representation_values <- 
    values_dist_representation(distribution_args,
                               interval_distribution,
                               x_dist = ifelse(is.null(x_dist), NA, x_dist),
                               a_dist = ifelse(is.null(a_dist), NA, a_dist), 
                               b_dist = ifelse(is.null(b_dist), NA, b_dist),
                               n_dist = ifelse(is.null(n_dist), NA, n_dist))
  
  if(length(representation) != length(representation_values)) result <- NULL
  else result <- map2(representation, representation_values, ~ glue("{.x} {.y}")
  )%>% pluck(distribution_choice) %>% withMathJax()
  
  result
}
# distribution_representation(distribution_choice = "hypergeometric",
#                             interval_distribution = "under_x",
#                             M_dist = 50,
#                             N_dist = 500,
#                             n_dist = 10,
#                             x_dist = 2.4)
# distribution_representation(distribution_choice = "hypergeometric",
#                             interval_distribution = "interval",
#                             M_dist = 50,
#                             N_dist = 500,
#                             n_dist = 10,
#                             a_dist = 1,
#                             b_dist = 2)
# distribution_representation(distribution_choice = "gamma",
#                             interval_distribution = "above_x",
#                             shape = 3,
#                             rate = 1,
#                             x_dist = 2.4)
# distribution_representation(distribution_choice = "gamma",
#                             interval_distribution = "interval",
#                             shape = 3,
#                             rate = 1,
#                             a_dist = 1,
#                             b_dist = 2)

#' @SUBFUNCTION
#' @description 
#' 1. define the list of each distribution function 
#' 2. calculate the distribution for under and above an x value of X
#' 3. sorted the list to have a sublist of under values and the other sublist with the above values
#' 4. calculate the distribution for an interval of X
#' 5. sorted the list with the calculations of (b-a)
#' 6. join the lists to bring only one list with three sublists of values. Rounding each value.
#' @param distribution_args,n_dist arguments to make calculations
#' @param interval_distribution type of tail where the output is going to be
#' @param x_dist,a_dist,b_dist elements of X to delimit the area under the curve of  X
#' @return list of the values of distribution function of each distribution
values_dist_representation <- function(distribution_args,
                                       interval_distribution,
                                       x_dist = NA,
                                       a_dist = NA, 
                                       b_dist = NA,
                                       n_dist = NA){
  distribution_functions <- list(
    DISCRETE = list(safely(pbinom), safely(phyper), safely(ppois),
                    safely(pgeom), safely(pgeom), safely(pnbinom),
                    safely(pnbinom)),
    CONTINOUS = list(safely(pexp), safely(pgamma), safely(pchisq), 
                     safely(pbeta), safely(pweibull), safely(pt), 
                     safely(plogis), safely(pnorm), safely(plnorm), 
                     safely(pcauchy), safely(pf))
  )
  
  under_above_values <- 
    values_dist_representation_quantils(x_dist = x_dist,  
                                        n_dist = n_dist,
                                        distribution_functions = distribution_functions, 
                                        distribution_args = distribution_args,
                                        quantiles ="x_dist")
  
  under_above <- map(c("under_x", "above_x"), function(i){
    map(c("DISCRETE", "CONTINOUS"), 
        ~ map(pluck(under_above_values, .x), i)
    ) %>% unlist(recursive = FALSE)
  }) %>% set_names("under_x", "above_x")
  
  interval_values <- 
    values_dist_representation_quantils(a_dist = a_dist, 
                                        b_dist = b_dist, 
                                        n_dist = n_dist, 
                                        distribution_functions = distribution_functions, 
                                        distribution_args = distribution_args,
                                        quantiles ="interval")
  interval <- map(c("DISCRETE", "CONTINOUS"),
                  ~  map(pluck(interval_values, ..1),
                         ~ ifelse(pluck(.x, "a_dist")<=pluck(.x, "b_dist"),
                                  pluck(.x, "b_dist")-pluck(.x, "a_dist"),
                                  "a must be less than or equal to b"
                         )
                  )
  ) %>% unlist(recursive = FALSE)
  
  under_above %>% append(list(interval = interval)) %>% 
    pluck(interval_distribution) %>% 
    map(safely(~round(.x, digits = 4))) %>% map("result") %>% 
    return()
}
# values_dist_representation(distribution_args = distribution_args,
#                            interval_distribution = "interval",
#                            a_dist = 12,
#                            b_dist = 14,
#                            n_dist = 10)
# values_dist_representation(distribution_args = distribution_args,
#                            interval_distribution = "under_x",
#                            x_dist = 2.24)

#' @SUBFUNCTION
#' @description 
#' 1. Bring a list of values where we want to calculate the distribution function
#' 2. make the calculation of the distribution in each value of each distribution
#' 3. select the type of result to bring based on the quantiles argument.
#' @param x_dist,a_dist,b_dist elements of X to delimit the area under the curve of  X
#' @param distribution_functions,distribution_args,n_dist arguments to make calculations
#' @param quantiles type of values we want to return 
#' @return the value of the distribution function of the random variable of each distribution (above and under X or an interval)
values_dist_representation_quantils <- function(x_dist = NA, 
                                                a_dist = NA, 
                                                b_dist = NA,
                                                n_dist = NA, 
                                                distribution_functions, 
                                                distribution_args,
                                                quantiles = c("interval", "x_dist")){
  values = list(
    DISCRETE = map(c(x_dist, (a_dist-1), b_dist), 
                   ~ list_of_lists(no_sublists = length(pluck(dist_types, "DISCRETE")[c(1:4,6)]), #"binomial", "hypergeometric", "poisson", "geometric1","negative1"
                                   element_sublists = .x) %>% 
                     set_names(pluck(dist_types, "DISCRETE")[c(1:4,6)])
    ), 
    
    CONTINOUS = map(c(x_dist, a_dist, b_dist), 
                    ~ list_of_lists(no_sublists = length(pluck(dist_types, "CONTINOUS")),
                                    element_sublists = .x) %>% 
                      set_names(pluck(dist_types, "CONTINOUS"))
    )
  )
  values <- map(values, ~.x %>% set_names("x_dist", "a_dist", "b_dist"))
  
  for(i in 1:3){
    element <- c("x_dist", "a_dist", "b_dist")
    val_geometric2 <- c((x_dist-1), (a_dist-2), (b_dist-1))
    val_negative2 <- c((x_dist-n_dist), (a_dist-n_dist-1), (b_dist-n_dist))
    
    pluck(values[["DISCRETE"]], element[i]) <- 
      pluck(values[["DISCRETE"]], element[i]) %>% 
      append(list(geometric2 = val_geometric2[i],
                  negative2 = val_negative2[i])) 
  }
  values <- map(c("DISCRETE", "CONTINOUS"), function(i){ #we need to sort each sublist based in the order of dist_types to work the next steps
    map(pluck(values, i), ~.x[pluck(dist_types, i)])
  }) %>% set_names("DISCRETE", "CONTINOUS")
  
  values <- map(c("DISCRETE", "CONTINOUS"), function(i){
    map2(c("x_dist", "a_dist", "b_dist"),
         list(list_of_lists(no_sublists = length(pluck(dist_types, i)),
                            element_sublists = c(TRUE, FALSE)),
              list_of_lists(no_sublists = length(pluck(dist_types, i)),
                            element_sublists = TRUE),
              list_of_lists(no_sublists = length(pluck(dist_types, i)),
                            element_sublists = TRUE)),
         ~ pmap(list(pluck(distribution_functions, i),
                     pluck(distribution_args, i),
                     pluck(values[[i]], ..1),
                     ..2), 
                function(func, args, val, tail){
                  map(tail,
                      ~do.call(what = func,
                               args = args %>% 
                                 append(list(q = val,
                                             lower.tail = .x))
                      ) %>% pluck("result")
                  )
                }) 
    ) %>% map2(list(c("under_x", "above_x"), "interval", "interval"), 
               function(i,j){
                 map(i, ~ .x %>% set_names(j))
               }) %>% set_names("x_dist", "a_dist", "b_dist") 
  }) %>% set_names("DISCRETE", "CONTINOUS")
  
  if(quantiles == "interval"){
    result <- map(c("DISCRETE", "CONTINOUS"), function(i){
      
      each <- map(c("a_dist", "b_dist"), ~pluck(pluck(values, i), .x)) %>% 
        map(~ map2(..1, pluck(dist_types, i), ~.x %>% set_names(.y))
        ) %>% map(~.x %>% unlist(recursive = FALSE)) %>% 
        set_names("a_dist", "b_dist")
      
      map(pluck(dist_types, i), ~map(each, .x)) %>% 
        set_names(pluck(dist_types, i))
    }) %>% set_names("DISCRETE", "CONTINOUS")
    
  }else{
    result <- map(values, quantiles) %>% map2(dist_types, ~.x %>% set_names(.y))
  }
  result   
} 
# values_dist_representation_quantils(a_dist = 12,
#                                     b_dist = 12,
#                                     n_dist = NULL,
#                                     distribution_functions = distribution_functions,
#                                     distribution_args = distribution_args,
#                                     quantiles ="interval")
# values_dist_representation_quantils(x_dist = 2.24,
#                                     n_dist = NULL,
#                                     distribution_functions = distribution_functions,
#                                     distribution_args = distribution_args,
#                                     quantiles ="x_dist")

# Formula Distribution Function -------------------------------------------
#' @description iterate over each distribution to bring the formula representation.
#' @param distribution_choice type of distribution (discrete or continous)
#' @return html formula of the representaion of the distribution of a selected distribution.
distribution_function_representation <- function(
  distribution_choice = dist_types %>% flatten_chr()){
  
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  function_representation <- 
    pmap(list(c("DISCRETE", "CONTINOUS"),
              c("Probability Mass Function", "Probability Density Function"),
              dist_types), 
         function(type, representation, names){
           map2(map(pluck(distribution_json, type), "FUNCTION"),
                map(pluck(distribution_json, type), "PARAMETERS"),
                ~withMathJax(
                  helpText(glue("{representation}: {.x}")),
                  br(),
                  helpText(glue("where {.y}"))
                )) %>% set_names(names)
         }) 
  pluck(function_representation %>% unlist(recursive = F) ,
        distribution_choice) %>% return()
}
# distribution_function_representation(distribution_choice = "hypergeometric")
# distribution_function_representation(distribution_choice = "student")

# Summary Distribution Function -------------------------------------------
#' @description 
#' 1. Define the first part of the representation of the summary stats in a string of each distribution.
#' 2. use the formula values_summary_representation to bring the calculations
#' 3. paste the first part and the calculation in an HTML format 
#' @param distribution_choice type of distribution (discrete or continous)
#' @param arguments of each distribution
#' @return html representation of the formula and calculation for the mean, var, sd of a specific distribution
summary_representation <- function(
  distribution_choice = dist_types %>% flatten_chr(),
  n_dist = NULL, p_dist = NULL, 
  M_dist = NULL, N_dist = NULL, 
  lambda_dist = NULL, rate = NULL,
  x_mean = NULL, x_sd = NULL, 
  scale = NULL, shape = NULL, 
  deg_free = NULL, deg_free2 = NULL,
  location = NULL){
  
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  representation <- map(c("DISCRETE", "CONTINOUS"), function(i){
    
    each <- map2(pluck(distribution_json, "SUMMARY"), 
                 map(c("MEAN", "SD", "VAR"), ~ map(pluck(distribution_json, i), .x)),
                 ~glue("{.x} {.y}")
    )
    map(1:length(pluck(each, 1)), function(i){
      map(each, i) }) %>% map(~ .x %>% set_names("MEAN", "SD", "VAR"))
  }) %>% map2(dist_types, ~ .x %>% set_names(.y)) %>% unlist(recursive = FALSE)
  
  representation_values <- values_summary_representation(
    n_dist = n_dist, p_dist = p_dist, 
    M_dist = M_dist, N_dist = N_dist, 
    lambda_dist = lambda_dist, rate = rate,
    x_mean = x_mean, x_sd = x_sd, 
    scale = scale, shape = shape, 
    deg_free = deg_free, deg_free2 = deg_free2,
    location = location)
  
  map2(representation, representation_values,  
       ~ glue("{.x} {.y}")) %>% 
    map(~ withMathJax(
      helpText(.x[[1]]),
      br(),
      helpText(.x[[2]]),
      br(),
      helpText(.x[[3]])
    )) %>% pluck(distribution_choice) %>% return()
}
# summary_representation(distribution_choice = "hypergeometric",
#                        M_dist = 50,
#                        N_dist = 500,
#                        n_dist = 10)
# summary_representation(distribution_choice = "gamma",
#                        shape = 3,
#                        rate = 1)

#' @SUBFUNCTION
#' @description list of formulas to calculate for mean, var, sd each distribution 
#' @param arguments of each distribution
#' @return a list of calculations of mean, var, sd of each distribution.
values_summary_representation <- function(
  n_dist = NULL, p_dist = NULL, 
  M_dist = NULL, N_dist = NULL, 
  lambda_dist = NULL, rate = NULL,
  x_mean = NULL, x_sd = NULL, 
  scale = NULL, shape = NULL, 
  deg_free = NULL, deg_free2 = NULL,
  location = NULL){
  
  summary_functions <- list(
    DISCRETE = list(
      MEAN = list(binomial = n_dist*p_dist, 
                  hypergeometric = n_dist*(M_dist/N_dist), 
                  poisson = lambda_dist, 
                  geometric1 = (1-p_dist)/p_dist, 
                  geometric2 = 1/p_dist, 
                  negative1 = (n_dist*(1-p_dist))/p_dist, 
                  negative2 = n_dist/p_dist),
      VAR = list(binomial = n_dist*p_dist*(1-p_dist), 
                 hypergeometric = n_dist*(M_dist/N_dist)*
                   (1-(M_dist/N_dist))*((N_dist-n_dist)/(N_dist-1)), 
                 poisson = lambda_dist, 
                 geometric1 = (1-p_dist)/(p_dist^2), 
                 geometric2 = (1-p_dist)/(p_dist^2), 
                 negative1 = (n_dist*(1-p_dist))/(p_dist^2), 
                 negative2 = (n_dist*(1-p_dist))/(p_dist^2))
    ),
    CONTINOUS = list(
      MEAN = list(exponential = 1/rate, 
                  gamma = shape/rate, 
                  chi_square = deg_free, 
                  beta = scale/(scale + shape), 
                  weibull = safely(weibullparinv)
                  (shape = shape, scale = scale, loc = 0)[["result"]]$mu, 
                  student = ifelse(deg_free > 1, 0, "Undefined"), 
                  logistic = location, 
                  normal = x_mean,
                  log_normal = exp(x_mean+((x_sd^2)/ 2)), 
                  cauchy = "", 
                  fisher = ifelse(deg_free2 > 2, deg_free2/(deg_free2-2), "Undefined")),
      VAR = list(exponential = (1/rate)^2, 
                 gamma = shape/(rate^2), 
                 chi_square = 2*deg_free, 
                 beta = (scale * shape)/(((scale + shape)^2)*(scale + shape + 1)), 
                 weibull = safely(weibullparinv)
                 (shape = shape, scale = scale, loc = 0)[["result"]]$sigma^2, 
                 student = ifelse(deg_free > 2, deg_free/(deg_free-2), "Undefined"), 
                 logistic = ((scale^2)*(pi^2))/3, 
                 normal = x_sd^2,
                 log_normal = (exp(x_sd^2)-1)*(exp((2*x_mean)+(x_sd^2))), 
                 cauchy = "", 
                 fisher = ifelse(deg_free2 > 4, ((2*(deg_free2^2))*(deg_free+deg_free2-2))/
                                   (deg_free*((deg_free2-2)^2)*(deg_free2-4)), 
                                 "Undefined"))
    )
  )
  SD_summaries <- map(c("DISCRETE", "CONTINOUS"),
                      ~  pluck(summary_functions, ..1) %>% 
                        pluck("VAR")) %>% 
    map(~map(..1, safely(~ifelse(is.character(.x), .x, sqrt(.x)))) %>% 
          map("result")
    ) %>% set_names("DISCRETE", "CONTINOUS")
  
  result <- map(c("DISCRETE", "CONTINOUS"), 
                ~ pluck(summary_functions, .x) %>% 
                  append(list(SD = pluck(SD_summaries, .x)))
  ) %>% set_names("DISCRETE", "CONTINOUS") 
  
  map(1:length(result), function(i){
    map(pluck(dist_types, i), ~ map(pluck(result, i), .x)) %>% 
      map(~.x[c("MEAN","SD","VAR")]) #order elements
  }) %>% map2(dist_types, ~.x %>% set_names(.y)) %>% 
    unlist(recursive = FALSE) %>% 
    map(~ map(..1, function(x){
      if(is.character(x) | is.null(x)) x
      else round(x, digits = 3)
    })
    ) %>% return()
}
# values_summary_representation(shape = 3,
#                               rate = 1)
