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
# distribution_choice <- "exponential"

# Continous Distribution --------------------------------------------------
continous_distribution <- function(distribution_choice = c("exponential", "gamma",
                                                           "chi_square", "beta",
                                                           "weibull", "student",
                                                           "logistic", "normal",
                                                           "log_normal", "cauchy",
                                                           "fisher"),
                                   interval_distribution = c("under_x", "above_x", "interval"),
                                   return = c("table", "plot"),
                                   x_mean = NULL, x_sd = NULL, rate = NULL,
                                   scale = NULL, shape = NULL, location = NULL,
                                   deg_free = NULL, deg_free2 = NULL, x_dist = NULL,
                                   a_dist = NULL, b_dist = NULL){
  
  options(scipen = 999, warn = -1) #avoid scientific notation and warnings
  
  distribution_functions <- list(QUANTILE = 
                                   list(safely(qexp), safely(qgamma), safely(qchisq),
                                        safely(qbeta), safely(qweibull), safely(qt),
                                        safely(qlogis), safely(qnorm)),
                                 DENSITY = 
                                   list(dexp, dgamma, dchisq,
                                        dbeta, dweibull, dt,
                                        dlogis, dnorm, dlnorm,
                                        dcauchy, df))
  
  distribution_args <- list(list(rate = rate), 
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
  
  distribution_args_quantile <- map(1:(length(distribution_args)-3), function(i){ #not for log_normal, cauchy, fisher
    each <- pluck(distribution_args, i)
    each %>% append(list(p = 0.99999))
  }) 
  quantile_values <- map(c(FALSE, TRUE), 
                         ~invoke_map(distribution_functions[["QUANTILE"]],
                                     distribution_args_quantile, 
                                     lower.tail = .x)) %>% 
    map(~map(.x,"result")) %>% set_names("upper_tail", "under_tail") 
  
  df_func_probability <- map(1:length(pluck(quantile_values, 1)), #upper or under that is pluck(,1) to make map(quantile_values, 1) and get a sublist of upper_tail and under_tail
                             function(i){map(quantile_values, i)}) %>% 
    map(~general_sequence(from = .x$upper_tail, to = .x$under_tail)) %>% 
    append(list(#for log_normal, cauchy, fisher
      general_sequence(from = 0, to = do.call(what = qlnorm, 
                                              args = distribution_args[[9]] %>% 
                                                append(list(p = 0.9)))),
      general_sequence(from = location - (6 * scale), to = location + (6 * scale)),
      general_sequence(from = 0, to = 5)
    )) %>% map(~data.table(RANDOM_VARIABLE = .x)) 
  
  distribution_args_density <- map(1:length(distribution_args), function(i){ #we add the random variable to the arguments of each from distribution_args 
    each <- pluck(distribution_args, i)
    each %>% append(map(df_func_probability, ~.x[,RANDOM_VARIABLE])[i])
  }) 
  df_func_probability <- map2(df_func_probability, 
                              invoke_map(map(distribution_functions[["DENSITY"]], 
                                             ~safely(.x)),
                                         distribution_args_density) %>% map("result"),
                              safely(~.x[,PROBABILITY:=.y])) %>% 
    map("result") %>% 
    set_names("exponential", "gamma", "chi_square","beta", "weibull", "student", 
              "logistic", "normal", "log_normal",  "cauchy","fisher")
  
  df_func_probability <- map(df_func_probability,
                             safely(~copy(.x)[,ID:=map_chr(RANDOM_VARIABLE, function(X){
                               if(interval_distribution == "under_x"){
                                 ifelse(X <= x_dist, "INSIDE", "OUTSIDE")
                               }else if(interval_distribution == "above_x"){
                                 ifelse(X > x_dist, "INSIDE", "OUTSIDE")
                               }else if(interval_distribution == "interval"){
                                 ifelse(X >= a_dist & X <= b_dist, 
                                        "INSIDE", "OUTSIDE")
                               }
                             })])) %>% map("result")
  plot_func_probability <- pmap(list(df_func_probability, 
                                     distribution_functions[["DENSITY"]],
                                     distribution_args),
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
  plot_func_probability <- 
    pmap(list(plot_func_probability,
              distribution_functions[["DENSITY"]],
              distribution_args), 
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
         })) %>% map("result")
  
  if(return == "table"){
    result <- pluck(df_func_probability, distribution_choice) %>% 
      desing_DT()
    
  }else if(return == "plot"){
    result <- pluck(plot_func_probability, distribution_choice) 
    result <- 
      if(is.null(result)) NULL 
    else result + 
      labs(x = "X",
           y = "DENSITY",
           title = glue("PLOT OF {toupper(distribution_choice)}"))
  }
  result
}
# continous_distribution(distribution_choice = "gamma",
#                        interval_distribution = "under_x",
#                        return = "plot",
#                        shape = 3,
#                        rate = 1,
#                        x_dist = 2.4)
# continous_distribution(distribution_choice = "gamma",
#                        interval_distribution = "interval",
#                        return = "plot",
#                        shape = 3,
#                        rate = 1,
#                        a_dist = 1,
#                        b_dist = 2)

#' @SUBFUNCTION
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
#                 density = do.call(what = dexp, args = distribution_args[[1]] %>% append(list(x = 3))))

distribution_representation <- function(distribution_choice = c("exponential", "gamma",
                                                                "chi_square", "beta",
                                                                "weibull", "student",
                                                                "logistic", "normal",
                                                                "log_normal", "cauchy",
                                                                "fisher"),
                                        interval_distribution = c("under_x", "above_x", "interval"),
                                        x_mean = NULL, x_sd = NULL, rate = NULL,
                                        scale = NULL, shape = NULL, location = NULL,
                                        deg_free = NULL, deg_free2 = NULL, x_dist = NULL,
                                        a_dist = NULL, b_dist = NULL){
  
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  distribution_representation <- map(distribution_json[["CONTINOUS"]], 
                                     "DISTRIBUTION")
  
  distribution_args <- list(list(rate = rate), 
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
  
  distribution_representation <- 
    map2(distribution_args, distribution_representation, 
         ~if(length(.x) == 1){
           glue("{head(.y, n = 1)} {.x} {tail(.y, n = 1)}")
         }else{
           glue("{head(.y, n = 1)} {.x[1]} , {.y[2]} {.x[2]} , {tail(.y, n = 1)}")
         }
    ) %>% set_names("exponential", "gamma", "chi_square","beta", "weibull", "student", 
                    "logistic", "normal", "log_normal",  "cauchy","fisher")
  
  probability_representation <- map(c("GENERAL", "Z"),
                                    ~pluck(distribution_json[["TAIL"]], .x) %>%
                                      pluck(interval_distribution)
  )
  probability_representation <- pmap(list(
    probability_representation,
    list(x_dist, round((x_dist-x_mean)/x_sd, digits = 2)),
    list(a_dist, round((a_dist-x_mean)/x_sd, digits = 2)),
    list(b_dist, round((b_dist-x_mean)/x_sd, digits = 2))),
    
    function(representation, value_x, value_a, value_b){
      paste(representation[1],
            if(!is.null(x_dist)) value_x else value_a,
            representation[2],
            if(!is.null(b_dist)) value_b else NULL,
            representation[3],
            ifelse(is.na(representation[4]), "", representation[4]))
    }) %>%
    set_names("probability_representation", "z_representation")
  
  representation <- map(distribution_representation, 
                        ~str_c(.x, 
                               probability_representation[['probability_representation']],
                               sep = " and "))
  
  representation[["normal"]] <- str_c(representation[["normal"]], 
                                      probability_representation[["z_representation"]], 
                                      sep = " = ")
  
  dist_func_value <- specific_dist_repre(distribution_args = distribution_args,
                                         interval_distribution = interval_distribution,
                                         x_dist = x_dist, 
                                         a_dist = a_dist, 
                                         b_dist = b_dist)
  
  if(length(representation) != length(dist_func_value)) NULL %>% return()
  else  map2(representation, dist_func_value, ~ glue("{.x} {.y}")) %>% 
    pluck(distribution_choice) %>% withMathJax() %>% return()
}
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
specific_dist_repre <- function(distribution_args,
                                interval_distribution,
                                x_dist = NULL, 
                                a_dist = NULL, 
                                b_dist = NULL){
  
  distribution_functions <- list(safely(pexp), safely(pgamma), safely(pchisq), 
                                 safely(pbeta), safely(pweibull), safely(pt), 
                                 safely(plogis), safely(pnorm), safely(plnorm), 
                                 safely(pcauchy), safely(pf)) 
  
  dist_func_value <- map(c(TRUE, FALSE),
                         ~invoke_map(distribution_functions,
                                     distribution_args,
                                     lower.tail = .x,
                                     q = x_dist)) %>% map(~map(.x, "result")) %>% 
    set_names("under_x", "above_x")
  
  interval <- map(c(a_dist, b_dist),
                  ~invoke_map(distribution_functions,
                              distribution_args,
                              lower.tail = TRUE,
                              q = .x)) %>% map(~map(.x, "result"))
  
  interval <- map(1:length(pluck(interval, 1)), function(i){ #same explanation of pluck because the length 
    each <- map(interval, i)
    
    ifelse(a_dist <= b_dist, 
           each[[2]]-each[[1]], #b_dist-a_dist
           "a must be less than or equal to b") %>%  
      return()
  }) 
  
  dist_func_value %>% append(list(interval = interval)) %>% 
    pluck(interval_distribution) %>% map(safely(~round(.x, digits = 4))) %>% 
    map("result") %>% return()
}
# specific_dist_repre(distribution_args = distribution_args,
#                     interval_distribution = "above_x",
#                     x_dist = 2.4)
# specific_dist_repre(distribution_args = distribution_args,
#                     interval_distribution = "interval",
#                     a_dist = 1,
#                     b_dist = 2)

distribution_function_representation <- function(
  distribution_choice = c("exponential", "gamma",
                          "chi_square", "beta",
                          "weibull", "student",
                          "logistic", "normal",
                          "log_normal", "cauchy",
                          "fisher")){
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  function_representation <- 
    map2(map(distribution_json[["CONTINOUS"]], "FUNCTION"),
         map(distribution_json[["CONTINOUS"]], "PARAMETERS"),
         ~withMathJax(
           helpText(glue("Probability density function: {.x}")),
           br(),
           helpText(glue("where {.y}"))
         )) %>% 
    set_names("exponential", "gamma", "chi_square","beta", "weibull", "student", 
              "logistic", "normal", "log_normal",  "cauchy","fisher")
  
  pluck(function_representation, distribution_choice) %>% return()
}
#distribution_function_representation(distribution_choice = "chi_square")

summary_representation <- function(
  distribution_choice = c("exponential", "gamma",
                          "chi_square", "beta",
                          "weibull", "student",
                          "logistic", "normal",
                          "log_normal", "cauchy",
                          "fisher"),
  x_mean = NULL, x_sd = NULL, rate = NULL,
  scale = NULL, shape = NULL, location = NULL,
  deg_free = NULL, deg_free2 = NULL){
  
  distribution_json <- 
    jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  conti_dist <- distribution_json[["CONTINOUS"]]
  
  summary_repre <- map(names(conti_dist), function(each){
    
    map2(distribution_json[["SUMMARY"]],
         map(c("MEAN", "SD", "VAR"),
             ~conti_dist[[each]][[.x]]),
         
         ~glue("{.x} {.y}")) %>% set_names("MEAN", "SD", "VAR")
  }) %>% 
    set_names("exponential", "gamma", "chi_square","beta", "weibull", "student", 
              "logistic", "normal", "log_normal",  "cauchy","fisher")
  
  summary_repre_value <- specific_summary_repre(x_mean = x_mean, x_sd = x_sd, 
                                                rate = rate, scale = scale, 
                                                shape = shape, location = location,
                                                deg_free = deg_free, deg_free2 = deg_free2)
  summary_repre <- map2(summary_repre,
                        summary_repre_value,
                        ~glue("{.x} {.y}")) 
  
  summary_repre <- map(summary_repre,
                       ~ withMathJax(
                         helpText(.x[1]),
                         br(),
                         helpText(.x[2]),
                         br(),
                         helpText(.x[3])
                       ))
  pluck(summary_repre, distribution_choice) %>% 
    return()
}
# summary_representation(distribution_choice = "gamma", 
#                        shape = 3,
#                        rate = 1)

#' @SUBFUNCTION #simplify the sd
specific_summary_repre <- function(x_mean = NULL, x_sd = NULL, rate = NULL,
                                   scale = NULL, shape = NULL, location = NULL,
                                   deg_free = NULL, deg_free2 = NULL){
  
  summary_functions <- list(
    
    MEAN = list((1/rate), 
                (shape/rate),
                (deg_free),
                (scale/(scale + shape)),
                (safely(weibullparinv)
                 (shape = shape, scale = scale, loc = 0)[["result"]]$mu),
                ifelse(deg_free > 1, 0, "Undefined"),
                (location),
                (x_mean),
                (exp(x_mean+((x_sd^2)/ 2))),
                "",
                ifelse(deg_free2 > 2, (deg_free2/(deg_free2-2)), "Undefined")
    ),
    SD = list((1/rate),
              (sqrt(shape/(rate^2))),
              (sqrt(2*deg_free)),
              (sqrt((scale*shape)/(((scale + shape)^2)*(scale+shape+1)))),
              (safely(weibullparinv)
               (shape = shape, scale = scale, loc = 0)[["result"]]$sigma),
              ifelse(deg_free > 2, sqrt(deg_free/(deg_free-2)), "Undefined"),
              (sqrt(((scale^2)*(pi^2))/ 3)),
              (x_sd),
              (sqrt((exp(x_sd^2)-1)*(exp((2*x_mean)+(x_sd^2))))),
              "",
              ifelse(deg_free2 > 4, (sqrt(((2*(deg_free2^2))*(deg_free+deg_free2-2))/
                                            (deg_free*((deg_free2-2)^2)*(deg_free2-4)))), 
                     "Undefined")
    ),
    VAR = list(((1/rate)^2),
               (shape/(rate^2)),
               (2*deg_free),
               ((scale * shape)/(((scale + shape)^2)*(scale + shape + 1))),
               (safely(weibullparinv)
                (shape = shape, scale = scale, loc = 0)[["result"]]$sigma^2),
               ifelse(deg_free > 2, deg_free/(deg_free-2), "Undefined"),
               (((scale^2)*(pi^2))/3),
               (x_sd^2),
               ((exp(x_sd^2)-1)*(exp((2*x_mean)+(x_sd^2)))),
               "",
               ifelse(deg_free2 > 4, (((2*(deg_free2^2))*(deg_free+deg_free2-2))/
                                        (deg_free*((deg_free2-2)^2)*(deg_free2-4))), 
                      "Undefined")
    )
  ) 
  
  summary_functions <- map(summary_functions, function(i){
    map(i, function(x){
      if(is.character(x) | is.null(x)) x
      else round(x, digits = 3)
    })
  })
  
  map(1:length(pluck(summary_functions,1)), function(i){ #same explanation of pluck because the length 
    map(summary_functions, i)
  }) %>% 
    set_names("exponential", "gamma", "chi_square","beta", "weibull", "student", 
              "logistic", "normal", "log_normal",  "cauchy","fisher") %>% 
    return()
}
# specific_summary_repre(shape = 3,
#                        rate = 1)





