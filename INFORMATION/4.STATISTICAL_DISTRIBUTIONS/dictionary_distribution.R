#' @description dictionary of the values of the inputs
#' @param .
#' @return a list of the values of the parameters in the inputs of the app
dictionary_distributions <- function(){
  
  list(
    value = list(
      arguments = list(
        DISCRETE = list(c(20, 0.5), c(100, 500, 50), 4) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = 0.5)) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = c(5, 0.5))),
        CONTINOUS = list(
          exponential = 1,
          gamma = c(3, 2),
          chi_square = c(6),
          beta = c(1, 3),
          weibull = c(5, 1),
          student = c(10),
          logistic = c(0, 1),
          normal = c(0, 1),
          log_normal = c(0, 1),
          cauchy = c(0, 1),
          fisher = c(10, 5)
        )
      ),
      x_value = list(
        DISCRETE = list(
          binomial = c(rep(8, 2) , 9), #x,a,b
          hypergeometric = c(rep(8, 2) ,9),
          poisson = c(rep(6, 2), 7),
          geometric1 = c(rep(1, 2), 2),
          geometric2 = c(rep(2, 2), 3),
          negative1 = c(rep(2, 2), 3),
          negative2 = c(rep(6, 2), 7)
        ),
        CONTINOUS = list(
          exponential = c(rep(2.24, 2), 3.36),
          gamma = c(rep(0.8, 2), 2.4),
          chi_square = c(rep(9.6, 2), 14.4),
          beta = c(rep(0.25, 2), 0.45),
          weibull = c(rep(0.8, 2), 1.2),
          student = c(rep(-1, 2), 1),
          logistic = c(rep(-1.2, 2), 1.2),
          normal = c(rep(-1, 2), 1),
          log_normal = c(rep(1, 2), 2),
          cauchy = c(rep(-1.2, 2), 1.2),
          fisher = c(rep(2.76, 2), 4.14)
        )
      )
    ),
    
    min = list(
      arguments = list(
        DISCRETE = list(rep(0, 2), rep(0, 3), 1) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = 0)) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = c(1, 0))),
        CONTINOUS = list(
          exponential = 0,
          gamma = c(0, 0),
          chi_square = 1,
          beta = c(0, 0),
          weibull = c(0, 0),
          student = 1,
          logistic = c(NA, 0),
          normal = c(NA, 0),
          log_normal = c(NA, 0),
          cauchy = c(NA, 0),
          fisher = c(1, 1)
        )
      ),
      x_value = list(
        DISCRETE = list_of_lists(no_sublists = 4, element_sublists = rep(0, 3)) %>% 
          append(list(rep(1, 3), rep(0, 3), rep(1, 3))),
        CONTINOUS = list_of_lists(no_sublists = 5, element_sublists = rep(0, 3)) %>% 
          append(list_of_lists(no_sublists = 3, element_sublists = rep(NA, 3))) %>% 
          append(list(rep(0, 3))) %>% 
          append(list(rep(NA, 3))) %>% 
          append(list(rep(0, 3)))
      )
    ),
    
    max = list(
      arguments = list(
        DISCRETE = list(c(NA, 1), rep(NA, 3), NA) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = 1)) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = c(NA, 1))),
        CONTINOUS = NULL
      ),
      x_value = list(
        DISCRETE = NULL,
        CONTINOUS = list_of_lists(no_sublists = 3, element_sublists = rep(NA, 3)) %>% 
          append(list(rep(1, 3))) %>% 
          append(list_of_lists(no_sublists = 7, element_sublists = rep(NA, 3)))
      )
    ),
    
    step = list(
      arguments = list(
        DISCRETE = list(c(1, 0.01), rep(1, 3), 1) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = 0.01)) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = c(1, 0.01))),
        CONTINOUS = list(0.5) %>% 
          append(list(rep(1, 2))) %>%   
          append(list(c(1))) %>% 
          append(list_of_lists(no_sublists = 2, element_sublists = rep(1,2))) %>% 
          append(list(c(1))) %>% 
          append(list_of_lists(no_sublists = 5, element_sublists = rep(1,2)))
      ),
      x_value = list(
        DISCRETE = list_of_lists(no_sublists = 7, element_sublists = rep(1, 3)),
        CONTINOUS = list_of_lists(no_sublists = 3, element_sublists = rep(1, 3)) %>% 
          append(list(rep(0.01, 3))) %>% 
          append(list_of_lists(no_sublists = 7, element_sublists = rep(1, 3)))
      )
    )
  ) %>% map(function(i){
    map(i,
        ~ map2(..1,
               dist_types, safely(~ .x %>% set_names(.y))) %>% map("result")
    )
  }) %>% return()
}