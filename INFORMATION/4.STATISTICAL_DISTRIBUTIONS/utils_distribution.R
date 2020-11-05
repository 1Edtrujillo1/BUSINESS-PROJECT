# Arguments for the Inputs ------------------------------------------------
#' @description 
#' 0. if the choice is tail define its specific arguments for the radioButtons
#' 1. define the inputId and the label arguments of the numericInput
#' 2. add the parameters of the dictionary  to the list
#' @param id of the module
#' @param distribution_type type of distribution
#' @param choice type of the parameters to use
#' @return list of all the parameters to use in the future numericInput of a discrete or continous distribution
helpDist_library <- function(distribution_type = c("DISCRETE", "CONTINOUS"), 
                             choice = c("tail", "arguments", "x_value")){
  
  distribution_json <- jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  if(choice == "tail"){
    tails <- pluck(distribution_json, "TAIL") %>% pluck("GENERAL")
    
    specific_tails <-  c(
      glue("{map(tails[1:2],1)} x {map(tails[1:2],2)}"),
      glue("{map(tails[3],1)} a {map(tails[3],2)} b {map(tails[3],3)}")
    ) 
    names_tails <- str_replace_all(string = names(tails), 
                                   pattern = "_", 
                                   replacement = " ") %>% str_to_title()
    
    result <- c("lower_tail", "upper_tail", "interval") %>% 
      set_names(glue("{names_tails} : {specific_tails}"))
    
  }else{
    distribution_arguments <- map(c("DISCRETE", "CONTINOUS"),
                                  ~ map(pluck(distribution_json, .x), "ARGUMENTS")) %>% 
      set_names("DISCRETE", "CONTINOUS")
    
    inputId = list(
      arguments = map(c("DISCRETE", "CONTINOUS"),
                      ~map2(map(pluck(distribution_arguments, ..1), ~ names(.x)),
                            names(pluck(distribution_arguments, ..1)),
                            ~ glue("{.x}_{.y}")
                      ) %>% map(~str_replace_all(string = .x, 
                                                 pattern = "\\s", 
                                                 replacement = "_"))
      ) %>% set_names("DISCRETE", "CONTINOUS"),
      
      x_value = map(c("DISCRETE", "CONTINOUS"), 
                    ~ map(names(pluck(distribution_json, ..1)), 
                          function(i) map(c("x", "a", "b"), ~ glue("{.x}_{i}"))) %>% 
                      map(~as.character(.x)) 
      ) %>% set_names("DISCRETE", "CONTINOUS")
    )
    
    label = list(
      arguments = map(c("DISCRETE", "CONTINOUS"), 
                      ~ map2(map(pluck(distribution_arguments, ..1) , ~ names(.x)),
                             pluck(distribution_arguments, ..1),
                             ~ map2(..1, ..2, ~glue("{str_to_title(tolower(.x))} {.y} :")) 
                      )) %>% set_names("DISCRETE", "CONTINOUS"),
      x_value = map(map(dist_types, length), 
                    ~ list_of_lists(no_sublists = .x, 
                                    element_sublists = pluck(distribution_json, 
                                                             "VALUES"))
      ) %>% set_names("DISCRETE", "CONTINOUS")
    )
    
    result <- map(list(inputId, label), function(i){
      map(i, 
          ~map2(..1, dist_types, ~ .x %>% set_names(.y))
      )
    }) %>% set_names("inputId", "label") %>% 
      append(
        dictionary_distributions()
      ) %>%  map(~map(.x, distribution_type)) %>% map(~pluck(.x, choice)) %>% 
      compact()
  }
  result
}
# helpDist_library(choice = "tail")
# helpDist_library(distribution_type = "CONTINOUS", choice = "arguments")
# helpDist_library(distribution_type = "CONTINOUS", choice = "x_value")

# Define the Inputs --------------------------------------------------------
#' @description 
#' 1. use the function helpDist_library to bring the necessary inputs
#' 2. select the correct inputs to use in the dynamic numericInput
#' @param id of the module
#' @param distribution_type type of distribution
#' @param choice type of the parameters to use
#' @param distribution specific distribution to bring the input
#' @param tail input used
#' @return dinamic numericInput´s depend on the distribution
distribution_dinamicInput <- function(id, 
                                      distribution_type = c("DISCRETE", "CONTINOUS"),
                                      choice = c("arguments", "x_value"),
                                      distribution,
                                      tail = NULL){
  moduleServer(id, function(input, output, session){
    
    specific_distr <- helpDist_library(distribution_type = distribution_type, 
                                       choice = choice) %>% 
      map(distribution())
    
    if(choice == "arguments"){
      specific_distr
      
    }else if(choice == "x_value"){
      specific_distr <- specific_distr %>% map(~ .x %>% set_names("x", "a", "b"))
      
      if(tail() == "lower_tail" | tail() == "upper_tail"){ #%in% not work in reactive expressions
        specific_distr <- map(specific_distr, "x")
        
      }else if(tail() == "interval"){
        specific_distr <- map(c("a", "b"), ~ map(specific_distr, .x))
        
        names_specific_distr <- map(specific_distr, ~names(.x)) %>% unique() %>% 
          unlist()
        
        specific_distr <- map(names_specific_distr, ~ map(specific_distr, .x)) %>% 
          set_names(names_specific_distr)
      }
    }
    
    modify_each <- map2(c("inputId", "label"), 
                        list(~NS(id, .x), withMathJax),
                        ~map(pluck(specific_distr, ..1), ..2)
    ) %>% set_names("inputId", "label")
    
    specific_distr[c("inputId", "label")] <- modify_each
    
    map(1:length(pluck(specific_distr, 1)), function(i){ #is not necessary the conditionalPanel because we already filter the reactive expressions
      do.call(what = numericInput,
              args = map(specific_distr, i))
    }) %>% return()
  })
}

# Body Structure ----------------------------------------------------------
#' @description tabs and UI´s in the body of the app
#' @param id of the module
#' @param arguments to create the body
#' @return the body of the app
distribution_body <- function(id, distribution_type, tab_table, tab_plot,
                              id_reference, uId_distribution, uId_probability,
                              uId_summary){
  moduleServer(id, function(input, output, session){
    
    list(
      tabsetPanel(
        tabPanel(title = "Dataset",
                 DT::DTOutput(NS(id, tab_table))), 
        tabPanel(title = "Plot",
                 if(distribution_type == "DISCRETE"){
                   plotly::plotlyOutput(NS(id, glue("DISCRETE_{tab_plot}")))
                   
                 }else if(distribution_type == "CONTINOUS"){
                   plotOutput(NS(id, glue("CONTINOUS_{tab_plot}")))
                 })
      ),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          box(
            title = "Distribution Information",
            width = NULL,
            accordion(
              inputId = glue("{distribution_type}_{id_reference}"),  #important to work collapsed argument
              accordionItem(
                title = "Distribution Function",
                color = "danger",
                collapsed = TRUE,
                uiOutput(NS(id, uId_distribution))),
              accordionItem(
                title = "Probability Function",
                color = "warning",
                collapsed = TRUE,
                uiOutput(NS(id, uId_probability))),
              accordionItem(
                title = "Summary",
                color = "info",
                collapsed = TRUE,
                uiOutput(NS(id, uId_summary)))
            ))))
    ) %>% do.call(what = tagList, args = .)
  })
}

# Statistical Distribution Reactive ---------------------------------------
#' @description 
#' 1. assign the name parameters to the values from the values reactive expression
#' 2. eject the type of function from statistical_distribution.R depend on result_choice argument.
#' @param id of the module
#' @param distribution_type type of distribution
#' @param distribution specific distribution to bring the output
#' @param values values of the inputsIds
#' @param tail input used
#' @param result_choice type of output to bring
#' @return the output resulted from executing the created functions in statistical_distribution.R
distribution_reactive <- function(id, distribution_type = c("DISCRETE", "CONTINOUS"), 
                                  distribution, 
                                  values,
                                  tail = NULL,
                                  result_choice = c("summary",
                                                    "table_plot", 
                                                    "distribution"),
                                  return = c("table", "plot")){
  moduleServer(id, function(input, output, session){
    
    parameters_names <- list(
      distribution_args = list(
        DISCRETE = list(binomial = c("n_dist", "p_dist"), 
                        hypergeometric = c("M_dist", "N_dist", "n_dist"), 
                        poisson = "lambda_dist", 
                        geometric1 = "p_dist", 
                        geometric2 = "p_dist", 
                        negative1 = c("n_dist", "p_dist"), 
                        negative2 = c("n_dist", "p_dist")),
        CONTINOUS = list(exponential = "rate", 
                         gamma = c("shape", "rate"),
                         chi_square = "deg_free", 
                         beta = c("scale", "shape"),
                         weibull = c("shape", "scale"), 
                         student = "deg_free",
                         logistic = c("location", "scale"), 
                         normal = c("x_mean", "x_sd"),
                         log_normal = c("x_mean", "x_sd"), 
                         cauchy = c("location", "scale"),
                         fisher = c("deg_free", "deg_free2"))
      ),
      tails = list_of_lists(no_sublists = 2, 
                            element_sublists = c("x_dist", "a_dist", "b_dist")) %>% 
        set_names("DISCRETE", "CONTINOUS")
    ) %>% map(distribution_type)
    pluck(parameters_names, "distribution_args") <- #filter correct names
      pluck(pluck(parameters_names, "distribution_args"), distribution())
    
    values_inputIds <- map2(values, parameters_names, ~ .x %>% set_names(.y))
    
    if(result_choice == "summary"){
      result <- do.call(what = summary_representation,
                        args = list(distribution_choice = distribution()) %>% 
                          append(pluck(values_inputIds, "arguments")))
    }else{
      if(tail() == "lower_tail" | tail() == "upper_tail"){
        if(tail() == "lower_tail"){interval_distribution <- "under_x"}
        else if(tail() == "upper_tail"){interval_distribution <- "above_x"}
        
        pluck(values_inputIds, "x_value") <- 
          pluck(values_inputIds, "x_value")["x_dist"]
        
      }else if(tail() == "interval"){
        interval_distribution <- "interval"
        
        pluck(values_inputIds, "x_value") <- 
          pluck(values_inputIds, "x_value")[2:3]
      }
      values_inputIds <- values_inputIds %>% unname() %>% unlist(recursive = FALSE)
      
      values_inputIds <- 
        list_of_lists(no_sublists = 2, 
                      element_sublists = 
                        list(distribution_choice = distribution(),
                             interval_distribution = interval_distribution) %>% 
                        append(values_inputIds)
        )
      pluck(values_inputIds, 1) <- pluck(values_inputIds, 1) %>% 
        append(list(return = return))
      
      result <- map2(list(table_plot_distribution, distribution_representation),
                     values_inputIds,
                     ~ do.call(what = .x, args = .y)) %>% 
        set_names("table_plot", "distribution") %>% 
        pluck(result_choice)
    }
    result
  })
}



