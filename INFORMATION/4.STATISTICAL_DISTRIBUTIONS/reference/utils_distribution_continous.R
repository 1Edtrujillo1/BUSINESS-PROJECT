helpCont_library <- function(choice = c("tail", "arguments", "x_value")){
  
  distribution_json <- jsonlite::fromJSON("INFORMATION/4.STATISTICAL_DISTRIBUTIONS/distribution_info.json")
  
  cont_distributions <- c("exponential", "gamma",
                          "chi_square", "beta",
                          "weibull", "student",
                          "logistic", "normal",
                          "log_normal", "cauchy",
                          "fisher")
  
  if(choice == "tail"){
    tails <- pluck(distribution_json, "TAIL") %>% pluck("GENERAL")
    
    specific_tails <- 
      c(glue("{map(tails[1:2],1)} x {map(tails[1:2],2)}"),
        glue("{map(tails[3],1)} a {map(tails[3],2)} b {map(tails[3],3)}")
      ) 
    
    names_tails <- str_replace_all(string = names(tails), 
                                   pattern = "_", 
                                   replacement = " ") %>% 
      str_to_title()
    
    result <- c("lower_tail", "upper_tail", "interval") %>% 
      set_names(glue("{names_tails} : {specific_tails}"))
    
  }else{
    arguments_cont <- map(distribution_json[["CONTINOUS"]], "ARGUMENTS")
    
    inputId = list(
      arguments = map2(names(arguments_cont),
                       map(arguments_cont, ~ names(.x)),
                       ~glue("{.y}_{.x}")) %>%
        map(~str_replace_all(string = .x, pattern = "\\s", replacement = "_")),
      
      x_value = map(names(distribution_json[["CONTINOUS"]]), function(i){
        map(c("x", "a", "b"), ~glue("{.x}_{i}"))
      }) %>% map(~as.character(.x))
    )
    
    label = list(
      arguments = map2(
        map(arguments_cont, ~ names(.x)),
        map(arguments_cont, ~ .x),
        ~map2(.x , .y, ~glue("{..1} {..2}:"))
      ),
      x_value = list_of_lists(no_sublists = 11, 
                              element_sublists = pluck(distribution_json, "VALUES"))
    )
    
    value = list(
      arguments = list(
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
      ),
      x_value = list(
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
    
    min = list(
      arguments = list(
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
      ),
      x_value = list_of_lists(no_sublists = 5, element_sublists = rep(0, 3)) %>% 
        append(list_of_lists(no_sublists = 3, element_sublists = rep(NA, 3))) %>% 
        append(list(rep(0, 3))) %>% 
        append(list(rep(NA, 3))) %>% 
        append(list(rep(0, 3)))
    )
    
    max = list(
      arguments = NULL,
      x_value = list_of_lists(no_sublists = 3, element_sublists = rep(NA, 3)) %>% 
        append(list(rep(1, 3))) %>% 
        append(list_of_lists(no_sublists = 7, element_sublists = rep(NA, 3)))
    )
    
    step = list(
      arguments = list(0.5) %>% 
        append(list(rep(1, 2))) %>%   
        append(list(c(1))) %>% 
        append(list_of_lists(no_sublists = 2, element_sublists = rep(1,2))) %>% 
        append(list(c(1))) %>% 
        append(list_of_lists(no_sublists = 5, element_sublists = rep(1,2))),
      
      x_value = list_of_lists(no_sublists = 3, element_sublists = rep(1, 3)) %>% 
        append(list(rep(0.01, 3))) %>% 
        append(list_of_lists(no_sublists = 7, element_sublists = rep(1, 3)))
    )
    
    result <- map(list(inputId, label, value, min, max, step), 
                  ~pluck(.x, choice)) %>% 
      map(safely(~.x %>% set_names(cont_distributions))) %>% map("result") %>% 
      set_names("inputId", "label", "value", "min", "max", "step") %>% 
      compact()
  } 
  result
}
#helpCont_library(choice = "x_value")
#helpCont_library(choice = "tail")

inumericInput_reactive <- function(id, distribution, tail = NULL,
                                   choice = c("arguments", "x_value")){
  moduleServer(id, function(input, output, session){
    
    values <- c("value", "min", "max", "step")
    
    specific_distr <- map(helpCont_library(choice = choice),
                          distribution())
    if(choice == "arguments"){
      specific_distr
      
    }else if(choice == "x_value"){
      specific_distr <- specific_distr %>% map(~.x %>% set_names("x", "a", "b"))
      
      if(tail() == "lower_tail" | tail() == "upper_tail"){ #%in% not work in reactive expressions
        specific_distr <- map(specific_distr, "x")
        
      }else if(tail() == "interval"){
        specific_distr <- map(c("a", "b"), ~map(specific_distr, .x))
        
        names_specific_distr <- map(specific_distr, ~names(.x)) %>% unique() %>%
          unlist()
        
        specific_distr <- map(names_specific_distr, ~map(specific_distr, .x)) %>%
          set_names(names_specific_distr)
      }
    }
    for(var in c("inputId", "label")){
      modify_each <- map2(c("inputId", "label"), list(~ NS(id, .x), withMathJax),
                          function(i, func){
                            map(pluck(specific_distr, i),  func)
                          }) %>%
        set_names("inputId", "label")
      pluck(specific_distr, var) <- pluck(modify_each, var)
    }
    map(1:length(pluck(specific_distr, 1)), function(i){ #is not necessary the conditionalPanel because we already filter the reactive expressions
      do.call(what = numericInput,
              args = map(specific_distr, i))
    }) %>% return() 
  })
}

###################################################################
distribution_body <- function(id, tab_table, tab_plot, 
                              id_reference, uId_distribution,
                              uId_probability, uId_summary){
  moduleServer(id, function(input, output, session){
    
    list(
      tabsetPanel(
        tabPanel(title = "Dataset",
                 DT::DTOutput(NS(id, tab_table))),
        tabPanel(title = "Plot",
                 plotOutput(NS(id, tab_plot)))
      ),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          box(
            title = "Distribution Information",
            width = NULL,
            accordion(
              inputId = id_reference, #important to work collapsed argument
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
    ) %>% do.call(what = tagList, .) %>% return()
  })
}

###################################################################
continous_distribution_reactive <- function(id, values, distribution, tail = NULL,
                                            result_choice = c("summary",
                                                              "table_plot", 
                                                              "distribution"),
                                            return = c("table", "plot")){
  moduleServer(id, function(input, output, session){
    
    parameters <- list(distribution_args = list(exponential = "rate", 
                                                gamma = c("shape", "rate"),
                                                chi_square = "deg_free", 
                                                beta = c("scale", "shape"),
                                                weibull = c("shape", "scale"), 
                                                student = "deg_free",
                                                logistic = c("location", "scale"), 
                                                normal = c("x_mean", "x_sd"),
                                                log_normal = c("x_mean", "x_sd"), 
                                                cauchy = c("location", "scale"),
                                                fisher = c("deg_free", "deg_free2")),
                       tails = c("x_dist", "a_dist", "b_dist"))
    
    pluck(parameters, "distribution_args") <- #filter correct names
      pluck(pluck(parameters, "distribution_args"), distribution())
    
    values_inputIds <- map2(parameters, values, ~ .y %>% set_names(.x))
    
    if(result_choice == "summary"){
      result <- do.call(what = summary_representation,
                        args = list(distribution_choice = distribution()) %>% 
                          append(pluck(values_inputIds, "distribution_args")))
    }else{
      if(tail() == "lower_tail" | tail() == "upper_tail"){
        if(tail() == "lower_tail"){interval_distribution <- "under_x"}
        else if(tail() == "upper_tail"){interval_distribution <- "above_x"}
        
        pluck(values_inputIds, "tails") <- pluck(values_inputIds, "tails")["x_dist"]
        
      }else if(tail() == "interval"){
        interval_distribution <- "interval"
        
        pluck(values_inputIds, "tails") <- pluck(values_inputIds, "tails")[2:3]
      }
      values_inputIds <- values_inputIds %>% unlist(recursive = FALSE)
      values_inputIds <- values_inputIds %>% set_names(
        str_remove_all(
          string = str_remove_all(string = names(values_inputIds),
                                  pattern = ".*(?=[.])"),
          pattern = "[.]")
      )
      values_inputIds <- 
        list_of_lists(no_sublists = 2, 
                      element_sublists = 
                        list(distribution_choice = distribution(),
                             interval_distribution = interval_distribution) %>% 
                        append(values_inputIds))
      pluck(values_inputIds, 1) <- pluck(values_inputIds, 1) %>% 
        append(list(return = return))
      
      result <- map2(list(continous_distribution, distribution_representation),
                     values_inputIds, 
                     ~ do.call(what = .x, args = .y)) %>% 
        set_names("table_plot", "distribution")
      
      result <- pluck(result, result_choice)
    }
    result
  })
}

# help --------------------------------------------------------------------
# k <- function(id, distribution, tale){
#   
#   moduleServer(id, function(input, output, session){
#     
#     conditionalPanel(
#       ns = NS(id),
#       condition = glue("input.cont_args == {distribution()}"),
#       
#       conditionalPanel(
#         ns = NS(id),
#         condition = glue("input.cont_tail == {tale()}"),
#         
#         distribution(),
#         tale()
#       )
#     )
#     
#   })
# }

# inumericInput_reactive <- function(id){
#   
#   moduleServer(id, function(input, output, session){
#     
#     conditionalPanel(
#       ns = NS(id),
#       condition = "input.continous == 'exponential'", #| input.continous == 'chi_square' | input.continous == 'student'",
#       
#       div("\\(n\\)", style = "font-size:1%", color = "transparent"), #color to hide because is not a parameter (trick) 
#       
#       numericInput(inputId = NS(id, "rate_exponential"),
#                    label = withMathJax("Rate \\(\\lambda\\):"),
#                    value = 1,
#                    min = 0,
#                    step = 0.5)
#     )
#     
#   })
# }


