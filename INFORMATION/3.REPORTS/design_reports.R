#df <- report_creator(df = df, year = 2020, semester = 1, summary = TRUE)

# Design Dataset ----------------------------------------------------------

#' @description we create two vectors first *percent_vars* containg the percentage
#' variables and those variables in the dataset we are going to apply the percent
#' desigm with the help of the walk function. And the *not_percent_vars* 
#' containing the not percentage variables (that are numeric or integer) and 
#' those variables in the dataset we are going to apply the accounting design 
#' with the help of the walk function.
#' Thanks to the previous steps our dataset is going to have a class of formattable
#' With the help of the function *formattable* we apply to our dataset *df_desing*
#' (with our previous designs) the design for the factors with the help of
#' the function *design_factor_vars* and the design for the numeric and integer
#' variables with the help of the function *design_numeric_vars*
#' @note  Because the design in the previous two functions are in list, we need
#' to use the function append to join the lists in one list.
#' @param df to apply a design
#' @return dataset with the design for each variable of a dataset.
#' @note rembember that the dataset need to have variables only with data type
#' factor, numeric and integer
design_dataset <- function(df){
  
  df_desing <- copy(df)
  
  # Format to number variables 
  percent_vars <- percentage_variables(df = df)
  not_percent_vars <- not_percentage_variables(df = df)
  
  walk(percent_vars,
       ~df_desing[,(.x):=percent(get(.x), format = "f")])
  walk(not_percent_vars, 
       ~df_desing[,(.x):=accounting(get(.x), format = "f")])
  
  # Style of the dataset
  formattable(df_desing, 
              design_factor_vars(df = df_desing) %>% 
                append(
                  design_numeric_vars(df = df_desing,
                                      varnumber_choice = "principal_not_percentage",
                                      not_percent_vars = not_percent_vars)) %>% 
                append(
                  design_numeric_vars(df = df_desing, 
                                      varnumber_choice = "principal_percentage",
                                      percent_vars = percent_vars)) %>% 
                append(
                  design_numeric_vars(df = df_desing,
                                      varnumber_choice = "result_not_percentage",
                                      not_percent_vars = not_percent_vars)) %>% 
                append(
                  design_numeric_vars(df = df_desing,
                                      varnumber_choice = "result_percentage",
                                      percent_vars = percent_vars)
                )) %>% 
    return()
}

# design_dataset(df = df)

#' @description define a vector *factor_variables* as the factor variables
#' of the dataset and after that each sublist need to be a design for each
#' factor variable that is why we replicate a list (which each sublist is the 
#' design)based on the number of factor variables we have
#' @param df dataset to get the factor variables
#' @return list which each sublist is a design for each factor variable
design_factor_vars <- function(df){
  
  factor_variables <- classes_vector(data_type = "factor", df = df)
  
  rep(list(
    formatter("span", style = ~style(color = "grey",
                                     font.weight = "bold"))), 
    length(factor_variables)) %>% 
    set_names(factor_variables) %>% 
    return() 
}
#design_factor_vars(df = df)

#' @description For each type of numeric or integer variable of the dataset we 
#' create a type of design creating a list called *design_numeric_vars* that each
#' sublist represent the design of a variable  that is why we replicate
#' a list based on the numeber of variables
#' we have.
#' @param df dataset to get the numeric and integer variables
#' @param varnumber_choice choice of the type of design
#' - If varnumber_choice %in% c("principal_not_percentage", "result_not_percentage")
#' @param not_percent_vars names of numeric or integer variables that are not
#' percentage 
#' - If varnumber_choice %in% c("principal_percentage", "result_percentage")
#' @param percent_vars names of numeric variables that are percentage
#' @return list which each sublist is a design fo each numeric or integer variable
design_numeric_vars <- function(df ,varnumber_choice = c("principal_not_percentage",
                                                         "principal_percentage",
                                                         "result_not_percentage",
                                                         "result_percentage"),
                                not_percent_vars = NULL, percent_vars = NULL){
  if(varnumber_choice == "principal_not_percentage"){
    principal_not_percent_vars <- str_subset(string = not_percent_vars,
                                             pattern = "^(?!\\^[.]).*$") #everything that not contain ^.
    design_numeric_vars <- rep(list(color_tile("lightpink", "lightblue")), 
                               length(principal_not_percent_vars)) %>% 
      set_names(principal_not_percent_vars)
    
  }else if(varnumber_choice == "principal_percentage"){
    principal_percent_vars <- str_subset(string = percent_vars,
                                         pattern = "^(?!\\^[.]).*$") 
    design_numeric_vars <- rep(list(color_tile("white", "orange")), 
                               length(principal_percent_vars)) %>% 
      set_names(principal_percent_vars)
    
  }else if(varnumber_choice == "result_not_percentage"){
    result_not_percent_vars <- str_subset(string = not_percent_vars,
                                          pattern = "\\^[.].*") #everything that contain ^. 
    
    unit.scale <- function(x) (x-min(x))/(max(x)-min(x))
    
    design_numeric_vars <- rep(list(color_bar(color = "lightblue",
                                              fun = unit.scale)), 
                               length(result_not_percent_vars)) %>% 
      set_names(result_not_percent_vars)
    
  }else if(varnumber_choice == "result_percentage"){
    result_percent_vars <- str_subset(string = percent_vars,
                                      pattern = "\\^[.].*") 
    design_numeric_vars <- rep(list(
      formatter("span", 
                style = x ~ style(font.weight = "bold",
                                  color = ifelse(x > 0 , 
                                                 "#71CA97", 
                                                 ifelse(x < 0, 
                                                        "#ff7f7f", "black"))),
                x ~ icontext(ifelse(x > 0, "arrow-up", "arrow-down"), x))),
      length(result_percent_vars)) %>% 
      set_names(result_percent_vars)
  }
  design_numeric_vars 
}
# percent_vars <- percentage_variables(df = df)
# design_numeric_vars(df = df, varnumber_choice = "principal_percentage",
#                     percent_vars = percent_vars)
# not_percent_vars <- not_percentage_variables(df = df)
# design_numeric_vars(df = df, varnumber_choice = "result_not_percentage",
#                     not_percent_vars = not_percent_vars)

# Identify Variables ------------------------------------------------------

#' @description this function iterate over all the variables of a dataset and
#' check if a variable is percentage if it is bring the name of that variable
#' in other case do not bring anything
#' @param df
#' @return vector of strings of variables that are percentage (0<=X<=1)
percentage_variables <- function(df){
  
  numerc_variable <- classes_vector(data_type = "numeric", df = df)
  
  map(numerc_variable, safely(function(variable){
    
    if(length(keep(df[,get(variable)], ~-1<=.x & .x<=1)) == df[,length(get(variable))])
      variable
    else NULL
  })) %>% map(~.x[["result"]]) %>% compact() %>% unlist() %>% 
    return()
}
#percentage_variables(df = df)

#' @description we define two vectors, the first with all the numeric and 
#' integer variables and the other with the percentage variables.
#' - If the vector *percentage vars* got content then we subset from 
#' *numerc_int_variable*, the elements that not are in *percentage vars*
#' - If the vector *percentage vars* do not got any content then we bring
#' all the *numerc_int_variable*
#' @param df
#' @return vector of strings of variables that are not percentage from all the
#' integer and numeric variables
not_percentage_variables <- function(df){
  
  numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"), 
                                        df = df)
  percent_vars <- percentage_variables(df = df)
  
  if(!is.null(percent_vars)){
    
    percent_vars[1] <- str_c("\\", percent_vars[1])
    percent_vars <- str_c(percent_vars, collapse = "|\\")
    
    not_percent_vars <- str_subset(string = numerc_int_variable, 
                                     pattern = glue("^(?!(?:{percent_vars})).*$")) #everything except percent_vars variables
  }else not_percent_vars <- numerc_int_variable
  not_percent_vars
}
#not_percentage_variables(df = df)





















