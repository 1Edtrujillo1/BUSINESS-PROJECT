###############3  
# df <- read_data("INFORMATION/FILES/saved.rds")
# factor <- c("PCLASS", "SEX")
# date_variable <- classes_vector(data_type = "Date", df = df)
# numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"),
#                                       df = df)
# df <- df[,c(factor, date_variable, numerc_int_variable), with = FALSE]
############### 

#' @description with the help of the function *levels_datasets* we get a list 
#' which each sublist is for each factor variable of the dataset, then we 
#' iterate over each element of this list with the help of the function 
#' *df_dates_info* 
#' @param df
#' @param period_choice type of period we want
#' @return for a specific period_choice we get all the possible combinations 
#' possible dates that are available for each leavel for each factor variable
recommended_periods <- function(df, period_choice = c("year", "month", 
                                                      "semester", "quarter")){
  
  dfs_list <- levels_datasets(df = df)
  
  map(dfs_list, function(dfs){
    
    map(dfs, function(df){
      
      map(df, ~df_dates_info(df = .x, period_choice = period_choice))
      
    })
  }) %>% return()
}
#all_possible_dfs <- recommended_periods(df = df, period_choice = "semester")
#map(all_possible_dfs$PCLASS, ~names(.x)) 
#all_possible_dfs$PCLASS[[1]]$`2`

#' @SUBFUNCTION 
#' @description we define dates_df as the possible allow dates in the df, with
#' the help of the function *year_month* and *semester_quarter*
#' @param df dataset from the list from the *levels_datasets* function
#' @param period_choice type of period we want
#' @return list of vector of all the possible dates in a specific period_choice
#' that are allowed in the dataset df
df_dates_info <- function(df, period_choice = c("year", "month", "semester", "quarter")){
  
  date_variable <- classes_vector(data_type = "Date", df = df)
  
  years_date <- year(df[,get(date_variable)]) %>% unique()
  
  if(period_choice == "year"){
    dates_df <- map(years_date, ~year_month(df = df, year = .x, previous = FALSE)) %>% 
      set_names(years_date)
  }
  
  else if(period_choice == "month"){
    dates_df <- map(years_date, function(year){
      months <- map(specific_month, 
                    ~year_month(df = df, year = year, month = .x, previous = FALSE)) %>% 
        set_names(specific_month)
      months <- months[map(months,~is.Date(.x)) == TRUE]
      months
    }) %>% set_names(years_date)
  }
  
  else if(period_choice == "semester"){
    dates_df <- map(years_date, function(year){
      semesters <- map(1:2, 
                       ~semester_quarter(df = df, year = year, 
                                         semester = .x, past = FALSE)) %>% 
        set_names(1:2)
      semesters <- semesters[map(semesters,~is.Date(.x)) == TRUE]
      semesters
    }) %>% set_names(years_date)
  }
  
  else if(period_choice == "quarter"){
    dates_df <- map(years_date, function(year){
      trimestres <- map(1:4, 
                        ~semester_quarter(df = df, year = year,
                                          quarter = .x, past = FALSE)) %>% 
        set_names(1:4)
      trimestres <- trimestres[map(trimestres,~is.Date(.x)) == TRUE]
    }) %>% set_names(years_date)
  }
  
  dates_df
}
#df_dates_info(df = dfs$SEX[[1]]$FEMALE, period_choice = "semester")

#' @SUBFUNCTION 
#' @description 
#' 1. define vectors of the factor(s) and date variable respectively
#' 2. If we do not have a date variable send a string message
#' 3. If we have a date variable:
#' 3.1 If we do not have factor variables send string message
#' 3.2 If we have factor variables:
#' 3.2.1 We create a list *dfs_by_factor* which each sublist is a dataset 
#' grouped by a respective factor of all the factor variables
#' 3.2.2 We create a list *possible_levels_each_factor* which we iterate over
#' all the factor variables of the dataset, which each sublist is a list of
#' the possible combinations without order from the levels of each factor variable
#' with the help of the function *levels_comb_NOreplace*
#' 3.2.3 With the help of the function *filter_levels* and each dataset in 
#' *dfs_by_factor* and the list *possible_levels_each_factor* we create a list
#' which each sublist is a list of dataset where each factor variable is filtered 
#' with the possible levels in each sublist of *possible_levels_each_factor*
#' 3.2.3 We change the name of each sublist with the help of the funcion 
#' *sublist_names*
#' @param df dataset to get the list of filter dataset for each possible level
#' @return list which each sublist is for each factor variable of the dataset
#' and each sublist are the filter dataset for each possible combinations without
#' replacement of the levels(lists inside the sublist)
levels_datasets <- function(df){
  
  factor_vars <- classes_vector(data_type = "factor", df = df)
  date_variable <- classes_vector(data_type = "Date", df = df)
  
  if(length(date_variable) == 0) "You need a Date variable"
  
  else{
    
    if(length(factor_vars) == 0) "You need at least one factor variable"
    
    else{
      
      dfs_by_factor <- map(factor_vars, 
                           ~df[,c(.x, date_variable), with = FALSE] %>% 
                             .[,.SD, by = .x]) %>% 
        set_names(factor_vars)
      dfs_by_factor <- dfs_by_factor[factor_vars] #confirm the order of the sublists
      
      possible_levels_each_factor <- map(factor_vars, 
                                         ~levels_comb_NOreplace(
                                           factor = df[,get(.x)])) %>% 
        set_names(factor_vars)
      possible_levels_each_factor <- possible_levels_each_factor[factor_vars] 
      
      filter_datasets <- pmap(
        list(dfs_by_factor, factor_vars, possible_levels_each_factor), 
        filter_levels)
      filter_datasets <- filter_datasets[factor_vars]
      
      filter_datasets <- map2(possible_levels_each_factor, filter_datasets, 
                              sublist_names)
      
      filter_datasets
    }
  }
}
# dfs <- levels_datasets(df = df)
# map(dfs$PCLASS, ~names(.x)) #levels names

#' @SUBFUNCTION
#' @description We iterate over all the levels of a factor variable using the 
#' combinations function to get the possible combinations without order of 
#' that levels as a dataset, and use split function to divide each row as a 
#' sublist
#' the possible 
#' @param factor 
#' @return List of the possible combination without order from the levels of ONE
#' factor variable
#' For example:
#' 1:3
#' 1
#' 2
#' 3
#' 1,2
#' 1,3
#' 2,3
#' 1,2,3
levels_comb_NOreplace <- function(factor){
  
  all_levels <- levels(factor)
  
  comb_list <- pmap(list(i = 1: length(all_levels)), function(i){
    
    combinations(x = all_levels, k = i, replace = FALSE) %>% as.data.table()
  })
  
  comb_list <- map(comb_list, ~split(x = .x, f = seq(.x[,.N])))
}
#levels_comb_NOreplace(factor = df$PCLASS) %>% return()

#' @SUBFUNCTION 
#' @description
#' From the *levels_datasets* function we defined the lists:
#' *dfs_by_factor*: if we extract one sublist which is a dataset grouped
#' by a factor variable (for example PCLASS)
#' *possible_levels_each_factor*: if we extract one sublist which is a list of 
#' the possible combinations without order from the levels of a factor variable
#' (for example PCLASS)
#' Then we filter the factor variable of the dataset from *dfs_by_factor* to 
#' each sublist from the list *possible_levels_each_factor*
#' @param df
#' @param factor_var
#' @param possible_levels
#' @return List of datasets which a factor variable factor_var of a dataset df 
#' is filtered in each sublist (possible combinations without replacement of the 
#' levels) of a list possible_levels  
filter_levels <- function(df, factor_var, possible_levels){
  
  map(possible_levels, function(levels){
    
    pmap(list(i = 1:length(levels)), function(i){
      
      df[get(factor_var) %in% levels[[i]]]
      
    })
  }) %>% return()
}
# filter_levels(df = dfs_by_factor$PCLASS, factor_var = "PCLASS", 
#               possible_levels = possible_levels_each_factor$PCLASS)

#' @SUBFUNCTION 
#' @description 
#' 1.we create a list of names called *names_as_levels* where each
#' sublist contains the possible unique levels of each dataset in each sublist
#' df
#' 2. We replace the name of each sublist
#' @param possible_levels
#' @param df
#' @return change the name of each sublist as the levels of the factor variable
#' of each dataset in each sublist
sublist_names <- function(possible_levels, df){
  
  names_as_levels <- map(possible_levels, function(lev){
    
    pmap(list(i = 1:length(lev)), function(i){
      
      changing_name <- lev[[i]]
      
      if(length(changing_name) > 1)
        changing_name[,.(str_c(.SD, collapse = ","))]
      else changing_name  
    })
  })
  
  for (j in 1:length(df)){
    names(df[[j]]) <- unlist(names_as_levels[[j]]) %>% unname()} #allow to rename each sublist
  
  df
}
# sublist_names(possible_levels = possible_levels_each_factor$SEX, 
#               df = filter_datasets$SEX)


# Additional --------------------------------------------------------------

#' @description 
#' 1. Define a vector of the levels of the factor variable
#' 2. List which each sublist is each level
#' 3. Create a null sublist called *e.g.list*
#' 4. In each null sublist we add *e.g.list* times the number represent the 
#' level 
#' 5. With the help of the function expand.grid for each sublist we create 
#' the dataset of combinations for each sublist
#' @param factor factor variable of a dataset
#' @return list of dataset of all the combinations in the levels of a factor
#' variable
levels_combinations <- function(factor){
  
  no_levels <- levels(factor) %>% length()

  no_sublists <- map(1:no_levels, ~rep(1, .x)) %>% 
    map(~length(.x))
  
  e.g.list <- pmap(list(i = 1:length(no_sublists)),function(i){
    rep(c(), i)})#create null sublists
  
  e.g.element <- 1:no_levels
  e.g.list <- pmap(list(i = 1:length(e.g.list)), 
                   function(i){
                     pmap(list(j = 1: i),
                          function(j){
                            e.g.list[[i]][[j]] <- e.g.element #define each element of each sublist
                          })
                   })
  
  e.g.list <- pmap(list(i = 1:length(e.g.list)), function(i){
    
    args.grid <- pluck(e.g.list, i) 
    
    do.call(what = expand.grid, args = args.grid)
  })
}
#levels_combinations(factor = df$PCLASS)




