###############3  
# df <- read_data("INFORMATION/FILES/saved.rds")
# factor <- c("PCLASS", "SEX")
# date_variable <- classes_vector(data_type = "Date", df = df)
# numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"),
#                                       df = df)
# df <- df[,c(factor, date_variable, numerc_int_variable), with = FALSE]
# 
# df1 <- report_creator(df = df, year = 2020, semester = 1, summary = TRUE)
# df2 <- report_creator(df = df, year = 2020, semester = 1, summary = FALSE) #we use this to get the data distribution of the numeric and integer variables
############### 

#' @description 
#' 1. With our function created report_creator using the same information in 
#' the dates variable we iterate over the summary argument to get the summary
#' report saved in the Summary sublist and to get the grouped data saved in the
#' Not_Summary of the final_reports list
#' 2. In the defined summary report we change all the sufixes in its names
#' 3. In the defined Not_Summary grouped dataset we apply the function 
#' *sparklines_all_NumIntVar* to creates a dataset with the sparklines of all
#' the numeric and integer variables in that dataset for each possible levels 
#' 4. Finally we merge both reports based on the factor variables with the 
#' intention that the summary report cointain the sparklines for each numeric
#' and integer variable.
#' 5.To this final dataset we apply our function *design_dataset* to apply 
#' the design to all the variables of the final dataset.
#' 6. The next part of the code is to allow to watch the sparkline 
#' @param df
#' @return The final dataset with all the correct design 
final_design_df <- function(df,
                            year = NULL, month = NULL,
                            semester = NULL, quarter = NULL){
  
  df <- copy(df)
  
  final_reports <- map(c(TRUE,FALSE), 
                       ~report_creator(df = df, year = year, 
                                       month = month, semester = semester,
                                       quarter = quarter, summary = .x)) %>% 
    set_names(c("Summary", "Not_Summary"))
  
  summary_report <- final_reports[["Summary"]] 
  
  summary_report %>% setnames(
    old = str_subset(
      string = names(summary_report),
      pattern = classes_vector(data_type = "factor", df = summary_report)),
    new = str_remove_all(
      string = classes_vector(data_type = "factor", df = summary_report),
      pattern = "(?:[.]present$|[.]past$)"))
  
  sparkline_report <- sparklines_all_NumIntVar(df = final_reports[["Not_Summary"]])
  
  factor_variables <- classes_vector(data_type = "factor", df = df)
  
  final_report <- merge(summary_report, sparkline_report, by = factor_variables, 
                        all.x = TRUE)
  
  final_report <- design_dataset(df = copy(final_report))
  final_report <- as.htmlwidget(final_report)
  final_report$dependencies = c(final_report$dependencies, 
                                htmlwidgets:::widget_dependencies("sparkline", 
                                                                  "sparkline"))
  final_report
}
#final_design_df(df = df, year = 2020, semester = 1)

#' @description 
#' 1. For each name variable in the dataset we delete the suffixes .present or
#' .past
#' 2. We define two vectors containg all the factor variables and the (numeric
#' and integer) variables respectively.
#' 3. With the help of the function *sparklines_unique_NumIntVar* we get a list
#' iterating over all the numeric and integer variables. Where each sublist
#' contain a dataset where for each level of the factor(s) variable(s) we get 
#' the sparkline of the distribution of that numeric or integer variable in each
#' level.
#' 4. Thanks to each dataset of the list *each_sparkline* is based on the same
#' factor(s) variable(s) we merge with the help of the function *iterative_merge*,
#' by the factor variables
#' @param df dataset to get the sparkline of each numeric or integer variable
#' @return dataset where for each level of the factor(s) variable(s) we get the
#' sparkline of the distribution of ALL the numeric and integer variables in each
#' level in the dataset
#' @note This function is applied to ALL numeric and integer variable
sparklines_all_NumIntVar <- function(df){
  
  # Define the Dataset 
  df <- copy(df)
  df %>% setnames(old = names(df),
                  new = str_remove_all(string = names(df),
                                       pattern = "(?:[.]present$|[.]past$)"))
  
  factor_variables <- classes_vector(data_type = "factor", df = df)
  
  numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"), 
                                        df = df)
  each_sparkline <- map(numerc_int_variable, 
                        ~sparklines_unique_NumIntVar(df = df, num_int_var = .x,
                                                     factor_variables = factor_variables)) %>% 
    set_names(numerc_int_variable)
  df_sparklines <- iterative_merge(dfs_list = each_sparkline, 
                                   key = factor_variables)
  df_sparklines
}
#sparklines_all_NumIntVar(df = df2) 

#' @description 
#' 1. We apply the *marginal_distribution* function to the grouped dataset 
#' to get the list of the distribution of a numeric or integer variable in each 
#' possible levels.
#' 2. We get a list iterating over each sublist to get the sparkline of the 
#' distribution of that numeric or integer variable in each sublist (each levels)
#' 3. We use the function rbindlist() to get a dataset where each rows are the 
#' sublists. This represent a dataset with the possible leveles and the sparkline
#' in each levels.
#' @param df grouped dataset 
#' @param num_int_var numeric or integer variable that we want to get the 
#' sparklines of its distribution
#' @param factor_variables factor(s) variable(s) of the grouped dataset 
#' @return a dataset where for each level of the factor(s) variable(s) we get 
#' the sparkline of the distribution of the numeric or integer variable in each
#' level.
#' @note This function is applied to ONE numeric or integer variable
sparklines_unique_NumIntVar <- function(df, num_int_var, factor_variables){
  
  df <- copy(df)
  
  distributions <- marginal_distribution(df = df, num_int_var = num_int_var,
                                         factor_variables = factor_variables)
  
  map(distributions, 
      ~.x[,factor_variables, with = FALSE] %>% unique() %>% 
        .[,str_c(num_int_var, "spark", sep = "."):=
            as.character(htmltools::as.tags(
              sparkline(.x[,get(num_int_var)], type = "line")))
        ]) %>% 
    rbindlist() %>% return()
}
# sparklines_unique_NumIntVar(df = df2, num_int_var = "PASSENGERID.present",
#                             factor_variables = factor_variables)

#' @description 
#' 1. from the grouped dataset we only bring the factor(s) and the numeric or 
#' integer variable.
#' 2.A grouped dataset contain possible levels (We want the uniquecombinations)
#' That is why we create a list *possibilities* where we get the first by the
#' factor(s) variable(s) to get the possible levels and split to get a list 
#' where each sublist are the possible levels.
#' 3. We iterate over each sublist of *possibilities* (each possible levels)
#' with the map function to filter from the grouped dataset df each possible 
#' levels in each sublist with the merge function.
#' 4. We change the names of the sublists as a vector of the possible levels
#' in each sublist.
#' @param df grouped dataset to get the distribution of a numeric or integer 
#' variable
#' @param num_int_var numeric or integer variable that we want to get it 
#' distribution
#' @param factor_variables factor(s) variable(s) of the grouped dataset 
#' @return a list where each sublist is for the possible levels of the factor(s)
#' variable(s) in the grouped dataset and each sublist contain a dataset of the 
#' observations of the factor(s) and the numeric or integer variable filtering
#' the factor(s) variable(s) in the respective levels 
#' @note This function is applied to ONE numeric or integer variable
marginal_distribution <- function(df, num_int_var, factor_variables){
  
  df <- copy(df)[,c(factor_variables, num_int_var), with = FALSE]
  
  # list of filter dataset of each level 
  possibilities <- df[,.SD[1], by = factor_variables] %>% 
    .[,factor_variables, with = FALSE] %>% 
    split(x = ., f = seq(.[,.N]))
  
  filter_dfs <- map(possibilities, ~merge(df,.x)) #filter more than two not standard variables 
  
  # Name of each sublist 
  names_filter_dfs <- map(filter_dfs, 
                          ~str_c(.x[,factor_variables, with = FALSE] %>% 
                                   unique() %>% unlist(),
                                 collapse = ","))
  
  filter_dfs <- filter_dfs %>% set_names(names_filter_dfs)
  
  filter_dfs
}
# factor_variables <- classes_vector(data_type = "factor", df = df2)
# marginal_distribution(df = df2, num_int_var = "PASSENGERID.present",
#                       factor_variables = factor_variables)



