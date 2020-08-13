###############3  
# df <- read_data("INFORMATION/FILES/saved.rds")
# factor <- c("PCLASS", "SEX")
# date_variable <- classes_vector(data_type = "Date", df = df)
# numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"),
#                                       df = df)
# df <- df[,c(factor, date_variable, numerc_int_variable), with = FALSE]
############### 

# Report Creator ----------------------------------------------------------
#' @description 
#' 1. If we select a year:
#' 1.1. Check that we only select a month or a semester or a quarter because 
#' if we select more than one then option is going to show a message string
#' 1.2 If we select only one period we want the dates information from the 
#' previous and the present dates, depend on if in the firm we select a month or 
#' a semester or a quarter or if only want the dates vector of the year:
#' - thanks to the functions *year_month* and *semester_quarter* we obtain 
#' the dates information saved in the list *date_info* from the previous and the 
#' present period sublists respectively iterating over the argument previous and 
#' past respectively
#' 1.3 Create the report:
#' - With the help of the function *report_all_NumIntVar*
#' obtaining a list *present_past_report* iterating over the list *date_info*,
#' (this is the present and past dates) that is why *present_past_report* is 
#' a list which the sublists of this list are going to be the reports from the 
#' past and the present. We need to terate again over the summary argument of 
#' the function *report_all_NumIntVar*, where each sublist is going to be a 
#' report with the summay and the report without the summary (only the grouped 
#' dataset)
#' - If from the list *present_past_report* each sublist (from Not_Summary
#' and Summary) from each have ONE report with nrow greater than 0 then bring 
#' that report (from present or past from Not_Summary and Summary sublists),
#' in other case we bringh both reports (present and past) from each sublist
#' (Not_Summary and Summary)
#' 1.4 Select type of report
#' 1.4.1 summary = TRUE select the summary report from the *present_past_report*
#' list
#' 1.4.1.0 If the sublist summary from the list *present_past_report*  do not
#' have any report, then send a string message
#' 1.4.1.1.If the sublist summary from the list *present_past_report* have one report
#' (present or past) then bring that report of the sublist Summary and convert 
#' it to data.table including in the name of each variable of the report .present 
#' or .past to identify if the report is from the present or the past.
#' 1.4.1.2 If the sublist summary from the list *present_past_report* have 
#' information both reports (present and past) then thanks to the function 
#' *iterative_merge* we merge both reports to get one report  by the factor 
#' variables, changing the suffixes .x and .y to .present and .past to identify 
#' the variables from each.
#' 1.4.1.2.1 In the report we have the respective information from present and past
#' based on the same reference variable (num or integer var), that is the reason 
#' that with the help of the function **difference_reference** we create the new 
#' variables that are going to be for each reference variable of the report the difference 
#' between the present and past of the same reference for each variable of the 
#' report and finaly in the final dataset we get the information of the present
#' and the difference for each numeric and integer variable, and in present 
#' variables we quit that suffix .present for each present variable.
#' 1.4.2 summary = FALSE select the Not_Summary report from the *present_past_report*
#' list
#' 1.4.2.0 If the sublist Not_Summary from the list *present_past_report*  do not
#' have any report, then send a string message
#' 1.4.2.1 If the sublist Not_Summary from the list *present_past_report* have 
#' one report (present or past) then bring that report of the sublist Not_Summary 
#' and convert it to data.table including in the name of each variable of the
#' report .present or .past to identify if the report is from the present or the 
#' past.
#' 1.4.2.2  In the report we have the respective information from present and past
#' we only bring the present information as a dataset including in the name of
#' each variable .present refering as the information of the present dataset.
#' 2. If we do not select a year (historical data)
#' With the help of the function *report_all_NumIntVar*
#' obtaining a list *historical data* iterating over the summary argument of 
#' the function *report_all_NumIntVar*, where each sublist is going to be a 
#' report with the summay and the report without the summary (only the grouped 
#' dataset).
#' 2.1 summary = TRUE We bring the historical summary report
#' 2.2 summary = FALSE we bring the historical grouped dataset
#' @param df dataset to obtain the report
#' @param year year that we want to obtain our report
#' @param month month of a year that we want to obtain our report
#' @param semester semester of a year that we want to obtain our report
#' @param quarter quarter of a year that we want to obtain our report
#' @param summary
#' *summary = TRUE*
#' - if we want the summary report 
#' *summary = FALSE*
#' - if we do not want the summary report (only a grouped dataset)
#' @return a report:
#' * If we want information of a specific date
#' - If summary = TRUE: 
#' -- If there is information in present and past: summary reflecting the 
#' information actual v.s. the previous period
#' -- If there is information in one period: summary reflecting the information
#' in that period
#' - If summary = FALSE:
#' -- If there is information in present and past: grouped dataset with all the 
#' numeric and integer variables in the present dates
#' -- If there is information in one period: grouped dataset with all the 
#' numeric and integer variables in that period
#' 
#' *If we want historical data (we do not care in the dates, we do not filter 
#' any type of date)
#' - If summary = TRUE: summary for each numeric and integer variable based
#' on the factor variables
#' - If summary = FALSE: A grouped dataset with all the numeric and integer 
#' variables
#' @Note the report is based on the factor variables that have the dataset
report_creator <- function(df,
                           year = NULL, month = NULL,
                           semester = NULL, quarter = NULL,
                           summary = FALSE){
  
  if(!is.null(year)){
    
    if((!is.null(month) & !is.null(semester) & !is.null(quarter)) |
       (!is.null(month) & !is.null(semester)) | 
       (!is.null(month) & !is.null(quarter)) | 
       (!is.null(semester) & !is.null(quarter))) 
      "Select only a Month or Semester or Quarter"
    else{
      # Define Date_info 
      if(!is.null(month)){
        date_info <- map(c(FALSE, TRUE),
                         ~year_month(df = df, year = year,
                                     month = month, previous = .x)) %>% 
          set_names(c("PRESENT", "PAST"))
        
      }else if(!is.null(semester)){
        date_info <- map(c(FALSE, TRUE), 
                         ~semester_quarter(df = df, year = year,
                                           semester = semester, past = .x)) %>% 
          set_names(c("PRESENT", "PAST"))
        
      }else if(!is.null(quarter)){
        date_info <- map(c(FALSE, TRUE),
                         ~semester_quarter(df = df, year = year,
                                           quarter = quarter, past = .x)) %>% 
          set_names(c("PRESENT", "PAST"))
        
      }else{
        date_info <- map(c(FALSE, TRUE),
                         ~year_month(df = df, year = year, previous = .x)) %>% 
          set_names(c("PRESENT", "PAST"))
      }
      # Creating the report 
      present_past_report <- map(c(FALSE, TRUE), function(summary){
        map(date_info, 
            ~report_all_NumIntVar(df = df, date_info = .x,
                                  date_filter = TRUE,
                                  summary = summary))
      }) %>% set_names(c("Not_Summary", "Summary"))
      
      present_past_report <- map(present_past_report, safely(function(subreports){
        if(any(map_int(subreports, ~.x[,.N]) == 0)){
          subreports <- subreports[map(subreports, ~.x[,.N]) > 0]
        }else subreports
        subreports
      })) %>% map(~.x[["result"]]) 
      
      if(summary){
        present_past_report <- present_past_report["Summary"]
        
        if(lengths(present_past_report) == 0) 
          final_report <- "You do not have any information in that period"
        
        else if(lengths(present_past_report) == 1){
          ## One period report
          final_report <- present_past_report %>% flatten_df() %>% as.data.table()
          
          final_report %>% setnames(old = names(final_report),
                                    new = str_c(names(final_report),
                                                tolower(names(present_past_report[["Summary"]])),
                                                sep = "."))
        }else{
          ## Two periods report
          factor_variable <- classes_vector(data_type = "factor", df = df)
          
          present_past_report <- iterative_merge(dfs_list = present_past_report[["Summary"]],
                                                 key = factor_variable)
          # Replacing the Names of each variable 
          names_variables <- map(c(".x$", ".y$"), 
                                 ~str_subset(string = names(present_past_report), 
                                             pattern = .x)) %>% 
            set_names(c("PRESENT", "PAST"))
          
          new_names <- pmap(list(names_variables, c(".x$", ".y$"), c(".present", ".past")),
                            function(x,y,z){
                              str_replace_all(string = x, 
                                              pattern = y, replacement = z)
                            })
          walk2(names_variables, new_names,
                ~present_past_report %>% setnames(old = .x, new = .y))
          
          # Difference 
          identify_vars <- str_subset(string = names(present_past_report), 
                                      pattern = ".present$|.past$") %>% 
            str_remove_all(pattern = ".present$|.past$") %>% unique()
          
          identify_vars_all <- map(identify_vars, 
                                   ~str_subset(string = names(present_past_report), 
                                               pattern = .x))
          
          walk2(identify_vars_all, glue("^.{identify_vars}"), #define new variable as ^name
                ~present_past_report[,(.y):=difference_reference(df = present_past_report, 
                                                                 time_var = .x)])
          # variables .present and ^.
          variables_final_report <- str_subset(string = names(present_past_report), 
                                               pattern = "^(?!.*[.]past$).*$")  #everything that not end in .past
          
          present_past_report <- present_past_report[,variables_final_report, 
                                                     with = FALSE]
          #replacing names of .present
          names.present <- str_subset(string = names(present_past_report), 
                                      pattern = ".present$")
          actual_names <- str_remove_all(string = names.present, 
                                         pattern = ".present$")
          present_past_report %>% setnames(old = names.present, new = actual_names)
          
          final_report <- present_past_report
        }
      }else{
        present_past_report <- present_past_report["Not_Summary"]
        
        if(lengths(present_past_report) == 0) 
          final_report <- "You do not have any information in that period"
        
        else if(lengths(present_past_report) == 1){
          final_report <- present_past_report %>% flatten_df() %>% as.data.table()
          
          final_report %>% setnames(old = names(final_report),
                                    new = str_c(names(final_report),
                                                tolower(names(present_past_report[["Not_Summary"]])),
                                                sep = "."))
        }else{
          final_report <- present_past_report[["Not_Summary"]][["PRESENT"]]
          
          final_report %>% setnames(old = names(final_report),
                                    new = str_c(names(final_report),
                                                "present",
                                                sep = "."))#
        }
      }
    }
  }else{
    historical_reports <- map(c(FALSE, TRUE), safely(
      ~report_all_NumIntVar(df = df, date_filter = FALSE, #historical
                            summary = .x))) %>% 
      map(~.x[["result"]]) %>% set_names(c("Not_Summary", "Summary"))
    
    if(summary){final_report <- historical_reports[["Summary"]]}
    else {final_report <- historical_reports[["Not_Summary"]]}
  }
  final_report
}
#report_creator(df = df, year = 2018, month = "JANUARY", summary = TRUE)
#report_creator(df = df, year = 2018, month = "JANUARY", summary = FALSE)
# report_creator(df = df, year = 2018, month = "NOVEMBER", summary = TRUE)
# report_creator(df = df, year = 2018, month = "DECEMBER", summary = TRUE)
# report_creator(df = df, year = 2018, month = "NOVEMBER", summary = FALSE)
# report_creator(df = df, year = 2018, month = "DECEMBER", summary = FALSE)
# report_creator(df = df, year = 2020, semester = 1, summary = TRUE)
# report_creator(df = df, year = 2020, quarter = 3, summary = TRUE)
# report_creator(df = df, summary = TRUE) #historical
# report_creator(df = df, summary = FALSE)

#' @SUBFUNCTION
#' @description in the *report_creator* function we are trying to create new 
#' variable that is going to be the difference between the present and past
#' that is the reason that we have a list *identify_vars_all* which each sublist
#' is the the same var in the present report and the same var in the past report.
#' that is the reason that the variables present and past in the function are the
#' respective present or past variable from the list and we make an arithmetic
#' difference between those two variables.
#' @param df dataset to obtanin the vector
#' @param time_var same variable with the termination .present and .past 
#' @return vector with the difference of the present and past variables from
#' the same reference variable
difference_reference <- function(df, time_var){
  
  present <- str_subset(string = time_var, pattern = ".present$")
  past <- str_subset(string = time_var, pattern = ".past$")
  
  df[,time_var, with = FALSE] %>% 
    .[,get(present)-get(past)] %>% 
    return()
} 
#difference(df = present_past_report, time_var = identify_vars_all[[1]])

#' @SUBFUNCTION
#' @description 
#' 1. Create variables with the factor(s) and numeric variables of the dataset
#' 2. with the help of the function *report_unique_NumIntVar* we can create a 
#' report in function of one numeric or integer variable. 
#' That is the reason that in this part we iterate over all numeric and integer 
#' variables, then the variable *each_report* is going to be a list, which each
#' sublist is a report applied to a specific numeric or integer variable, naming 
#' each sublist as the integer or numeric variable.
#' Where this list *each_report* we need to iterate again over the summary 
#' argument of the function *report_unique_NumIntVar*, where each sublist
#' is going to be a report with the summary and the report without the summary
#' (only the grouped dataset)
#' 2.1 date_filter = TRUE
#' means that we want that our date variable is filtered with specific dates in
#' the function *report_unique_NumIntVar*
#' 2.2 date_filter = FALSE
#' means that we do not want that our date variable is filtered with specific 
#' dates in the function *report_unique_NumIntVar* (We want the historical data)
#' 3. Select type of report
#' 3.1 summary = TRUE
#' Thanks to the reason that each report is created based on a 
#' factor variable we merge with the help of the function *iterative_merge*,
#' each report (sublists of the list *each_report*) by the factor variables
#' 3.2 summary = FALSE
#' We want the grouped dataset with all the numeric and integer variables, thanks
#' to the function *iterative cbind* we get a report (grouped dataset) with
#' all this numeric and integer variables.
#' @param df dataset to obtain the report
#' @param date_filter works equal to the *report_unique_NumIntVar* function
#' @param date_info works equal to the *report_unique_NumIntVar* function
#' @param summary works equal to the *report_unique_NumIntVar* function
#' @Note The arguments works equal because this created function is based on the
#' *report_unique_NumIntVar* function and use its parameters in the same way
#' @Note The diference between this function and the other one, is that in were
#' we use *report_unique_NumIntVar* and iterate over all the numeric and integer
#' variables (not only one numeric or integer variable) and obtain one dataset
#' @return a report:
#' If date_filter = TRUE
#' A report *from one period (present or past) indicated with date_info*
#' If date_filter = FALSE
#' A report without carrying about the dates (historical Data)
#' In any option the report: 
#' Is based on the factor variable applied to each *numeric or integer variable*
#' 
#' If summary = TRUE:
#' creating three variables FOR EACH NUMERIC AND INTEGER VARIABLE:
#' N.num = number of observations for each level of the factor variable 
#' SUM.num = sum of observations for each level of the factor variable
#' %.num = percentage of participation for each level of the factor variable
#' If summary = FALSE:
#' A grouped dataset with all the numeric and integer variables
report_all_NumIntVar <- function(df, date_info = NULL, date_filter = FALSE,
                                 summary = FALSE){
  
  numerc_int_variable <- classes_vector(data_type = c("integer", "numeric"), 
                                        df = df)
  factor_variable <- classes_vector(data_type = "factor", df = df)
  
  if(date_filter){
    each_report <- map(c(FALSE, TRUE), function(summary){
      map(numerc_int_variable, 
          ~report_unique_NumIntVar(df = df, 
                                   date_info = date_info, 
                                   num_int_var = .x,
                                   date_filter = TRUE,
                                   summary = summary))
    }) %>% set_names(c("Not_Summary", "Summary"))
    
  }else{
    each_report <- map(c(FALSE, TRUE), safely(function(summary){
      map(numerc_int_variable, 
          ~report_unique_NumIntVar(df = df,  
                                   num_int_var = .x,
                                   date_filter = FALSE,
                                   summary = summary))})) %>% 
      map(~.x[["result"]]) %>% set_names(c("Not_Summary", "Summary"))
  }
  
  if(summary){
    each_report <- each_report[["Summary"]]
    each_report <- each_report %>% set_names(numerc_int_variable)
    final_report <- iterative_merge(dfs_list = each_report, key = factor_variable)
  }
  else{
    each_report <- each_report[["Not_Summary"]]
    each_report <- each_report %>% set_names(numerc_int_variable)
    final_report <- iterative_cbind(dfs_list = each_report)
  } 
  final_report
}
# report_all_NumIntVar(df = df, date_info = date_info$PRESENT, date_filter = TRUE,
#                      summary = TRUE)
# report_all_NumIntVar(df = df, date_filter = FALSE, summary = TRUE) #same as under
# report_all_NumIntVar(df = df, date_info = date_info$PRESENT, date_filter = FALSE, #HISTORICAL
#                      summary = FALSE)


#' @description from the list of datasets we cbind the datasets, this is put in
#' a dataset all the columns from all the datasets. After that from the final
#' dataset we only select the unique variable (not repeated variables)
#' @param dfs_list list of datasets
#' @return dataset with cbinded multiple datasets
iterative_cbind <- function(dfs_list){
  
  df <- Reduce(function(x,y) cbind(x,y), x = dfs_list)
  
  df[,(unique(names(df))), with = FALSE] %>% 
    return()
}

#' @SUBFUNCTION
#' @description 
#' 1. Create variables with the factor(s) and date variable of the dataset
#' 2. Define our dataset with the variables of the previous ones and the 
#' numeric or integer variable defined in the firm, this dataset needs to be 
#' grouped based on the factor(s) variable(s) of the dataset, because the report 
#' is based on the levels of each factor variable
#' After we defined our dataset:
#' 3. Filter dataset
#' 3.1 if date_filter = TRUE: 
#' means that we want that our date variable is filtered with specific dates 
#' defined with the parameter date_info defined with the functions *year_month* 
#' or *semester_quarter*
#' 3.2 if date_filter = FALSE:
#' means that we do not care of date variable, this is the reference to get the 
#' historical data of each level
#' 4. Select type of report 
#' 4.1 summary = TRUE
#' 4.1.1 We create our report with different calculations based on the factor(s)
#' variable(s) applied to one numeric or integer variable.
#' 4.1.2 Define the name of the new created calculated variables, including in the 
#' each name, the name´s variable of the numeric or integer variable 
#' 4.2 summary = FALSE : Return the grouped dataset.
#' @param df dataset to obtain the report
#' @param date_filter
#' *date_filter = TRUE*
#' - if we want date variable is filtered with specific dates 
#' @param date_info if date_filter = TRUE then this argument indicates the 
#' specific dates (obtained with *year_month* or *semester_quarter* function)
#' *date_filter = FALSE*
#' - if we want date variable is NOT filtered with specific dates 
#' @param num_int_var one numeric or integer variable of the dataset to applied
#' to create the report
#' @param summary 
#' *summary = TRUE*
#' - if we want the summary report 
#' *summary = FALSE*
#' - if we do not want the summary report (only a grouped dataset)
#' @return a report:
#' If date_filter = TRUE
#' A report *from one period (present or past) indicated with date_info*
#' If date_filter = FALSE
#' A report without carrying about the dates (historical Data)
#' In any option the report: 
#' Is based on the factor variable applied to *ONE numeric or integer variable*
#' 
#' If summary = TRUE:
#' creating three variables:
#' N.num = number of observations for each level of the factor variable 
#' SUM.num = sum of observations for each level of the factor variable
#' %.num = percentage of participation for each level of the factor variable
#' If summary = FALSE:
#' A grouped dataset
report_unique_NumIntVar <- function(df, date_info = NULL, num_int_var, 
                                    date_filter = FALSE, summary = FALSE){
  
  factor_variable <- classes_vector(data_type = "factor", df = df)
  date_variable <- classes_vector(data_type = "Date", df = df)
  
  df <- copy(df)[,c(factor_variable, date_variable, num_int_var), with = FALSE] %>% 
    .[,.SD, by = factor_variable] 
  
  if(date_filter){df <- df[get(date_variable) %in% date_info]}  
  else{df <- df} #historical
  
  if(summary){
    df <- df[,.(.N,
                sum(get(num_int_var), na.rm = TRUE),
                sum(get(num_int_var), na.rm = TRUE)/sum(df[,get(num_int_var)], 
                                                        na.rm = TRUE)),
             by = factor_variable] 
    
    subset_names <- str_c(factor_variable, collapse = "|")
    
    df %>% setnames(old = str_subset(string = names(df), 
                                     pattern = glue("^(?!(?:{subset_names})).*$")), #everything except factor variables
                    new = str_c(c("N", "SUM", "`%`"), num_int_var, sep = "."))
  }else df
  df 
}
# report_unique_NumIntVar(df = df,
#                         num_int_var = "PASSENGERID", date_filter = FALSE, #HISTORICAL
#                         summary = TRUE)
# 
# report_unique_NumIntVar(df = df,
#                         date_info = date_info$PRESENT,
#                         num_int_var = "PASSENGERID", date_filter = TRUE,
#                         summary = TRUE)

# Dates Information -------------------------------------------------------
#monthly
#semester
#quarter
#yearly
#historical
specific_month <- c("JANUARY", "FEBRUARY", "MARCH", 
                    "APRIL", "MAY", "JUNE", 
                    "JULY", "AUGUST", "SEPTEMBER", 
                    "OCTOBER", "NOVEMBER", "DECEMBER")
#' @description 
#' 1. check if the dataset has a date variable if not then send a string message,
#' In other case:
#' 2. check if the year in the firm of the function is within the possible years 
#' in the dataset if not then send a string message
#' In other case Define the year and/or the month:
#' 2.1 - if previous = TRUE: 
#' 2.1.1 Define the year: as the previous one if there is a previous if not send 
#' a string message
#' 2.1.2 Define the month: define the month as the previous one if there is 
#' a previous this is the length is greater than 0, if not send a string message
#' 2.2 - if previous = FALSE:define the year and the month as the actual one.
#' 3.After define the year and/or the month:
#' 3.1. If the firm only contain a year
#' From the dates variable we subset only the year that we defined previously
#' if there is a year if not we are referring to the string message
#' 3.2. If we select a year and a month
#' if is the previous month is the string message then we show that message, if 
#' not, from the *info_year* (where is the subset of the year indicated in the
#' firm) we subset on this only the month that we defined.
#' If we don´t have information about that month
#' this is the length equal to 0 then send a string message in other case show
#' the subset information of a month of a year.
#' @param df dataset to obtain the dates vector
#' @param year year that we want to obtain dates
#' @param month month that we want to obtain dates
#' @param previous:
#' *previous = TRUE*
#' - if we want dates of the previous year from the selected year
#' - if we want dates of the previous month from the selected month of a year
#' *previous = FALSE*
#' - return the dates of the actual year or the dates of the actual month of a year
#' @return vector of the available dates in:
#' - a year
#' - a month of a specific year  
year_month <- possibly(
  
  function(df, year, month = NULL, previous = FALSE){
    
    date_variable <- classes_vector(df = df, data_type = "Date")
    
    if(length(date_variable) == 0) 
      "You don´t have any Date variable in your dataset"
    else{
      if(!as.character(year) %in% year(df[,get(date_variable)])) 
        info_year <- "Define a correct year"
      
      else{
        # Define Year and Month 
        if(previous){
          # Previous Information 
          if(is.null(month)){
            if((as.numeric(year)-1) %in% year(df[,get(date_variable)])) 
              year <- (as.numeric(year)-1)
            else year <- "There is no information on the Previous Year" 
            
          }else{
            months <- 1:12
            months <- ifelse(str_length(months) == 1, glue("0{months}"), months)
            names(months) <- specific_month
            previous_month_index <- (which(x = months == months[month])-1) %>% #work with the index
              unname()
            
            if(length(months[previous_month_index]) > 0)
              month <- months[previous_month_index] %>% names()
            else month <- "There is no information on the Previous month"
          }
        }else{
          # Actual Information 
          if(is.null(month)) year <- year
          else year <- year ; month <- month
        }
        
        if(year == "There is no information on the Previous Year")
          info_year <- year
        else
          info_year <- str_subset(string = df[,get(date_variable)],
                                  pattern = as.character(year)) %>% 
            assign_date_type()
        
        if(is.null(month)){
          # Info of a Year 
          info_year
        }else{
          # Info of a month of that year 
          months <- 1:12
          months <- ifelse(str_length(months) == 1, glue("0{months}"), months)
          names(months) <- specific_month
          
          if(is.Date(info_year)){
            
            if(month == "There is no information on the Previous month")
              info_month <- month
            else{
              info_month <- str_subset(string = info_year, 
                                       pattern = glue("(?<=[-]){months[month]}(?=[-])"))
              
              if(length(info_month) == 0) info_month <- "Define a correct month"
              else info_month <- assign_date_type(info_month)
              info_month
            }
          }else info_year
        }  
      }
    }
  }, otherwise = "You need to define a correct Year")
# year_month(df = df) 
# year_month(df = df[,-"DATE"])
# year_month(df = df, year = 2017) %>% return() 
# 
# year_month(df = df, year = 2019, previous = TRUE) %>% return()
# year_month(df = df, year = 2018, previous = TRUE) %>% return() 
# year_month(df = df, year = 2018, month = "DECEMBER", previous = TRUE) %>% return()
# year_month(df = df, year = 2018, month = "JANUARY", previous = TRUE) %>% return()
# 
# year_month(df = df, year = 2019, previous = FALSE) %>% return()
# year_month(df = df, year = 2018, previous = FALSE) %>% return() 
# year_month(df = df, year = 2018, month = "DECEMBER", previous = FALSE) %>% return()
# year_month(df = df, year = 2018, month = "OCTOBER", previous = FALSE) %>% return()
# 
# year_month(df = df, year = 2018, month = "DECEMBER") %>% length()

#' @description 
#' 1. Check that we only selected a semester or a quarter, if we selected both
#' then show string message
#' 2.Define the year: (thanks to the function *year_month* we can know if the
#' year of the firm is within the possible years of the dates variable)
#' 2.1 if past = TRUE: 
#' Define the year as the previous one if there is a previous
#' if not send a string message
#' 2.2 if past = FALSE: 
#' Define the year as the actual one
#' After define the year:
#' if the year is a string message (for the previous year) then the dates also 
#' is going to be, 
#' In other case:
#' 3. If we select a semester:
#' 3.1 If we select the first semester [1], thanks to the function *year_month*,
#' we can show a vector of a month of a specific year, then we iterate from
#' the first to the sixth month of a year defined (where the argument 
#' previous = FALSE is because we want the information of the actual year) 
#' getting a list, where each sublist is the information of a month of the 
#' semester using compact to delete nulls if there is no dates in a month.
#' 3.2 If we select the second semester [2], the process is the same as the 
#' first semester, and so on ...
#' 4. If we select a quarter: 
#' The process is the same as the semester, the only
#' difference is that in each quarter (each list) we select the respective months.
#' 5.For the list created before we filter the sublists that are data type date,
#' (we don´t want the string messages) This is the months (of the specific 
#' semester or quarter) that have dates information.
#' 5.1 If we don´t have any information in any sublists then send a string message
#' 5.2 If we have information in at least one sublist then we convert that list
#' dates in a vector.
#' @param df dataset to obtain the dates vector
#' @param year year that we want to obtain dates
#' @param semester 1 or 2 from the semester that we want to obtain dates
#' @param quarter 1,2,3 or 4 from the quarter that we want to obtain dates
#' @param past:
#' *past = TRUE*
#' - if we want dates of the same semester from the previous year 
#' example: semester 2 of 2019 -> semester 2 of 2018
#' - if we want dates of the quarter from the previous year
#' *past = FALSE*
#' - return the dates of the actual semester or quarter of the actual year
#' @return vector of the available dates in:
#' - a semester of a year (first or second semester)
#' - a quarter of a year  (first, second, third or fourth quarter)
semester_quarter <- possibly(
  
  function(df ,year, semester = NULL, quarter = NULL, past = FALSE){
    
    if(!is.null(semester) & !is.null(quarter)) 
      "Select only a semester or quarter"
    else{
      
      if(past){
        date_variable <- classes_vector(df = df, data_type = "Date")
        
        if((as.numeric(year)-1) %in% year(df[,get(date_variable)]))
          year <- (as.numeric(year)-1)
        else year <- "There is no information on the Previous Year"
        
      }else year <- year
      
      if(year == "There is no information on the Previous Year") dates <- year
      else{
        # Info Semester 
        if(!is.null(semester)){
          if(semester == 1){
            dates <- map(specific_month[1:6],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[1:6]) %>% compact()
          }
          else if(semester == 2){
            dates <- map(specific_month[7:12],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[7:12]) %>% compact()
          }
          else NULL
        }
        # Info Quarter 
        else if(!is.null(quarter)){
          if(quarter == 1){
            dates <- map(specific_month[1:3],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[1:3]) %>% compact()
          }
          else if(quarter == 2){
            dates <- map(specific_month[4:6],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[4:6]) %>% compact()
          }
          else if(quarter == 3){
            dates <- map(specific_month[7:9],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[7:9]) %>% compact()
          }
          else if(quarter == 4){
            dates <- map(specific_month[10:12],
                         ~year_month(df = df, year = year, 
                                     month = .x, previous = FALSE)) %>% 
              set_names(specific_month[10:12]) %>% compact()
          }
          else NULL
        }
        
        dates <- dates[map(dates,~is.Date(.x)) == TRUE] #filter only sublists that are date
        
        if(length(dates) == 0) dates <- glue("No dates in that period of the year {year}")
        else{
          dates <- do.call(what = "c", args = dates) %>%    #convert list as vector and respecting the datatype
            unname() #remove the names of the vector
        }
        dates
      }
    }
  }, otherwise = "Define a correct semester or quarter")
# semester_quarter(df = df, year = 2018, semester = 2, quarter = 3)
# semester_quarter(df = df, year = 2018, semester = 2)
# semester_quarter(df = df, year = 2019, quarter = 4, past = TRUE)



