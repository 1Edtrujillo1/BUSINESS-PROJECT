
# Edit Data ---------------------------------------------------------------

#' @description All the possible special characters
special_chrs = c('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A',
                 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N',
                 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a',
                 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i',
                 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y',
                 'þ'='b', 'ÿ'='y', 'ã'='a')

#' @description: 
#' @1.Import a dataset of any type of termination (.sas, .rds, .txt, .csv, .xls, 
#' .xlsx)
#' @2.Add the parameter col_select, to select variables that we want from the 
#' dataset
#' @3.Delete special characters
#' @4.Toupper Variables 
#' @5.Toupper observations
#' @6.Assign the correct data type to each variable
#' @7.Change "" strings to N/A
#' @param path and arguments of importing
#' @return a clean dataset
read_data <- function(path, encoding = 'UTF-8', sheet = 1, col_select = NULL, 
                      ...){
  
  options(encoding = encoding, warn = -1) #warn = -1 hide warnings
  
  if(str_detect(string = path, pattern = ".sas7bdat$"))
    df <- haven::read_sas(data_file = path, encoding = encoding, ...)
  else if(str_detect(string = path, pattern = ".rds$"))
    df <- readRDS(file = path, ...)
  else if(str_detect(string = path, pattern = ".txt$"))
    df <- read.table(file = path, header = TRUE, encoding = encoding, 
                     na.strings = "NA", ...)
  else if(str_detect(string = path, pattern = ".csv$"))
    df <- read.csv(file = path, header = TRUE, encoding = encoding, 
                   na.strings = "NA", ...)
  else if(str_detect(string = path, pattern = "\\.xls$|\\.xlsx$"))
    df <- readxl::read_excel(path = path, sheet = sheet, col_names = TRUE, 
                             na = "", ...)
  else NULL
  
  df <- df %>% as.data.table() %>% 
    .[,(unique(names(df))), with = FALSE]
  
  if(!is.null(col_select)){
    df <- df[,(unique(col_select)), with = FALSE]
  }
  
  df <- df[,names(df):=lapply(.SD, str_replace_all, special_chrs)] %>% 
    setnames(toupper(names(df))) %>% 
    .[,lapply(.SD, toupper), .SDcols = names(df)] %>% 
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]
  
  character_na <- c('\\A\\z'='N/A') #change "" strings to N/A string
  character_variables <- classes_vector(data_type = c("character", "factor"), 
                                        df = df)
  
  df <- df[,(character_variables):=map(.SD, str_replace_all, character_na), 
           .SDcols = character_variables] %>% 
    .[,(character_variables):=map(.SD, str_trim),
      .SDcols = character_variables] %>% 
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]
  
  return(df)
  
}
#a <- "C:/Users/actje/Dropbox/PROGRAM_FILES/1. PROJECT/STATISTICS/INFORMATION/FILES/my_data.csv"
#df <- read_data(path = a)

#' @description this function assign the correct datatype of a variable, and is 
#' useful when we want to assign the correct datatype to each variable of a dataset 
#' @param variable
#' @return variable with the correct data type 
assign_data_type <- function(variable){
  
  values_type <- map(variable,function(variable){
    if(is.na(variable)) NA
    else if(str_detect(string = variable, pattern = "[A-Z]+")) "character"
    else if(str_detect(string = variable, pattern = "[0-9]+[.][0-9]+")) "numeric"
    else if(str_detect(string = variable, pattern = ".+\\/.+\\/.+")) "date"
    else if(is.Date(variable)) "date"
    else "integer"}
  ) %>% flatten_chr() 
  
  type <- table(values_type) %>% sort(decreasing = TRUE)   #sort based on type most repeated
  
  if(any(names(type) == "character")) type <- "character"   #character, datatype <- character
  else if(any(names(type) == "numeric")) type <- "numeric" #integer, numeric <- numeric
  else if(any(duplicated(type))) type <- "character" #if any count of the table is equal with other datatype
  else  type <- type %>% head(1) %>% names() #datatype most repeated for all its observations
  
  variable_notNA <- table(variable)[str_subset(string = names(table(variable)),
                                               pattern = "[^N/A]+")]  #everything except an empty string "" .+
  
  if(type %in% c("character", "integer")){
    if(length(variable_notNA) == sum(variable_notNA >= 3)) type <- "factor" #*each observation* is repeated at least 3 times is a factor
    else type}
  else type
  
  if(type == "character") variable <- as.character(variable)
  else if(type == "numeric") variable <- as.numeric(variable)
  else if(type == "integer") variable <- as.integer(variable)
  else if(type == "factor") variable <- as.factor(variable)
  else if(type == "date") variable <- assign_date_type(variable = variable)
  
  variable
}
# for(i in 1:length(df)){
#   print(paste0(class(assign_data_type(df[[i]])), names(df)[[i]], sep = "," ))
# }
#assign_data_type(df$SPECIES) %>% class()
# pmap(list(map(names(df), ~df[,get(.x)]) %>% set_names(names(df))), 
#      assign_data_type) %>% map(~class(.x))

#' @SUBFUNCTION 
#' @description this function check if a variable is date and if it is then 
#' return the variable, in other case return a date type with all the 
#' possibilities (Permutation 3 to 3)to return the date with the format 
#' ISO 8601 YYYY-MM-DD
#' @param variable
#' @return variable with date as the data type  
assign_date_type <- function(variable){
  
  if(is.Date(variable)) variable <- variable
  
  else{
    variable <- as.character(variable)
    
    date_type <- map_dfc(variable, function(variable){
      
      if(str_detect(string = variable, 
                    pattern = "^(?:[1-9]|0[1-9]|1[0-2])\\/.{1,2}\\/.{1,4}"))
        variable <- mdy(variable)
      else if(str_detect(string = variable, 
                         pattern = "^(?:[1-9]|0[1-9]|1[0-2])\\/.{1,4}\\/.{1,2}"))
        variable <- myd(variable)
      else if(str_detect(string = variable, 
                         pattern = "^.{1,2}\\/(?:[1-9]|0[1-9]|1[0-2])\\/.{1,4}"))
        variable <- dmy(variable)
      else if(str_detect(string = variable, 
                         pattern = "^.{1,4}\\/(?:[1-9]|0[1-9]|1[0-2])\\/.{1,2}"))
        variable <- ymd(variable)
      else if(str_detect(string = variable, 
                         pattern = "^.{1,2}\\/.{1,4}\\/(?:[1-9]|0[1-9]|1[0-2])"))
        variable <- dym(variable)
      else if(str_detect(string = variable, 
                         pattern = "^.{1,4}\\/.{1,2}\\/(?:[1-9]|0[1-9]|1[0-2])"))
        variable <- ydm(variable)
    })
    
    date_type <- gather(date_type, names(date_type),
                        key = "name_vars" , value = "date") %>% 
      data.table()
    
    variable <- date_type[["date"]] 
  }
  variable
}

#' @description this function brings all the variables´ names of a dataset that  
#' are in a certain data type
#' @param data_type 
#' @param df
#' @return vector of strings of variables that are in a certain data type
classes_vector <- function(data_type, df){
  
  list_types <- map(names(df), ~df[,class(get(.x))]) %>% 
    map(~str_subset(string = .x, pattern = data_type)) %>% 
    set_names(names(df))
  
  list_types[map(list_types,length)>0] %>% 
    names() %>% return()
  
}
#classes_vector(data_type = "numeric", df = df)


# Outliers ----------------------------------------------------------------

#' @description this function first calculate the z-score of each variable
#' (numeric or integer) grouped by a level(s) of a dataset and assign it 
#' as a new variable (for each calculated), after that for each new calculated 
#' variable that has a z-score +-3 then the observation of the original variable 
#' is going to be a NA (to not affect the distribution of the variable) and 
#' finally delete the calculated variables, to only have the originalvariables 
#' without the outliers
#' @param df 
#' @param level=factor_variable
#' @return original dataset without the outliers of each variable
delete_outliers <- function(df, level){
  
  factor_variables <- classes_vector(data_type = "factor", df = df)
  
  choose_level <- factor_variables[factor_variables %like% paste(level, 
                                                                 collapse = "|")] #filter selected factor variables
  
  df <- df[,.SD, by = choose_level]
  
  params <- list(variable = classes_vector(data_type = c("numeric", "integer"), 
                                           df = df),
                 no_sd = str_c("no_sd", classes_vector(
                   data_type = c("numeric", "integer"), df = df), 
                   sep = "_")
  )
  pwalk(params, function(variable, no_sd){
    df[,(no_sd):= 
         (get(variable)-mean(get(variable), na.rm = TRUE))/sd(get(variable), na.rm = TRUE), 
       by = choose_level]
  })
  
  numb_sd <- str_subset(string = classes_vector(
    data_type = c("numeric", "integer"), df = df), 
    pattern = "^no_sd")  #everything that starts with no_sd
  variable <- str_subset(string = classes_vector(
    data_type = c("numeric", "integer"), df = df), 
    pattern = "^(?!^no_sd).*") #everything that NOT starts with no_sd
  
  df2 <- df
  for(i in 1:length(variable)){
    numb_sd_var <- numb_sd[i] 
    var <- variable[i]
    df2[,(var):=ifelse(get(numb_sd_var) > -3 & get(numb_sd_var) < 3, get(var), NA), 
        by = choose_level] #in this interval the observatioon is not an outlier 
  }
  
  df2[,(numb_sd):=NULL] %>% 
    return()
}
#A <- delete_outliers(df = copy(A), level = "SPECIES")

#' @description this function first calculate the number of NA´s before applying 
#' the delete_outliers function and get a *list* called 
#' *count_variables_with_outliers* of the counts of each numeric or integer variable. 
#' Then we apply the function *delete_outliers* to the dataset 
#' for each factor variable of the dataset, and get a list with the same dataset 
#' applied with the function delete_outliers by each factor, for this we will have 
#' a list of datasets. After that we create a *list* called 
#' *count_variables_without_outliers* which each element is the 
#' counts of NA´s of each calculated dataset of each numeric or integer variable.
#' With the help of the subfunction count_difference_outliers we create a list 
#' called *count_difference_variables* calculating the difference of the elements 
#' it the list count_variables_without_outliers and the elements of the list
#' count_variables_with_outliers.
#' The previous to last step is to create the list num_int_count_each_factor, 
#' that based on the previous list, this new list take each first row of each 
#' sublist (representing the factor variables applied to each numeric variables)
#' and creating a sublist to this new list and so on with the rows. And finally 
#' we sort this new list.
#' @param df 
#' @return list whith sublists which each name of the sublist is the 
#' *numeric or integer variable* of the dataset and the names elements of 
#' each sublist are the *factor variables* of the dataset where each elemnt is the 
#' difference of NAS between each numeric or integer variable before and after 
#' applying delete_outliers function sorted from the higher to lower applied 
#' factor variable to the numeric or integer variable
recomended_level_outliers <- possibly(
  function(df){
    
    num_int_vars <- classes_vector(data_type = c("numeric", "integer"), df = df)
    
    count_variables_with_outliers <- map(num_int_vars, ~df[is.na(get(.x)), .N]) %>% 
      set_names(num_int_vars)
    
    factor_vars <- classes_vector(data_type = "factor", df = df)
    
    dfs_without_outliers <- map(factor_vars,
                                delete_outliers, df = copy(df)) %>% 
      set_names(factor_vars)
    
    count_variables_without_outliers <-  
      map(dfs_without_outliers,
          function(df_w_ou){
            map(num_int_vars, ~df_w_ou[is.na(get(.x)), .N]) %>% 
              set_names(num_int_vars)}) %>% 
      set_names(factor_vars)
    
    count_difference_variables <- 
      map(count_variables_without_outliers, count_difference_outliers, 
          NAs_with_outliers = count_variables_with_outliers)
    
    if(length(count_difference_variables) == 1){
      num_int_count_each_factor <- 
        pmap(list(i = 1:lengths(count_difference_variables)), #lengths count the elements of a sublist
             function(i){
               map(count_difference_variables,i)
             })
    }else{
      num_int_count_each_factor <- 
        pmap(list(i = 1:length(count_difference_variables)), #lengths count the elements of a list
             function(i){
               map(count_difference_variables,i)
             }) 
    }
    num_int_count_each_factor <- 
      num_int_count_each_factor%>% map(compact) %>% compact() %>% #discard NULL´s inside and outside
      set_names(num_int_vars)
    
    sorted_num_int_count_each_factor <- 
      pmap(list(i = 1:length(num_int_count_each_factor)),
           function(i) {
             num_int_count_each_factor[[i]][order(unlist(num_int_count_each_factor[[i]]),
                                                  decreasing=TRUE)]}) %>% 
      set_names(num_int_vars)
    
    sorted_num_int_count_each_factor  
    
  }, otherwise = "You don´t have any factor variable")

#' @SUBFUNCTION 
count_difference_outliers <- function(NAs_without_outliers, NAs_with_outliers){
  
  num_int_vars <- classes_vector(data_type = c("numeric", "integer"), df = df)
  
  pmap(list(i = 1:length(NAs_without_outliers),
            j = 1:length(NAs_with_outliers)),
       function(i,j){
         NAs_without_outliers[[i]]-NAs_with_outliers[[j]] %>% 
           return()
       }) %>% set_names(num_int_vars)
}


# Adding ID ---------------------------------------------------------------

#' @description This function is specialy for shiny "reactive_raw_data",
#' that return a vector of strings where if the first three variables of the 
#' dataset are data type factor then return as vector of string the names of 
#' that three variables, but if there are two variables then the vector are
#' the name of that two variables, but if there is only the first variable then 
#' the vector is going to be the name of that first factor variable. 
#' In other case return the string First you need to delete outliers
#' @param df 
#' @return vector of strings of firsts factor variables
optimal_factor_variables <- function(df){
  
  indexes_variables <- list(three_variables = 1:3,
                            two_variables = 1:2,
                            first_variable = 1)
  
  first_variables <- 
    map(indexes_variables, ~ every(pmap(list(i = .x), 
                                        function(i)is.factor(df[[i]])),
                                   ~isTRUE(.x)))
  if(first_variables$three_variables) 
    variables <- map_chr(indexes_variables$three_variables, ~names(df)[[.x]])
  
  else if(first_variables$two_variables) 
    variables <- map_chr(indexes_variables$two_variables, ~names(df)[[.x]])
  
  else if(first_variables$first_variable) 
    variables <- map_chr(indexes_variables$first_variable, ~names(df)[[.x]])
  
  else variables <- "First you need to delete outliers"
  
}
#every(pmap(list(i = 1:3) ,function(i)is.factor(df[[i]])),~isTRUE(.x))

#' @description this function create a key where if don´t have a date then we 
#' create a DATE variable as asecuence of days based on a level and a unique key 
#' as a paste of the digit of the day,month and year and a number that represent
#' the level of the factor variable. 
#' But if we have a date then we create only a unique key in the same way that 
#' the previous step.
#' @NOTES:
#' today()+(1:length(vector)) is:
#' sequence of days from today to the day that represent the last element 
#' of the vector
#' 
#' today()+(1:length(vector))*days(-1) is:
#' days(-1) invert the order, sequence this is from the day that represent the first 
#' element of the vector to yesterday "today()-1)"
#' 
#' sort(today()+(1:length(df1$ID_REGION))*days(-1)) is:
#' sorted day from the lowest to the gratest
#' 
#' @NOTES:SELECT_ONLY_ONE_level_=_FACTOR_VARIABLE
#' @param df 
#' @param level=factor_variable
#' @return key for a dataset (with the intention that in the future we use this
#' variable as an ID for merges)
key_creation <- function(df, level){
  
  if(length(classes_vector(data_type = "Date", df = df)) == 0){ #NO DATES
    
    df[,DATE:= sort(today()+(1:.N)*days(-1)), by = level] %>% 
      .[,KEY:=str_c(
        str_c(day(DATE), month(DATE), year(DATE)),
        match(get(level), unique(get(level))), 
        sep = "_")]
    
  }else{
    day_name <- classes_vector(data_type = "Date", df = df)
    day_variable <- df[,day_name, with = FALSE]
    
    df[,KEY:=str_c(
      str_c(day(day_variable[,get(day_name)]), 
            month(day_variable[,get(day_name)]), 
            year(day_variable[,get(day_name)])),
      match(get(level),unique(get(level))), sep = "_"
    )]
  }
}
#A <- key_creation(df = copy(df), level = "SPECIES")


# Push Table to SQL Server ------------------------------------------------

#' @description this function stablish a connection with a server, but if I write
#' wrong a parameter or if there is no server connection then return a character
#' string "Incorrect connection"
#' @param DBI parameters 
#' @return connection between R and a server in SQL Server
SQL_connection <- possibly(function(dsn, database, uid, pwd, port){
  DBI::dbConnect(odbc::odbc(),
                 dsn = dsn,
                 Database = database,
                 UID = uid,
                 PWD = pwd,
                 Port = port)
}, otherwise = "Incorrect connection") 

#' @description copy a dataset that we have and then select the variables that
#' we want of the dataset and finally push this dataset to SQL Server
#' @param connection DBI connection
#' @param name names that we want to give to the table in SQL Server
#' @param df dataset to import  
#' @param variables selected variables of the dataset
#' @return push table to SQL Server
push_sql_table <- function(connection, name, df, variables){
  
  df <- copy(df) %>% as.data.table %>% 
    .[,variables, with = FALSE]
  
  DBI::dbWriteTable(conn = connection,  
                    name = str_c("PERSONAL", name, sep = "_"), 
                    value = df,
                    overwrite = TRUE)
}

