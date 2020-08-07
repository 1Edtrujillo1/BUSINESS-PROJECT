# connection <- SQL_connection(dsn = "business", database = "PROJECT_DATA",
#                              uid = "MEMBER", pwd = "member",
#                              port = 1433)

#' @description this function creates a list of the variables´names of each table 
#' that is in the SQL SERVER that started with PERSONAL_ (are the pushed tables)
#' @param connection DBI connection
#' @return list of sublists which each sublist containts the variables´ names as
#' a vector of each table, and the name of each sublist is the name of each table
tables_variables <- function(connection){
  
  if(is.character(connection)) "Incorrect connection"
  
  else{
    TABLES <- str_subset(string = DBI::dbListTables(conn = connection), 
                         pattern = "^PERSONAL[_].*")
    
    map(TABLES, ~DBI::dbListFields(conn = connection, name =.x)) %>% 
      set_names(TABLES) %>% return()
  }
}

#' @description when we have at least two name tables then we create a list *dfs*
#' that represent list of datasets, this list was formed based on, first selecting 
#' the variables and the id of each table in the *vars_id* list and pass it to the 
#' *information* list that contain the variables, id  and name of the table 
#' of each table that we want, after that we create the list *dfs* that 
#' for each table that we want, we extract each dataset from a SQL Server and
#' changing the name of each id of each dataset to "UNIQUE_ID" to merge the 
#' datasets with the help of the function *iterative_merge* and finally thanks
#' to the function *unify_merge_columns* we unify each duplicated column from the
#' final dataset
#' @param connection DBI connection
#' @param tables selected name tables 
#' @param variables selected variables of each table
#' @param id selected unique id of each table
#' @param join type of join 
#' @return 
#' if we have only one name table then we  extract a dataset that is pulled 
#' from a SQL server
#' if we have more than one name table then we will get a specific merged 
#' dataset based on the join parameter
#' other case we get a string refered as a message of error
join_tables <- function(connection, tables, variables, id, 
                        join = c("innerjoin", "leftjoin", "outerjoin")){
  tryCatch({
    
    if(length(tables) == 1) 
      df <- pull_df(connection = connection, vars = unlist(variables),
                    table = unlist(tables))
    else{
      
      vars_id <- map2(variables, id, ~c(.x, .y))
      
      information <- list(vars_id, tables) 
      
      dfs <- pmap(information, ~pull_df(connection = connection,
                                        vars = .x, table = .y)) %>% 
        set_names(information[[2]])
      
      walk2(dfs, id, ~setnames(x = .x, old = .y, new = "UNIQUE_ID"))
      
      if(join == "innerjoin") 
        df <- iterative_merge(dfs_list = dfs, key = "UNIQUE_ID")
      else if(join == "leftjoin") 
        df <- iterative_merge(dfs_list = dfs, key = "UNIQUE_ID", all.x = TRUE)
      else if(join == "outerjoin") 
        df <- iterative_merge(dfs_list = dfs, key = "UNIQUE_ID", all = TRUE)
      
      df <- unify_merge_columns(df = df, keep.y = TRUE)
      setorder(df, -UNIQUE_ID) #- ascending order
    }
    
    df
    
  },error = function(e)
    message("Select a correct table with its corresponding variables and ID
            or correct ID to merge")
  )}
#1 TABLE
# d <- join_tables(connection = connection, tables = "PERSONAL_TABLE2", 
#
#2 OR MORE TABLES
# d <- join_tables(connection = connection, tables = names(sql_tables)[c(1,2)],
#                  variables = list(c("SIBSP", "NAME", "TICKET"),
#                                   c("SIBSP", "AGE", "PARCH", "FARE")),
#                  id = rep("KEY", 2),
#                  join = "outerjoin")
# d <- join_tables(connection = connection, tables = names(sql_tables),
#                  variables = list(str_subset(string = sql_tables$PERSONAL_TABLE1, pattern = "^(?!KEY)"), 
#                                   str_subset(string = sql_tables$PERSONAL_TABLE2, pattern = "^(?!KEY)"),
#                                   str_subset(string = sql_tables$PERSONAL_TABLE3, pattern = "^(?!KEY)")),
#                  id = rep("KEY", 3),
#                  join = "innerjoin")

#' @SUBFUNCTION 
#' @description this function allow to select the variables that we want from
#' a dataset from a SQL Server, extracting with the help of a query
#' @param connection DBI connection
#' @param vars variables of a table
#' @param table table
#' @return dataset that is pulled from a SQL server
pull_df <- function(connection, vars, table){
  
  query_variables <- str_c("[",vars,"]", collapse = ",")
  
  df <- as.data.table(
    dbGetQuery(conn = connection,
               statement = 
                 glue("SELECT {query_variables} FROM [{table}]")) 
  )
  df[,lapply(.SD, assign_data_type), .SDcols = names(df)] %>% 
    return()
}

#' @SUBFUNCTION 
#' @description merge multiple datasets 
#' @param dfs_list list of datasets
#' @param key unique id of each dataset [each key need to have the same name]
#' @return merged dataset from all the datasets of the list
iterative_merge <- function(dfs_list, key, ...){
  Reduce(function(x,y) merge(x, y, by = key, ...),
         x = dfs_list)
}
#df11 <- iterative_merge(dfs_list = dfs, key = "KEY")

#' @SUBFUNCTION 
#' @description This function is useful when we merge datasets and we have
#' duplicated columns (this is the extension .x and .y).
#' if we detect at least one duplicate column then we define the column as the 
#' new information from the merge (.y) [if keep.y = TRUE]
#' if we detect at least one duplicate column then we define the column as the
#' old information from the merge (.y) [if keep.y = FALSE]
#' (between .x and .y columns).
#' And then delete the duplicated columns (.x and .y) from the same variable. 
#' In other case we return the original merged dataset
#' @param df dataset
#' @param keep.y
#' @return the same dataset without duplicated columns (unified values ofcolumns)
#' where: 
#' if keep.y = TRUE then the variable contain observation of the new information
#' if keep.y = FALSE then the variable contain observation of the old information
unify_merge_columns <- function(df, keep.y = TRUE){
  
  if(any(str_detect(string = names(df), pattern = ".x$|.y$"))){
    
    duplicated <- names(df)[names(df) %like% ".x$|.y$"]
    
    originals <- str_remove_all(string = duplicated,
                                pattern = ".x$|.y$") %>% unique()
    
    walk(originals, function(i){
      
      if(keep.y) df[,(i):= get(paste0(i, ".y"))] 
      else df[,(i):= get(paste0(i, ".x"))] 
      
      df[,c(paste0(i,".x"), paste0(i,".y")):=NULL]
    })
    df <- df
  }else df
}
# df_left <- unify_merge_columns(df = copy(df11), keep.y = TRUE)
# df_right <- unify_merge_columns(df = copy(df11), keep.y = FALSE)





