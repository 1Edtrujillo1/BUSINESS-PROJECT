# connection <- SQL_connection(dsn = "business", database = "PROJECT_DATA",
#                              uid = "MEMBER", pwd = "member",
#                              port = 1433)

#' @description this function creates a list of the variables´names of each table 
#' that is in the SQL SERVER that started with PERSONAL. (are the pushed tables)
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




