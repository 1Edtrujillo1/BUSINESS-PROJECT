
#'@description Obtain a dataset of notifications for the header shiny
#'@param path path of the json file
#'@return Data.table obtained from a JSON file

notifications_list <- function(path){
  
  json_file <- jsonlite::fromJSON(path)
  
  df <- data.table(
    
    text = json_file[["Text"]],
    
    status = json_file[["Status"]],
    
    href = json_file[["Href"]],
    
    icon = json_file[["Icon"]]) %>%
    
    .[,lapply(.SD, as.character)] %>% 
    .[,"text":= toupper(get("text"))]
    
  df <- df %>% setnames(toupper(names(df)))

}




