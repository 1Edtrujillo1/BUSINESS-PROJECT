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

#FINAL DATASET
#1.IMPORTAR-
#2.ELIMINAR CARACTERES ESPECIALES-
#3.VARIABLES EN MAYUSCULAS-
#4.OBSERVACIONES EN MAYUSCULAS-
#5.DATA TYPE (FACTOR, CHARACTER, NUMERIC, INTEGER)-
#6. key (date - factor) - 
#7.NUMERIC AND INTEGER -> NA TO Z-SCORE +-3-
#8. NA, "", NULL
#' @description 
#' @param path 
#' @return
read_data <- function(path, encoding = 'UTF-8', sheet = 1, col_select = NULL, ...){
  
  
  options(encoding = encoding, warn = -1) #hide warnings
  
  if(str_detect(string = path, pattern = ".sas7bdat$"))
    df <- haven::read_sas(data_file = path, encoding = encoding, ...)
  else if(str_detect(string = path, pattern = ".rds$"))
    df <- readRDS(file = path, ...)
  else if(str_detect(string = path, pattern = ".txt$"))
    df <- read.table(file = path, header = TRUE, encoding = encoding, na.strings = "NA", ...)
  else if(str_detect(string = path, pattern = ".csv$"))
    df <- read.csv(file = path, header = TRUE, encoding = encoding, na.strings = "NA", ...)
  else if(str_detect(string = path, pattern = "\\.xls$|\\.xlsx$"))
    df <- readxl::read_excel(path = path, sheet = sheet, col_names = TRUE, na = "", ...)
  else NULL
  
  df <- df %>% as.data.table() %>% 
    .[,(unique(names(df))), with = FALSE]
  
  df <- df[,names(df):=lapply(.SD, str_replace_all, special_chrs)] %>% 
    setnames(toupper(names(df))) %>% 
    .[,lapply(.SD, toupper), .SDcols = names(df)] %>% 
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]
  
  character_na <- c('\\A\\z'='N/A') #change "" strings to N/A string
  character_variables <- classes_vector(data_type = c("character", "factor"), df = df)
  
  df <- df[,(character_variables):=map(.SD, str_replace_all, character_na), .SDcols = character_variables] %>% 
    .[,(character_variables):=map(.SD, str_trim), .SDcols = character_variables] %>% 
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]
  
  if(!is.null(col_select)){
    df <- df[,(unique(col_select)), with = FALSE]
  }
  
  return(df)
  
}
#a <- "C:/Users/actje/Dropbox/PROGRAM_FILES/1. PROJECT/STATISTICS/INFORMATION/FILES/my_data.csv"
#df <- read_data(path = a)

#' @description 
#' @param variable
#' @return
assign_data_type <- function(variable){
  
  values_type <- map(variable,function(variable){
    if(is.na(variable)) NA
    else if(str_detect(string = variable, pattern = "[A-Z]+")) "character"
    else if(str_detect(string = variable, pattern = "[0-9]+[.][0-9]+")) "numeric"
    else "integer"}
  ) %>% flatten_chr() 
  
  type <- table(values_type) %>% sort(decreasing = TRUE)   #sort based on type most repeated
  
  if(any(duplicated(type))) type <- "character"
  else  type <- type %>% head(1) %>% names() #datatype most repeated for all its observations
  
  if(type %in% c("character", "integer")){
    if(length(table(variable)) == sum(table(variable) >= 3, na.rm = TRUE)) type <- "factor" #*each observation* is repeated at least 3 times is a factor
    else type}
  
  if(type == "character") variable <- as.character(variable)
  else if(type == "numeric") variable <- as.numeric(variable)
  else if(type == "integer") variable <- as.integer(variable)
  else if(type == "factor") variable <- as.factor(variable)
  
  variable
}

# for(i in 1:length(df)){
#   print(paste0(class(assign_data_type(df[[i]])), names(df)[[i]], sep = "," ))
# }
#assign_data_type(df$SPECIES) %>% class()

#' @description 
#' @param data_type 
#' @param df
#' @return
classes_vector <- function(data_type, df){
  
  list_types <- map(names(df), ~df[,class(get(.x))]) %>% 
    map(~str_subset(string = .x, pattern = data_type)) %>% 
    set_names(names(df))
  
  list_types[map(list_types,length)>0] %>% 
    names() %>% return()
  
}

#classes_vector(data_type = "numeric", df = df)

#' @description 
#' @param df 
#' @param level=factor_variable
#' @return
key_creation <- function(df, level){
  df[,DATE:= sort((today()+ 1:.N * days(-1))), 
     by = level] %>% 
    .[,KEY:=str_c(year(DATE), month(DATE), day(DATE), 
                  get(level), sep = "_")]
  
}
#A <- key_creation(df = copy(df), level = "SPECIES")

#' @description 
#' @param df 
#' @param level=factor_variable
#' @return
delete_outliers <- function(df, level){
  
  choose_level <- str_subset(string = classes_vector(data_type = "factor", df = df),
                             pattern = level)
  
  df <- df[,.SD, by = choose_level]
  
  params <- list(variable = classes_vector(data_type = c("numeric", "integer"), df = df),
                 no_sd = str_c("no_sd", classes_vector(data_type = c("numeric", "integer"), df = df), sep = "_")
  )
  pwalk(params, function(variable, no_sd){
    df[,(no_sd):= (get(variable)-mean(get(variable)))/sd(get(variable)), by = choose_level]
  })
  
  numb_sd <- str_subset(string = classes_vector(data_type = c("numeric", "integer"), df = df), 
                        pattern = "^no_sd") 
  variable <- str_subset(string = classes_vector(data_type = c("numeric", "integer"), df = df), 
                         pattern = "^(?!^no_sd).*")
  
  df2 <- df
  for(i in 1:length(variable)){
    numb_sd_var <- numb_sd[i] 
    var <- variable[i]
    df2[,(var):=ifelse(get(numb_sd_var) > -3 & get(numb_sd_var) < 3, get(var), NA), by = choose_level]
  }
  
  df2[,(numb_sd):=NULL] %>% 
    return()
}
#A <- delete_outliers(df = copy(A), level = "SPECIES")


