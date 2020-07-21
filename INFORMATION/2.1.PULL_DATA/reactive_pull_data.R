pull_dataInput_body <- function(id){
  ns = NS(id)
  DT::DTOutput(ns("pulltable"))
}

#' @description define the necessary parammeters to pull dataset from an SQL 
#' server in the application
#' @param id to connect each inputs and the output in the same session  
#' @return inputs to pull the final datatable in the application
pull_dataInput_rightsidebar <- function(id){
  ns = NS(id)
  fluidRow(
    
    actionButton(inputId = ns("pull"), label = "Pull Dataset",
                 icon = icon(name = "fas fa-file-download",
                             lib = "font-awesome")))
}

#' @description Creation of interactive clickable boxes to pull the final datatable 
#' in the application
#' @param parameters of a shiny module 
#' @return boxes with sql parameters 
pull_dataOutput <- function(input, output, session){
  
  # Connection --------------------------------------------------------------
  observeEvent(input$pull,{
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Check Connection",
        uiOutput(ns("connection_sql")),
        easyClose = FALSE,
        footer = tagList(
          actionButton(inputId = ns("connection_sqll"), label = "Check connection",
                       icon = icon(name = "far fa-check-circle", 
                                   lib = "font-awesome")),
          actionButton(inputId = ns("pullsql"), label = "Pull Dataset",
                       icon = icon(name = "fas fa-file-download",
                                   lib = "font-awesome")),
          modalButton(label = "Close", 
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  output$connection_sql <- renderUI({
    ns <- session$ns
    sql_connection <- list(
      textInput(inputId = ns("dsn"), label = "Data Source Name"),
      textInput(inputId = ns("database"), label = "Database"),
      textInput(inputId = ns("uid"), label = "User Identification"),
      passwordInput(inputId = ns("pwd"), label = "Password"),
      textInput(inputId = ns("port"), label = "Port")
    )
    tags$div(
      class = "container",
      do.call(what = tagList, args = sql_connection))
  })
  
  # Pull Dataset ------------------------------------------------------------
  observeEvent(input$pullsql,{
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Pull Dataset",
        uiOutput(ns("pull_sql")),
        easyClose = FALSE,
        footer = tagList(
          actionButton(inputId = ns("pull_sqll"), label = "Pull Dataset",
                       icon = icon(name = "fas fa-file-download",
                                   lib = "font-awesome")),
          modalButton(label = "Close",
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  
  # SQL Information ---------------------------------------------------------
  connection_sql_reactive <- reactive({
    SQL_connection(dsn = input$dsn, 
                   database = input$database,
                   uid = input$uid, 
                   pwd = input$pwd,
                   port = input$port)
  }) 
  
  sql_tables <- reactive({
    
    message(class(connection_sql_reactive()))
    
    tables_variables(connection = connection_sql_reactive())
  })
  
  # # Inputs Pull Dataset ---------------------------------------------------
  output$pull_sql <- renderUI({
    
    ns <- session$ns
    message("sql_tables")
    req(sql_tables())
    
    number_tables <- function(){
      if(is.character(connection_sql_reactive())) 0 
      else 1:(sql_tables() %>% length())
    }
    
    inputs_table <- list(
      h4("No. of tables"),
      selectInput2(inputId = ns("numbertables"),
                   label = NULL,
                   choices = number_tables(),
                   multiple = FALSE),
      hr(),
      h4("Select Tables"),
      uiOutput(ns("tablesSQL")),
      hr(),
      h4("Select unique ID"),
      uiOutput(ns("idSQL")),
      hr(),
      h4("Select Variables"),
      uiOutput(ns("variablesSQL")),
      hr(),
      h4("Type of Join"),
      uiOutput(ns("joinSQL")))
    do.call(what = tagList, args = inputs_table)
  })
  
  numberTables <- reactive(input$numbertables) 
  
  # # # InputsID´s of pull_sql ----------------------------------------------
  inputIdS_table <- list(
    TABLE = "table",
    ID = "id",
    VARIABLES = "variable",
    JOIN = "jointype"
  )
  
  # # # Select Tables -------------------------------------------------------
  output$tablesSQL <- renderUI({
    
    ns <- session$ns
    message("tablesSQL")
    
    if(is.character(sql_tables())) "Incorrect Connection"
    
    else{
      names_sql_table <- names(sql_tables())
      names(names_sql_table) <- str_extract(string = names(sql_tables()),
                                            pattern = "(?<=^PERSONAL[_]).*") #everything afther PERSONAL_
      
      name_tables <- map(1:numberTables(), function(i){
        
        message(glue("{inputIdS_table$TABLE}{i}"))
        print(names_sql_table)
        
        selectInput2(inputId = ns(glue("{inputIdS_table$TABLE}{i}")),
                     label = paste("TABLE", i),
                     choices = names_sql_table,
                     multiple = FALSE,
                     width = 150)
      })
      do.call(what = tagList, args = name_tables)
    }
  })
  
  # # # Select ID -----------------------------------------------------------
  output$idSQL <- renderUI({
    
    ns <- session$ns
    message("idSQL")
    
    if(is.character(sql_tables())) "Incorrect Connection"
    
    else if(input$numbertables == 1) "Select more than one table."
    
    else if(input$numbertables > 1){
      select_tables <- glue("{inputIdS_table$TABLE}{1:numberTables()}")  #table1, table2, table3,...
      
      name_ids <- imap(select_tables, function(x, index){
        
        ids <- sql_tables()[input[[x]]]  #sql_tables[input[[table2]]] = sql_tables["PERSONAL_TABLE2"] = c(...)
        
        message(glue("{inputIdS_table$ID}{index}"))
        print(ids)
        
        selectInput2(inputId = ns(glue("{inputIdS_table$ID}{index}")),
                     label = glue("ID OF TABLE {index}"),
                     choices = ids,
                     multiple = FALSE,
                     width = 150) 
        
      })
      do.call(what = tagList, args = name_ids)
    }else NULL
  })
  
  # # # Select Variables ----------------------------------------------------
  output$variablesSQL <- renderUI({
    
    ns <- session$ns
    message("variablesSQL")
    
    if(is.character(sql_tables())) "Incorrect Connection"
    
    else{
      select_tables <- glue("inputIdS_table$TABLE{1:numberTables()}")
      
      if(input$numbertables == 1){
        
        name_variables <- imap(select_tables, function(x, index){
          
          inputId_TABLE <- glue("input${inputIdS_table$TABLE}{index}")
          TABLE_name <- eval(parse(text = inputId_TABLE))
          variables <- sql_tables()[[TABLE_name]]
          
          message(glue("{inputIdS_table$VARIABLES}{index}"))
          print(variables)
          
          selectInput(inputId = ns(glue("{inputIdS_table$VARIABLES}{index}")),
                      label = glue("VARIABLES OF TABLE {index}"),
                      choices = variables,
                      multiple = TRUE)
        })
      }else{
        name_variables <- imap(select_tables, function(x, index){
          
          inputId_IDS <- glue("input${inputIdS_table$ID}{index}")
          variable_IDS <- eval(parse(text = inputId_IDS))
          
          inputId_TABLE <- glue("input${inputIdS_table$TABLE}{index}")
          TABLE_name <- eval(parse(text = inputId_TABLE))
          variables <- sql_tables()[[TABLE_name]]
          
          variables_withoutID <- variables[!variables%in%variable_IDS]
          
          message(glue("{inputIdS_table$VARIABLES}{index}"))
          print(variables_withoutID)
          
          selectInput(inputId = ns(glue("{inputIdS_table$VARIABLES}{index}")),
                      label = glue("VARIABLES OF TABLE {index}"),
                      choices = variables_withoutID,
                      multiple = TRUE)})
      }
      do.call(what = tagList, args = name_variables)
    }
  })
  
  # # # Select Join ---------------------------------------------------------
  output$joinSQL <- renderUI({
    
    ns <- session$ns
    
    if(is.character(sql_tables())) "Incorrect Connection"
    
    else if(input$numbertables == 1) "Select more than one table."
    
    else if(input$numbertables > 1){
      selectInput2(inputId = ns(glue("{inputIdS_table$JOIN}")), label = NULL,
                   choices = c("Inner Join" = "innerjoin",
                               "Left Join" = "leftjoin",
                               "Outer Join" = "outerjoin"),
                   multiple = FALSE,
                   width = 115)
    }else NULL
  })
  
  # Observers for the buttons Pull Dataset ----------------------------------
  
  # # Check connection ------------------------------------------------------
  observeEvent(input$connection_sqll,{
    if(is.character(connection_sql_reactive())){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Incorrect Connection",
        type = "error")
    }else{
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success")
    }
  })
  
  # # Pull Dataset ----------------------------------------------------------
  FINAL_DF <- reactive({
    
    select_tables <- glue("inputIdS_table$TABLE{1:numberTables()}")
    
    tables_information <- imap(select_tables, function(x, index){
      
      Id_tables <- glue("input${inputIdS_table$TABLE}{index}")
      TABLES <- eval(parse(text = Id_tables))
      
      Id_id <- glue("input${inputIdS_table$ID}{index}")
      ID <- eval(parse(text = Id_id))
      
      Id_variables <- glue("input${inputIdS_table$VARIABLES}{index}")
      VARS <- eval(parse(text = Id_variables))
      
      list(
        TABLE = TABLES,
        ID = ID,
        VARIABLES = VARS
      ) %>% return()
    })  
    
    Id_join <- glue("input${inputIdS_table$JOIN}")
    JOIN <- eval(parse(text = Id_join))
    
    FINAL_DF <- join_tables(connection = connection_sql_reactive(),
                            tables = map(tables_information, "TABLE"),
                            variables = map(tables_information, "VARIABLES"),
                            id = map(tables_information, "ID"),
                            join = JOIN)
    
    message("tables_information")
    message(class(tables_information))
    print(tables_information)
    
    message("FINAL_DF")
    message(class(FINAL_DF))
    print(FINAL_DF)
    FINAL_DF
  })
  
  observeEvent(input$pull_sqll,{
    
    req(FINAL_DF())
    
    if(is.null(FINAL_DF())|is.character(FINAL_DF()) | (FINAL_DF()[,.N] == 0)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Pull Dataset not completed",
        type = "error")
    }else{
      saveRDS(object = FINAL_DF(), file = "INFORMATION/FILES/saved.rds")
      
      print(pmap(list(map(names(FINAL_DF()), ~FINAL_DF()[,get(.x)]) %>% 
                        set_names(names(FINAL_DF()))),~class(.x)))
      
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "Pull Dataset completed",
        type = "success")
    }
  })
  
  
}