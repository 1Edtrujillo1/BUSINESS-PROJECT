pull_dataInput_body <- function(id){
  ns = NS(id)
  
  DT::DTOutput(ns("pulltable"))
}


pull_dataInput_rightsidebar <- function(id){
  
  ns = NS(id)
  
  fluidRow(
    actionButton(inputId = ns("pull"), label = "Pull Dataset", 
                 icon = icon(name = "fas fa-file-download",
                             lib = "font-awesome"))
  )
}


pull_dataOutput <- function(input, output, session){
  
  # Connection --------------------------------------------------------------
  observeEvent(input$pull,{
    connectionSQL()
  })
  connectionSQL <- reactive({
    input$pull
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Check Connection",
        uiOutput(ns("connection_sql")),
        easyClose = FALSE,
        footer = tagList(
          actionButton(inputId = ns("connection_sqll"), label = "Check connection",
                       icon = icon(name = "far fa-check-circle", lib = "font-awesome")),
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
      textInput(inputId = ns("uid"), label = "User Identifier"),
      passwordInput(inputId = ns("pwd"), label = "Password"),
      textInput(inputId = ns("port"), label = "Port"))
    do.call(what = tagList, args = sql_connection)
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
  
  pullSQL <- reactive({
    
  })
  
  #' 1. Se ejecuta el botón para crear conexión.
  #' 2. Se crea conexión
  # 3. Se renderea la nueva ventana modal.
  # 3.1 Se generan elementos web.
  # 3.2 Se evalua elementos en conexión.
  # 3.3 Se crean elementos dinámicos
  # 3.4 Se enlazan reactivamente
  
  # # SQL Information ---------------------------------------------------------
  observeEvent(input$pull_sqll, {
    
    connection_sql_reactive <<- SQL_connection(dsn = isolate(input$dsn), 
                                               database = isolate(input$database),
                                               uid = isolate(input$uid), 
                                               pwd = isolate(input$pwd),
                                               port = isolate(input$port))
    
    sql_tables <<- tables_variables(connection = connection_sql_reactive)    
    
  })
  
  
  # # Inputs Pull Dataset ---------------------------------------------------
  output$pull_sql <- renderUI({
    req(sql_tables)
    message("pull_sql")
    ns <- session$ns
    choice_select <- function(){
      connection <- connection_sql_reactive
      if(is.character(connection)) 0
      else 1:(tables_variables(connection = connection) %>% 
                length())
    }
    inputs_table <- list(
      selectInput(inputId = ns("numbertables"), 
                  label = "No. of tables",
                  choices = choice_select(),
                  multiple = FALSE),
      hr(),
      h4("Select Tables"),
      uiOutput(ns("tablesSQL")),
      hr(),
      h4("Select unique ID"),
      uiOutput(ns("idSQL")),
      hr(),
      h4("Select variables"),
      uiOutput(ns("variablesSQL")),
      hr(),
      selectInput2(inputId = ns("jointype"), label = "Type of Join",
                   choices = c("Inner Join" = "innerjoin",
                               "Left Join" = "leftjoin",
                               "Outer Join" = "outerjoin",
                               "Anti Join" = "antijoin"),
                   multiple = FALSE))
    do.call(what = tagList, args = inputs_table)
  })
  
  # # # Select Tables -------------------------------------------------------
  output$tablesSQL <- renderUI({
    message("tablesSQL")
    
    ns <- session$ns
    
    name_tables <- pmap(list(i = 1:length(sql_tables)), function(i){
      
      if(is.character(sql_tables)) "Incorrect connection"
      
      else if(input$numbertables == i){
        message(names(sql_tables))
        choice_tables <- names(sql_tables)
        message(choice_tables)
        names_choices_tables <- str_extract(string = names(sql_tables), 
                                            pattern ="(?<=^PERSONAL[_]).*") 
        
        names(choice_tables) <- names_choices_tables
        
        pmap(list(j = 1:i), function(j){
          
          message(paste0("table", j))
          selectInput(inputId = ns(paste0("table", j)),
                      label = str_c("TABLE", j, sep = "_"),
                      choices = names(sql_tables))
          
        })
      } else NULL
    })
    do.call(what = tagList, args = name_tables)
  })
  
  # # # Select ID -----------------------------------------------------------
  output$idSQL <- renderUI({
    req(sql_tables)
    message("idSQL")
    ns <- session$ns
    
    name_ids <- pmap(list(i = 1:length(sql_tables)), function(i){
      
      if(is.character(sql_tables)) "Incorrect connection"
      
      else if(input$numbertables == i){
        
        pmap(list(j = 1:i), function(j){
          message(paste0("id", j))
          selectInput(inputId = ns(paste0("id", j)), 
                      label = str_c("ID of table", j, sep = "_"),
                      choices = "changes")
        })
      }else NULL
    })
    do.call(what = tagList, args = name_ids)
  })        
  
  observeEvent(sql_tables,{
    req(sql_tables)
    params <- list(length(sql_tables), 
                   paste0("table", length(sql_tables)), 
                   paste0("id", length(sql_tables)))
    
    pwalk(params, function(table, x, y){
      message(glue("reactive(input${x})"))
      assign(glue("r.{x}"),
             eval(parse(text = glue("reactive(input${x})"))),
             pos = 1)
      
      observeEvent(glue("r.{x}"), {
        message(sql_tables)
        updateSelectInput(session, y,
                          sql_tables[[table]])
      })
    })  
    
    
    # # # # Update choices ID -------------------------------------------------
    
    # observe({
    
    #   walk(list(i = 1:length(sql_tables)), function(i){
    
    #     pwalk(
    #       list(
    #         input[[paste0("table", i)]],
    #         input[[paste0("id", i)]]
    #       ),function(ids_table, ids_key){
    
    #         observeEvent(ids_table,{
    
    #           walk(list(i = 1:length(sql_tables)),function(i){
    
    #             if(ids_table == names(sql_tables)[i])
    
    #               updateSelectInput(session, inputId = ids_key,
    #                                 choices = sql_tables[[i]])
    
    #           })
    #         })
    #       })
    #   })
    
    
    # })
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # # Select Variables ------------------------------------------------------
  
  #tables
  #id
  #vars
  #almacenar dataset
  # Observers for the buttons Pull Dataset ----------------------------------
  
  
  
  ## Check Connections
  observeEvent(input$connection_sqll,{
    if(is.character(connection_sql_reactive)){
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
  
  
  
}