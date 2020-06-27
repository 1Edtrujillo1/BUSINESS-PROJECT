#' @description generate a datatable to manipulate in the application
#' @param id to connect each inputs and the output in the same session 
#' @return datatable to visualize
raw_dataInput_body <- function(id){
  ns = NS(id)
  DT::DTOutput(ns("origTable")) 
}

#' @description define the necessary parammeters to manipulate the datatable in 
#' the application
#' @param id to connect each inputs and the output in the same session  
#' @return inputs to manipulate the datatable in the application
raw_dataInput_rightsidebar <- function(id){
  ns = NS(id) 
  fluidRow( 
    conditionalPanel(condition="true==false",  
                     textInput(inputId = ns("result"), label = "Result"),
                     checkboxInput(inputId = ns("resetPage"), label = "ResetPage", 
                                   value = FALSE),
                     numericInput(inputId = ns("page"), label = "Page", 
                                  value = 1),
                     numericInput(inputId = ns("no"), label = "No", 
                                  value = 1),
                     numericInput(inputId = ns("width2"), label = "width2", 
                                  value = 170)
    ),
    fileInput(inputId = ns("selectdataset"), label = "Select a file to edit",
              multiple = FALSE,
              accept = c(".sas7bdat", ".rds", ".txt", ".csv", ".xls", ".xlsx")
    ),
    tagList(
      actionButton(inputId = ns("editData"), label = "Edit Data", 
                   icon = icon(name = "wrench", lib = "glyphicon")),
      actionButton(inputId = ns("outlierId"), label = "Outliers", 
                   icon = icon(name = "erase", lib = "glyphicon")),
      hr(),
      actionButton(inputId = ns("CreateId"), label = "Adding ID", 
                   icon = icon(name = "tag", lib = "glyphicon")),
      actionButton(inputId = ns("push"), label = "Push File", 
                   icon = icon(name = "file-upload", lib = "font-awesome"))
    )
  )
}

#' @description Creation of interactive clickable boxes to manipulate the datatable in the application
#' @param parameters of a shiny module 
#' @return boxes with editable parts of the datatable
raw_dataOutput <- function(input, output, session, text_dataset = reactive("")) {
  data <- reactive({
    if(is.null(input$selectdataset)) return(NULL)
    read_data(path = input$selectdataset$datapath) %>% 
      as.data.frame()
  })
  # Define Data -------------------------------------------------------------
  observe({
    updateTextInput(session, inputId = "result", value = text_dataset())
  })
  df <- reactive({
    if(is.null(input$result)){
      df <- data()
    } else if(input$result != "") {
      df <- eval(parse(text = input$result))
    }
    else df <- data()
  })
  output$origTable <- DT::renderDT({
    if(is.null(data())){
      validate( 
        need(any(class(try(eval(parse(text=input$result)))) %in% 
                   c("tbl_df","tibble","data.frame", "data.table")),
             "Please enter a valid Dataset")
      )}
    updateCheckboxInput(session, inputId = "resetPage", value = TRUE) 
    DT:: datatable(
      data = df(), 
      style = 'bootstrap',
      filter = list(position = 'top', clear = FALSE),
      options = list(
        autoWidth = TRUE),
      editable = "cell") 
  })
  proxy <- dataTableProxy("origTable") 
  observeEvent(input$resetPage,{
    if(input$resetPage){ 
      proxy %>% selectPage(input$page) 
      updateCheckboxInput(session, inputId = "resetPage", value = FALSE) 
    }
  })
  # Box with the editable part ----------------------------------------------
  observeEvent(input$editData,{
    id <- input$origTable_rows_selected 
    if(length(id) %in% c(0,1)) updateNumericInput(session, inputId = "no", 
                                                  value = id) 
    else if(input$no > nrow(df())) updateNumericInput(session, inputId = "no", 
                                                      value = 1) 
    EditDATA() 
    updateNumericInput(session, inputId = "page", value = (id-1)%/%10+1) 
  })
  EditDATA <- reactive({
    input$editData
    ns <- session$ns
    if(is.null(data())){
      showModal(
        modalDialog(
          title = "Edit Data",
          "Please enter a valid Dataset",
          easyClose = TRUE, 
          footer = modalButton(label = "Close", 
                               icon = icon(name = "eject", lib ="glyphicon"))
        ))
    }else{
      showModal(
        modalDialog(
          title = "Edit Data",
          uiOutput(ns("edit_data")), 
          easyClose = TRUE, 
          footer = tagList( 
            actionButton(inputId = ns("delete_cols"), label = "Delete Columns", 
                         icon = icon(name = "trash", lib = "glyphicon")),
            actionButton(inputId = ns("update"), label = "Update", 
                         icon = icon(name = "ok", lib ="glyphicon")),
            modalButton(label = "Close", 
                        icon = icon(name = "eject", lib ="glyphicon")))
        ))
    }
  })
  # Creation of the buttons of the editable part ----------------------------
  output$edit_data <- renderUI({
    ns <- session$ns
    if(length(input$no) == 1){
      d <- df()
      df <- d[input$no,] %>% as.data.frame() 
      names(df) <- names(d) 
      mylist <- list(
        actionButton(inputId = ns("home"), label = "", 
                     icon = icon(name = "backward", lib = "glyphicon")), 
        actionButton(inputId = ns("left"), label = "", 
                     icon = icon(name = "chevron-left", lib = "glyphicon")),
        numericInput2(inputId = ns("rowno"), label = "Row Number", 
                      value = input$no, min = 1, max = nrow(d), step = 1, 
                      width=50+10*log10(nrow(d))),
        actionButton(inputId = ns("right"), label = "", 
                     icon = icon(name = "chevron-right",lib = "glyphicon")),
        actionButton(inputId = ns("end"), label = "", 
                     icon = icon(name = "forward",lib = "glyphicon")),
        actionButton(inputId = ns("new"), label = "", 
                     icon = icon(name = "plus",lib = "glyphicon")),
        actionButton(inputId = ns("remove"), label = "", 
                     icon = icon(name = "trash", lib = "glyphicon")),
        selectInput2(inputId = ns("width"), label = "input width", 
                     choices = c(170, 250, 350, 450, 550), 
                     selected = input$width2, 
                     width = 80),
        hr() 
      ) 
      myclass <- lapply(d,class)
      for(i in 1:ncol(df)){
        myname <- colnames(df)[i]
        
        if(is.na(df[1,i])) myvalue <- NA
        else myvalue <- df[1,i]
        
        if("factor" %in% myclass[[i]]) 
          mylist[[i+10]] = selectInput2(ns(myname), myname, choices = levels(df[[i]]), 
                                        selected = myvalue, width = input$width2)
        else if("Date" %in% myclass[[i]]) 
          mylist[[i+10]] = dateInput2(ns(myname), myname, value = myvalue, 
                                      width = input$width2)
        else if("logical" %in% myclass[[i]]) 
          mylist[[i+10]] = checkboxInput2(ns(myname), myname, value = myvalue, 
                                          width = input$width2)
        else { # c("numeric","integer","charater")
          mywidth <- (((max(nchar(d[[i]]),20,na.rm=TRUE)*8) %/% input$width2)+1)*input$width2
          if(mywidth <= 500) mylist[[i+10]] = textInput2(ns(myname), myname, 
                                                         value = myvalue, 
                                                         width = mywidth)
          else mylist[[i+10]] = textAreaInput(ns(myname), myname, 
                                              value = myvalue, width = "500px")
        }
      }
      do.call(what = tagList, args = mylist)
    }
    else h4("You can edit data after select one row in datatable.")
  })
  # # Observers of the buttons ----------------------------------------------
  observeEvent(input$home,{  
    updateNumericInput(session, inputId = "no", value = 1)
  })
  observeEvent(input$left,{  
    value <- ifelse(input$no > 1, input$no-1, 1)
    updateNumericInput(session, inputId = "no", value = value)
  })
  observeEvent(input$rowno,{
    if(is.na(req(input$rowno))) updateNumericInput(session, inputId = "rowno", 
                                                   value = nrow(df()))
    if(req(input$rowno) > nrow(df())) { 
      updateNumericInput(session, inputId = "rowno", value = nrow(df()))
      updateNumericInput(session, inputId = "no", value = nrow(df()))
    } else{
      updateNumericInput(session, inputId = "no", 
                         value = ifelse(input$rowno < 0, 1, input$rowno))
    }
  })
  observeEvent(input$right,{ 
    value <- ifelse(input$no<nrow(df()),input$no+1,nrow(df()))
    updateNumericInput(session, inputId = "no", value = value)
  })
  observeEvent(input$end,{
    updateNumericInput(session, inputId = "no", value = nrow(df()))
  })
  observeEvent(input$width,{
    updateNumericInput(session, inputId = "width2", value = input$width)
  })
  ##The three under is over all the dataset complete not the filter because we want to add a new row or delete a row or update from the whole dataset
  # Add Row -----------------------------------------------------------------
  observeEvent(input$new,{
    ADD()
  })
  ADD <- reactive({
    input$new
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Are you sure to add a new Row?",
        easyClose = FALSE, 
        footer = tagList(
          actionButton(inputId = ns("AdD"), label = "Add Row", 
                       icon = icon(name = "plus",lib = "glyphicon")),
          modalButton(label = "Close", 
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  observeEvent(input$AdD,{
    df <- df()
    df1 <- tibble::add_row(df)
    newname <- max(as.numeric(rownames(df)), nrow(df), na.rm = TRUE) + 1
    rownames(df1) <- c(rownames(df), newname)
    added <- c()
    if(input$result=="added"){
      added_1 <<- df1 
      updateTextInput(session, inputId = "result", value = "added_1")
    } else{
      added <<- df1
      updateTextInput(session, inputId = "result", value ="added")
    } 
    updateNumericInput(session, inputId = "no", value = newname)
  })
  # Remove Row --------------------------------------------------------------
  observeEvent(input$remove,{
    DELETE()
  })
  DELETE <- reactive({
    input$remove
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Are you sure to delete the Row?",
        easyClose = FALSE,
        footer = tagList(
          actionButton(inputId = ns("DeleTe"), label = "Delete Row", 
                       icon = icon(name = "trash", lib = "glyphicon")),
          modalButton(label = "Close", 
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  observeEvent(input$DeleTe,{
    df <- df()[-input$no,]
    delete <- c()
    if(input$result == "delete"){
      delete_1 <<- df  
      updateTextInput(session, inputId = "result", value = "delete_1")
    }else{
      delete <<- df 
      updateTextInput(session, inputId = "result", value = "delete")
    }
    if(input$no > nrow(df)) updateNumericInput(session, inputId = "no", 
                                               value = nrow(df))
  })
  # Update ------------------------------------------------------------------
  observeEvent(input$update,{
    df <- df()
    for(i in 1:ncol(df)){
      try(df[input$no,i] <- input[[colnames(df)[i]]]) 
      #if is date
      if("POSIXct" %in% class(df[input$no,i])){
        df[input$no,i] <- 
          as.POSIXct(x = input[[colnames(df)[i]]],
                     tz = ifelse(!is.null(attr(df[input$no,i],"tzone")), 
                                 attr(df[input$no,i],"tzone"), ""),
                     origin="1970-01-01")
      }
    }
    updated <- c()
    if(input$result=="updated"){
      updated_1 <<- copy(df) %>% as.data.table() %>% 
        .[,lapply(.SD, assign_data_type), 
          .SDcols = names(df)] %>% as.data.frame()
      updateTextInput(session, inputId = "result",value ="updated_1")
    } else{
      updated <<- copy(df) %>% as.data.table() %>% 
        .[,lapply(.SD, assign_data_type), 
          .SDcols = names(df)] %>% as.data.frame()
      updateTextInput(session, inputId = "result",value = "updated")
    }
  })
  ##
  # Delete Columns ----------------------------------------------------------
  observeEvent(input$delete_cols,{
    id_col <- input$origTable_columns_selected
    DeleteCOLS()
    updateNumericInput(session, inputId = "page", value = (id_col-1)%/%10+1) 
  })
  DeleteCOLS <- reactive({
    input$delete_cols
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Delete Columns",
        uiOutput(ns("delete_columns")),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = ns("delete_colss"), label = "Delete",
                       icon = icon(name = "trash", lib = "glyphicon")),
          modalButton(label = "Close", 
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  output$delete_columns <- renderUI({
    ns <- session$ns
    checkboxGroupInput(inputId = ns("var_name"), label = NULL,
                       choices = colnames(df()), selected = NULL)
  })
  # # Observer for the button delete columns --------------------------------
  observeEvent(input$delete_colss,{
    df <- df() %>% as.data.table()
    delete_colss_func <- function(var = NULL, df){
      if(is.null(var)) df # if we don´t selec any variable
      else df[,(var):=NULL]
    }
    delete_col <- c()
    if(input$result == "delete_col"){
      delete_col1 <<- delete_colss_func(var = input$var_name, 
                                        df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "delete_col1")
    }else{
      delete_col <<- delete_colss_func(var = input$var_name, 
                                       df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "delete_col")
    }
  })
  ##
  # Outliers --------------------------------------------------------------
  observeEvent(input$outlierId,{
    DeleteOUTLIERS()
  })
  DeleteOUTLIERS <- reactive({
    input$outlierId
    ns <- session$ns
    if(is.null(data())){
      showModal(
        modalDialog(
          title = "Delete Outliers",
          "Please enter a valid dataset",
          easyClose = TRUE,
          footer = modalButton(label = "Close", 
                               icon = icon(name = "eject", lib ="glyphicon"))
        ))
    }else{
      showModal(
        modalDialog(
          title = "Delete Outliers",
          uiOutput(ns("delete_outliers")),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = ns("helpoutliers"), label = "Help",
                         icon = icon(name = "question", lib = "font-awesome")),
            actionButton(inputId = ns("delete_outlierss"), label = "Delete",
                         icon = icon(name = "erase", lib = "glyphicon")),
            modalButton(label = "Close", 
                        icon = icon(name = "eject", lib ="glyphicon")))
        ))
    }
  })
  output$delete_outliers <- renderUI({
    ns <- session$ns
    checkboxGroupInput(inputId = ns("vars_factor_outlier"), 
                       label = "Factor Variables:",
                       choices = classes_vector(data_type = "factor", 
                                                df = copy(df()) %>% as.data.table()),
                       selected = NULL)
  })
  # # Help Outliers -----------------------------------------------------------
  observeEvent(input$helpoutliers,{
    HELPOUTLIERS()
  })
  HELPOUTLIERS <- reactive({
    input$helpoutliers
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Best Factor Variable",
        uiOutput(ns("help_outliers_intro")),
        easyClose = TRUE,
        footer = modalButton(label = "Close", 
                             icon = icon(name = "eject", lib ="glyphicon"))
      ))
  })
  output$help_outliers_intro <- renderUI({
    ns <- session$ns
    variables_numbers_intro <- 
      list(selectInput(inputId = ns("int_num_fact"),
                       label = "Integer or Numeric Variable",
                       choices = classes_vector(data_type = c("integer", "numeric"), 
                                                df = copy(df()) %>% as.data.table()),
                       multiple = FALSE),
           hr(),
           uiOutput(ns("help_outliers")))
    do.call(what = tagList, args = variables_numbers_intro)
  })
  # # # Add another UI to use the int_num_fact ID
  output$help_outliers <- renderUI({
    ns <- session$ns
    recomended_variables <- recomended_level_outliers(df = copy(df()) %>% 
                                                        as.data.table())
    
    variables_numbers <- pmap(list(i = 1:length(recomended_variables)), function(i){
      
      recomended_variable <- pluck(recomended_variables, i)
      
      pmap(list(j = 1:length(recomended_variable)), function(j){
        
        if(length(names(recomended_variables)[[i]]) < 1) "You do not have any factor variable available"
        
        else if(input$int_num_fact == names(recomended_variables)[[i]]){
          numericInput2(inputId = ns(names(recomended_variable)[[j]]), 
                        label = names(recomended_variable)[[j]], 
                        value = recomended_variable[[j]])
        }else NULL
      })
    })
    do.call(what = tagList, args = variables_numbers)
  })
  # # Observer for the button Outliers --------------------------------------
  observeEvent(input$delete_outlierss,{
    df <- df() %>% as.data.table()
    delete_outlierss_func <- function(var = NULL, df){
      if(is.null(var)) df
      else delete_outliers(df = df, level = var)
    }
    outliers_col <- c()
    if(input$result == "outliers_col"){
      outliers_col1 <<- delete_outlierss_func(var = input$vars_factor_outlier, 
                                              df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "outliers_col1")
    }else{
      outliers_col <<- delete_outlierss_func(var = input$vars_factor_outlier, 
                                             df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "outliers_col")
    }
  })
  ##
  # Adding ID ---------------------------------------------------------------
  observeEvent(input$CreateId,{
    AddID()
  })
  AddID <- reactive({
    input$CreateId
    ns <- session$ns
    if(is.null(data())){
      showModal(
        modalDialog(
          title = "Adding unique ID variable",
          "Please enter a valid dataset",
          easyClose = TRUE,
          footer = modalButton(label = "Close",  
                               icon = icon(name = "eject", lib ="glyphicon"))
        ))
    }else{
      showModal(
        modalDialog(
          title = "Adding unique ID variable",
          uiOutput(ns("add_id")),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = ns("add_idd"), label = "Add ID",
                         icon = icon(name = "tag", lib = "glyphicon")),
            modalButton(label = "Close", 
                        icon = icon(name = "eject", lib ="glyphicon")))
        ))
    }
  })
  output$add_id <- renderUI({
    ns <- session$ns
    id_inputs <- list(
      selectInput(inputId = ns("optimal_vars_factor"), 
                  label = "Optimal Factor Variable(s):",
                  choices = optimal_factor_variables(df = copy(df()) %>% as.data.table())),
      hr(),
      selectInput(inputId = ns("vars_factor_id"), 
                  label = "Select a Factor Variable:",
                  choices = classes_vector(data_type = "factor",
                                           df = copy(df()) %>% as.data.table()),
                  selected = NULL, multiple = FALSE))
    do.call(what = tagList, args = id_inputs)
  })
  # # Observer for the button ID --------------------------------------------
  observeEvent(input$add_idd,{
    df <- df() %>% as.data.table()
    add_idd_func <- function(var = NULL, df){
      if(is.null(var)) df
      else key_creation(df = df, level = var)
    }
    idd_col <- c()
    if(input$result == "idd_col"){
      idd_col1 <<-  add_idd_func(var = input$vars_factor_id, 
                                 df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "idd_col1")
    }else{
      idd_col <<-  add_idd_func(var = input$vars_factor_id, 
                                df = df) %>% as.data.frame()
      updateTextInput(session, inputId = "result", value = "idd_col")
    }
  })
  ##
  # Push Dataset ------------------------------------------------------------
  # # Connections -----------------------------------------------------------
  observeEvent(input$push,{
    connectionSQL()
  })
  connectionSQL <- reactive({
    input$push
    ns <- session$ns
    if(is.null(data())){
      showModal(
        modalDialog(
          title = "Push Dataset",
          "Please enter a valid dataset",
          easyClose = TRUE,
          footer = modalButton(label = "Close", 
                               icon = icon(name = "eject", lib ="glyphicon"))
        ))
    }else{
      showModal(
        modalDialog(
          title = "Check Connection",
          uiOutput(ns("connection_sql")),
          easyClose = FALSE,
          footer = tagList(
            actionButton(inputId = ns("connection_sqll"), label = "Check connection", 
                         icon = icon(name = "far fa-check-circle", lib = "font-awesome")),
            actionButton(inputId = ns("pushsql"), label = "Define Dataset",
                         icon = icon(name = "file-upload", lib = "font-awesome")),
            modalButton(label = "Close", 
                        icon = icon(name = "eject", lib ="glyphicon")))
        ))
    }
  })
  output$connection_sql <- renderUI({
    ns <- session$ns
    sql_connections <- list(
      textInput(inputId = ns("dsn"), label = "Data Source Name"),
      textInput(inputId = ns("database"), label = "Database"),
      textInput(inputId = ns("uid"), label = "User Identifier"),
      passwordInput(inputId = ns("pwd"), label = "Password"),
      textInput(inputId = ns("port"), label = "Port"))
    do.call(what = tagList, args = sql_connections)
  })
  # # Define Dataset ----------------------------------------------------------
  observeEvent(input$pushsql,{
    pushSQL()
  })
  pushSQL <- reactive({
    input$pushsql
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Push Dataset",
        uiOutput(ns("push_sql")),
        easyClose = FALSE,
        footer = tagList(
          actionButton(inputId = ns("push_sqll"), label = "Push Dataset",
                       icon = icon(name = "file-upload", lib = "font-awesome")),
          modalButton(label = "Close",
                      icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  output$push_sql <- renderUI({
    ns <- session$ns
    sql_inputs <- list(
      textInput(inputId = ns("namedataset"), 
                label = "Name of the Exported Dataset"),
      hr(),
      checkboxGroupInput(inputId = ns("vars_sql"),
                         label = "Select Variables (Including the ID):",
                         choices = names(df()), selected = NULL))
    do.call(what = tagList, args = sql_inputs)
  })
  # # Observer for the buttons SQL ------------------------------------------
  connection_sql_reactive <- reactive({
    SQL_connection(dsn = input$dsn, database = input$database,
                   uid = input$uid, pwd = input$pwd,
                   port = input$port)
  })
  ### Check Connections
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
  ### Push Table
  observeEvent(input$push_sqll,{
    df <- copy(df()) %>% as.data.table()
    
    data_sqll_func <- function(connection, name, df, var = NULL){
      if(is.null(var) | is.character(connection)) df
      else push_sql_table(connection = connection, 
                          name = name, df = df, 
                          variables = var)
    }
    push_df <- c()
    if(input$result == "push_df"){
      push_df1 <<- data_sqll_func(connection = connection_sql_reactive(), 
                                  name = input$namedataset, df = df, 
                                  var = input$vars_sql) 
    }else{
      push_df <<- data_sqll_func(connection = connection_sql_reactive(), 
                                 name = input$namedataset, df = df, 
                                 var = input$vars_sql)
    }
  })
  
}
