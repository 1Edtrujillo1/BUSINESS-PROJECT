#'@param inputId
#'@param label 
#'@param value 
raw_dataInput_body <- function(id){
  ns = NS(id)
  DT::DTOutput(ns("origTable")) 
}

#'@param inputId
#'@param label 
#'@param value 
raw_dataInput_rightsidebar <- function(id){
  ns = NS(id) 
  fluidRow( 
    conditionalPanel(condition="true==false",  
                     textInput(inputId = ns("result"), label = "Result"),
                     checkboxInput(inputId = ns("resetPage"), label = "ResetPage", value = FALSE),
                     numericInput(inputId = ns("page"), label = "Page", value = 1),
                     numericInput(inputId = ns("no"), label = "No", value = 1),
                     numericInput(inputId = ns("width2"), label = "width2", value = 170)
    ),
    fileInput(inputId = ns("selectdataset"), label = "Select a file to edit",
              multiple = FALSE,
              accept = c(",sas7bdat", ".rds", ".txt", ".csv", ".xls", ".xlsx")
    ),
    tagList(
      actionButton(inputId = ns("editData"), label = "Edit Data", icon = icon(name = "wrench", lib = "glyphicon")),
      actionButton(inputId = ns("outlierId"), label = "Outliers", icon = icon(name = "erase", lib = "glyphicon")),
      hr(),
      actionButton(inputId = ns("CreateId"), label = "Adding ID", icon = icon(name = "tag", lib = "glyphicon")),
      actionButton(inputId = ns("push"), label = "Push File", icon = icon(name = "file-upload", lib = "font-awesome"))
    )
  )
}

#'@param inputId
#'@param label 
#'@param value 
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
        need(any(class(try(eval(parse(text=input$result)))) %in% c("tbl_df","tibble","data.frame", "data.table")),
             "Please enter a valid Dataset")
      )}
    updateCheckboxInput(session, inputId = "resetPage", value = TRUE) 
    DT:: datatable(
      data = df(), 
      style = 'bootstrap',
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
    if(length(id) %in% c(0,1)) updateNumericInput(session, inputId = "no", value = id) 
    else if(input$no > nrow(df())) updateNumericInput(session, inputId = "no", value = 1) 
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
          footer = modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon"))
        ))
    }else{
      showModal(
        modalDialog(
          title = "Edit Data",
          uiOutput(ns("edit_data")), 
          easyClose = TRUE, 
          footer = tagList( 
            actionButton(inputId = ns("update"), label = "Update", icon = icon(name = "ok", lib ="glyphicon")),
            modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
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
        actionButton(inputId = ns("home"), label = "", icon = icon(name = "backward", lib = "glyphicon")), 
        actionButton(inputId = ns("left"), label = "", icon = icon(name = "chevron-left", lib = "glyphicon")),
        numericInput2(inputId = ns("rowno"), label = "Row Number", value = input$no, min = 1, max = nrow(d), step = 1, width=50+10*log10(nrow(d))),
        actionButton(inputId = ns("right"), label = "", icon = icon(name = "chevron-right",lib = "glyphicon")),
        actionButton(inputId = ns("end"), label = "", icon = icon(name = "forward",lib = "glyphicon")),
        actionButton(inputId = ns("new"), label = "", icon = icon(name = "plus",lib = "glyphicon")),
        actionButton(inputId = ns("remove"), label = "", icon = icon(name = "trash", lib = "glyphicon")),
        textInput2(inputId = ns("rowname"), label = "Row Name", value = rownames(d)[input$no], width = 150), 
        selectInput2(inputId = ns("width"), label = "input width", choices = c(170, 250, 350, 450, 550), selected = input$width2, width = 80),
        hr() 
      ) 
      myclass <- lapply(d,class)
      for(i in 1:ncol(df)){
        myname <- colnames(df)[i]
        
        if(is.na(df[1,i])) myvalue <- NA
        else myvalue <- df[1,i]
        
        if("factor" %in% myclass[[i]]) 
          mylist[[i+10]] = selectInput2(ns(myname), myname, choices = levels(df[[i]]), selected = myvalue, width = input$width2)
        else if("Date" %in% myclass[[i]]) 
          mylist[[i+10]] = dateInput2(ns(myname), myname, value = myvalue, width = input$width2)
        else if("logical" %in% myclass[[i]]) 
          mylist[[i+10]] = checkboxInput2(ns(myname), myname, value = myvalue, width = input$width2)
        else { # c("numeric","integer","charater")
          mywidth <- (((max(nchar(d[[i]]),20,na.rm=TRUE)*8) %/% input$width2)+1)*input$width2
          if(mywidth <= 500) mylist[[i+10]] = textInput2(ns(myname), myname, value = myvalue, width = mywidth)
          else mylist[[i+10]] = textAreaInput(ns(myname), myname, value = myvalue, width = "500px")
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
    if(is.na(req(input$rowno))) updateNumericInput(session, inputId = "rowno", value = nrow(df()))
    if(req(input$rowno) > nrow(df())) { 
      updateNumericInput(session, inputId = "rowno", value = nrow(df()))
      updateNumericInput(session, inputId = "no", value = nrow(df()))
    } else{
      updateNumericInput(session, inputId = "no", value = ifelse(input$rowno < 0, 1, input$rowno))
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
          actionButton(inputId = ns("AdD"), label = "Add Row", icon = icon(name = "plus",lib = "glyphicon")),
          modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
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
          actionButton(inputId = ns("DeleTe"), label = "Delete Row", icon = icon(name = "trash", lib = "glyphicon")),
          modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
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
    if(input$no > nrow(df)) updateNumericInput(session, inputId = "no", value = nrow(df))
  })
  # Update ------------------------------------------------------------------
  observeEvent(input$update,{
    df <- df()
    for(i in 1:ncol(df)){
      try(df[input$no,i] <- input[[colnames(df)[i]]]) 
      #if is date
      if("POSIXct" %in% class(df[input$no,i])){
        df[input$no,i] <- as.POSIXct(x = input[[colnames(df)[i]]],
                                     tz = ifelse(!is.null(attr(df[input$no,i],"tzone")), attr(df[input$no,i],"tzone"), ""),
                                     origin="1970-01-01")
      }
    }
    updated <- c()
    if(input$result=="updated"){
      updated_1 <<- df 
      updateTextInput(session, inputId = "result",value ="updated_1")
    } else{
      updated<<- df
      updateTextInput(session, inputId = "result",value = "updated")
    }
  })
  
}
