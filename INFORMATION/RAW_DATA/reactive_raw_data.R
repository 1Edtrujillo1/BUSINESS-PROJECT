#every input defined needs to be insider a ns() when we are doing functions like here and session$ns if input is defined in the server

#'@param inputId
#'@param label 
#'@param value 
raw_dataInput_body <- function(id){
  ns = NS(id)
  DT::DTOutput(ns("origTable")) #dataset to edit
}

#'@param inputId
#'@param label 
#'@param value 
raw_dataInput_rightsidebar <- function(id){
  
  ns = NS(id) #creates namespaced IDs It is intended for use in Shiny modules
  
  fluidRow( #TO ADJUST THE PAGE  this is necessary
    conditionalPanel(condition="true==false",   # TO HIDE THE INPUTS (BEHIND THE PAGE)
                     #this textInput is necesary because the text is converted to a dataset
                     textInput(inputId = ns("result"), label = "Result"),
                     #this check is important because when it is TRUE we can edit the dataset
                     checkboxInput(inputId = ns("resetPage"), label = "ResetPage", value = FALSE),
                     #this input represent the page of where we are going to edit the dataset (This is for proxy)
                     numericInput(inputId = ns("page"), label = "Page", value = 1),
                     #this input represent the number of the renglon of the dataset to edit
                     numericInput(inputId = ns("no"), label = "No", value = 1),
                     #this input represent the width for the box with the editable part without this we can´t visualize multiple variables
                     numericInput(inputId = ns("width2"), label = "width2", value = 170)
    ),
    
    #This is the way how we import a file from our files and this dataset will be taken by the id result
    fileInput(inputId = ns("selectdataset"), label = "Select a file to edit",
              multiple = FALSE, #select only one file
              accept = c(",sas7bdat", ".rds", ".txt", ".csv", ".xls", ".xlsx")
    ),
    
    tagList(#taList ALLOW TO PUT IN THE SAME ROW THE BUTTONS 
      #Button that when we click it is going to show an observeEvent with all the modifications to the dataset
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
  
  #Read the imported dataset
  data <- reactive({
    if(is.null(input$selectdataset)) return(NULL)
    read_data(path = input$selectdataset$datapath) %>% 
      as.data.frame()
  })
  
  # Define Data -------------------------------------------------------------
  #UPDATE the value OF result id INPUT 
  observe({
    #text_dataset is a textInput that can be a loaded dataset for example "iris" 
    updateTextInput(session, inputId = "result", value = text_dataset())
  })
  #CHANGE AN INPUT  TEXT TO A DATASET  OF result id INPUT
  df <- reactive({
    if(is.null(input$result)){
      #if the text_dataset is null then we print the dataset imported
      df <- data()
    } else if(input$result != "") {
      #if text_dataset is a dataset then evaluate the text as a dataset
      df <- eval(parse(text = input$result))
    }
    #in other case print the dataset imported
    else df <- data()
  })
  #DEFINING OUR DATASET
  output$origTable <- DT::renderDT({
    if(is.null(data())){
      validate( #validating that we select a dataset
        need(any(class(try(eval(parse(text=input$result)))) %in% c("tbl_df","tibble","data.frame", "data.table")),
             "Please enter a valid Dataset")
      )}
    updateCheckboxInput(session, inputId = "resetPage", value = TRUE) #poner en TRUE "palomita" el input esto para el PROXY para editar el datatable
    DT:: datatable(
      data = df(), 
      style = 'bootstrap',
      editable = "cell") #If we select with doble click an observation we will be able to edit 
  })
  proxy <- dataTableProxy("origTable") #used to manipulate an existing DataTables instance in a Shiny app, e.g. select rows/columns, or add rows.
  observeEvent(input$resetPage,{
    if(input$resetPage){ #now that is TRUE the input resetPage we can edit de dataset
      proxy %>% selectPage(input$page) #select all the page (dataset) for editting
      updateCheckboxInput(session, inputId = "resetPage", value = FALSE) #now turn off the input resetPage (quitar la palomita del input resetPage) to stop editting
    }
  })
  
  ########################################
  
  # Box with the editable part ----------------------------------------------
  
  #This part is so necessary to function the box to show of editData()
  observeEvent(input$editData,{
    id <- input$origTable_rows_selected #id of DATATABLE indicating the indices of rows on all pages 
    if(length(id) %in% c(0,1)) updateNumericInput(session, inputId = "no", value = id) #if a row id is in that length, this is we are in a row or we are not in any row then update the number of the row
    else if(input$no > nrow(df())) updateNumericInput(session, inputId = "no", value = 1) #if we for example added a row then update the number of the row as 1 like a new value
    EditDATA() #show the box with the editable part 
    updateNumericInput(session, inputId = "page", value = (id-1)%/%10+1) #update the page where we are updating the row, the value showed is autommatical way to update correctly
  })
  EditDATA <- reactive({#is in reactive and not in observeEvent because we are creating an input update that needs ns()
    
    input$editData
    
    ns <- session$ns
    #If we don´t have a data loaded then we show a box with the message Please enter a valid Dataset
    if(is.null(data())){
      showModal(
        modalDialog(
          title = "Edit Data",
          "Please enter a valid Dataset",
          easyClose = TRUE, 
          footer = modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon"))
        ))
      #if the data is showed then we can show de box with the editable part
    }else{
      showModal(
        modalDialog(
          title = "Edit Data",
          uiOutput(ns("edit_data")), #in here is going to appear all the editable part of the dataset
          easyClose = TRUE, #close without necessary click ok,
          footer = tagList( #update and close button
            actionButton(inputId = ns("update"), label = "Update", icon = icon(name = "ok", lib ="glyphicon")),
            modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
        ))
    }
  })
  # Creating of the buttons of the editable part ----------------------------
  output$edit_data <- renderUI({
    
    ns <- session$ns
    
    #If we are analising one renglon this is a value, NO a vector of length 2 or more
    if(length(input$no) == 1){
      #all our dataset
      d <- df()
      #df that filter un renglon of the dataset
      df <- d[input$no,] %>% as.data.frame() #THIS IS THE IMPORTANT PART THAT WHEN WE **UPDATE** no THEN WE ARE GOING TO VIEW THE NEXT RENGLON OF THE DATASET, converted as dataframe because if we have only one variable we want to see it as dataframe not as vector
      names(df) <- names(d) 
      #buttons that are going to be inside the observeEvent
      mylist <- list(
        #buttons before the line space
        actionButton(inputId = ns("home"), label = "", icon = icon(name = "backward", lib = "glyphicon")), #button to return to the first RENGLON
        actionButton(inputId = ns("left"), label = "", icon = icon(name = "chevron-left", lib = "glyphicon")),#button to return to the previous RENGLON
        numericInput2(inputId = ns("rowno"), label = "Row Number", value = input$no, min = 1, max = nrow(d), step = 1, width=50+10*log10(nrow(d))),#button to get the number of the RENGLON
        actionButton(inputId = ns("right"), label = "", icon = icon(name = "chevron-right",lib = "glyphicon")),#button to go on to the next RENGLON
        actionButton(inputId = ns("end"), label = "", icon = icon(name = "forward",lib = "glyphicon")),#button to go on to the last RENGLON
        actionButton(inputId = ns("new"), label = "", icon = icon(name = "plus",lib = "glyphicon")),#button to add a new RENGLON
        actionButton(inputId = ns("remove"), label = "", icon = icon(name = "trash", lib = "glyphicon")),#button to delete an RENGLON
        textInput2(inputId = ns("rowname"), label = "Row Name", value = rownames(d)[input$no], width = 150), #button to bring the name of the RENGLON of the first indicative columns 
        selectInput2(inputId = ns("width"), label = "input width", choices = c(170, 250, 350, 450, 550), selected = input$width2, width = 80),
        
        hr() #line of space
      ) 
      #buttons after the line of space and are going to be the variables of the dataset 
      #df is the filter dataset d[input$no,] that allow that when we **UPDATE** "no" the We can watch next RENGLON
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
  observeEvent(input$home,{  #UPDATING RENGLON to return to the first RENGLON
    updateNumericInput(session, inputId = "no", value = 1)
  })
  observeEvent(input$left,{  #UPDATING RENGLON if we are in the RENGLON 2 or more the we return to the previous RENGLON
    value <- ifelse(input$no > 1, input$no-1, 1)
    updateNumericInput(session, inputId = "no", value = value)
  })
  observeEvent(input$rowno,{
    #nrow(df()) this df is the complete dataset and we are watching the length of the dataset
    #if RENGLON of the filter dataset is NA then we go to the last RENGLON 
    if(is.na(input$rowno)) updateNumericInput(session, inputId = "rowno", value = nrow(df()))
    if(input$rowno > nrow(df())) {
      #if we for example added a new RENGLON then we have a RENGLON and is mayor al length of df then we need to update the button that gets the number of the RENGLON
      #and update the new RENGLON    
      updateNumericInput(session, inputId = "rowno", value = nrow(df()))
      updateNumericInput(session, inputId = "no", value = nrow(df()))
    } else{
      #UPDATING RENGLON to return in which we are
      updateNumericInput(session, inputId = "no", value = input$rowno)
    }
  })
  observeEvent(input$right,{ #UPDATING RENGLON if we are in the previows to the last RENGLON then we go on to the next RENGLON in other case we are watching the last RENGLON
    value <- ifelse(input$no<nrow(df()),input$no+1,nrow(df()))
    updateNumericInput(session, inputId = "no", value = value)
  })
  observeEvent(input$end,{ #UPDATING RENGLON to gon on to the last RENGLON
    updateNumericInput(session, inputId = "no", value = nrow(df()))
  })
  observeEvent(input$width,{
    updateNumericInput(session, inputId = "width2", value = input$width)
  })
  ##The three under is over all the dataset complete not the filter because we want to add a new row or delete a row from the whole dataset
  
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
        easyClose = FALSE, #we do not want to close without necessary click ok,
        footer = tagList(
          actionButton(inputId = ns("AdD"), label = "Add Row", icon = icon(name = "plus",lib = "glyphicon")),
          modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  observeEvent(input$AdD,{
    df <- df()
    #add new row defined in a new dataset df1
    df1 <- tibble::add_row(df)
    #defining the rownames of df1 like the rownames of df (first indicative columns) adding 1 as the add_row new row
    newname <- max(as.numeric(rownames(df)), nrow(df), na.rm = TRUE) + 1
    rownames(df1) <- c(rownames(df), newname)
    added <- c()
    #if is our first raw to select this is is.null(input$reslt) = c() then add the first row
    if(input$result=="added"){
      added_1 <<- df1 #The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent environments for an existing definition of the variable being assigned.
      updateTextInput(session, inputId = "result", value = "added_1")
      #if we already added a raw then add more than one raw of the dataset df
    } else{
      added <<- df1
      updateTextInput(session, inputId = "result", value ="added")
    }
    #update the number of RENGLONES  (after adding one RENGLON)  
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
          actionButton(inputId = ns("DeleTe"), label = "Delete Row", icon = icon(name = "remove", lib = "glyphicon")),
          modalButton(label = "Close", icon = icon(name = "eject", lib ="glyphicon")))
      ))
  })
  observeEvent(input$DeleTe,{
    #delete a RENGLON of the dataset
    df <- df()[-input$no,]
    delete <- c()
    #if is our first raw to select this is is.null(input$reslt) = c() then delete the first row
    if(input$result == "delete"){
      delete_1 <<- df  #The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent environments for an existing definition of the variable being assigned.
      updateTextInput(session, inputId = "result", value = "delete_1")
      #if we already delete a raw then delete more than one raw of the dataset df
    }else{
      delete <<- df 
      updateTextInput(session, inputId = "result", value = "delete")
    }
    #update the number of RENGLONES  (after delete one RENGLON)  
    #put the last RENGLON  of the dataset (if we borrow a new raw)
    if(input$no > nrow(df)) updateNumericInput(session, inputId = "no", value = nrow(df))
  })
  # Update ------------------------------------------------------------------
  observeEvent(input$update,{
    df <- df()
    #for each column we are going to update 
    for(i in 1:ncol(df)){
      try(df[input$no,i] <- input[[colnames(df)[i]]]) #This is the way where we update our dataset 
      #if is date
      if("POSIXct" %in% class(df[input$no,i])){
        df[input$no,i] <- as.POSIXct(x = input[[colnames(df)[i]]],
                                     tz = ifelse(!is.null(attr(df[input$no,i],"tzone")), attr(df[input$no,i],"tzone"), ""),
                                     origin="1970-01-01")
      }
    }
    updated <- c()
    #if is our first raw to update this is is.null(input$reslt) = c() then update the first row
    if(input$result=="updated"){
      updated_1 <<- df #The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent environments for an existing definition of the variable being assigned.
      updateTextInput(session, inputId = "result",value ="updated_1")
      #if we already update a raw then update more than one raw of the dataset df
    } else{
      updated<<- df
      updateTextInput(session, inputId = "result",value = "updated")
    }#here is not necessary update "no" because we are updating the whole dataset
  })
  
}
