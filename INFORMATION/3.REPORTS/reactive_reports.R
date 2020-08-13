
path <<- "INFORMATION/FILES/saved.rds"

#' @description generate a datatables of the automatic report in the application
#' @param id to connect each inputs and the output in the same session 
#' @return datatables to visualize
reportInput_body <- function(id){
  
  ns = NS(id)
  tagList(
    DT::DTOutput(ns("show_report")),
    
    fluidRow(
      column(
        width = 12,
        offset = 3,
        gradientBox(
          title = "Sparklines",
          width = 6,
          icon = "fas fa-chart-area",
          gradientColor = "primary",
          boxToolSize = "xs",
          collapsible = TRUE,
          footer = column(
            width = 6,
            offset = 3,
            DT::DTOutput(ns("show_sparklines")))
        )))
  )
}

#' @description define one parameter to define the periodicity of the report in
#' the application
#' @param id to connect each inputs and the output in the same session  
#' @return inputs to select the options of periodicity in the application
reportInput_rightsidebar <- function(id){
  
  ns = NS(id)
  
  fluidRow(
    actionButton(inputId = ns("generateReport"), 
                 label = "Generate Report",
                 icon = icon(name = "fas fa-file-signature",
                             lib = "font-awesome"))
  )
}

#' @description Creation of interactive clickable box to create a report
#' in the application
#' @param parameters of a shiny module 
#' @return box with the buttons of periodicity 
reportOutput <- function(input, output, session){
  
    if(file.exists(path)){
      
      # Read pulled Dataset -----------------------------------------------------
      df <- reactiveFileReader(
        intervalMillis = 1000,
        session = session,
        filePath = path,
        readFunc = read_data)
      
      # Generate Report ---------------------------------------------------------
      observeEvent(input$generateReport,{
        
        ns <- session$ns
        
        showModal(
          modalDialog(
            title = "Generate the Report",
            uiOutput(ns("generate_report")),
            easyClose = FALSE,
            footer = tagList(
              actionButton(inputId = ns("generate_reportt"), label = "Generate",
                           icon = icon(name = "fas fa-file-signature", 
                                       lib = "font-awesome")),
              modalButton(label = "Close",
                          icon = icon(name = "eject", lib = "glyphicon")))
          ))
      })
      output$generate_report <- renderUI({
        ns <- session$ns
        
        date_variable <- classes_vector(data_type = "Date", df = df())
        
        if(length(date_variable) == 0){
          list(
            "You do not have a date variable",
            hr(),
            radioButtons(inputId = ns("choices_report"), 
                         label = "Choice periodicity",
                         choices = list("Historical" = "historical"),
                         selected = NULL)) %>% 
            do.call(what = tagList, args = .)
          
        }else{
          possible_years <- year(df()[,get(date_variable)]) %>% unique() %>% sort()
          
          list(
            radioButtons(inputId = ns("choices_report"), 
                         label = "Choice periodicity",
                         choices = list(
                           "Yearly" = "year",
                           "Monthly" = "month",
                           "Semester" = "semester",
                           "Quarter" = "quarter",
                           "Historical" = "historical"),
                         selected = NULL),
            hr(),
            conditionalPanel(ns = ns,
                             condition = "input.choices_report == 'year'",
                             selectInput(inputId = ns("year_year"), 
                                         label = "Select a year",
                                         choices = possible_years)),
            conditionalPanel(ns = ns,
                             condition = "input.choices_report == 'month'",
                             selectInput(inputId = ns("month_year"), 
                                         label = "Select a year",
                                         choices = possible_years),
                             selectInput(inputId = ns("month_month"), 
                                         label = "Select a month",
                                         choices = specific_month)),
            conditionalPanel(ns = ns,
                             condition = "input.choices_report == 'semester'",
                             selectInput(inputId = ns("semester_year"), 
                                         label = "Select a year",
                                         choices = possible_years),
                             selectInput(inputId = ns("semester_semester"), 
                                         label = "Select a semester",
                                         choices = 1:2)),
            conditionalPanel(ns = ns,
                             condition = "input.choices_report == 'quarter'",
                             selectInput(inputId = ns("quarter_year"), 
                                         label = "Select a year",
                                         choices = possible_years),
                             selectInput(inputId = ns("quarter_quarter"), 
                                         label = "Select a quarter",
                                         choices = 1:4))) %>% 
            do.call(what = tagList, args = .)
        }
      })
      
      # Define the design dataset -----------------------------------------------
      design_df <- reactive({
        
        df <- df() 
        
        if(input$choices_report == "year"){
          map(c(FALSE, TRUE), 
              ~ final_design_df(df = copy(df), year = input$year_year,
                                sparkline = .x)) %>% 
            set_names(c("Not_Sparkline", "Sparkline"))
          
        }else if(input$choices_report == "month"){
          map(c(FALSE, TRUE),
              ~ final_design_df(df = copy(df), year = input$month_year, 
                                month = input$month_month, 
                                sparkline = .x)) %>% 
            set_names(c("Not_Sparkline", "Sparkline"))
          
        }else if(input$choices_report == "semester"){
          map(c(FALSE, TRUE),
              ~ final_design_df(df = copy(df), year = input$semester_year,
                                semester = input$semester_semester, 
                                sparkline = .x)) %>% 
            set_names(c("Not_Sparkline", "Sparkline"))
          
        }else if(input$choices_report == "quarter"){
          map(c(FALSE, TRUE),
              ~ final_design_df(df = copy(df), year = input$quarter_year,
                                quarter = input$quarter_quarter, 
                                sparkline = .x)) %>% 
            set_names(c("Not_Sparkline", "Sparkline"))
          
        }else if(input$choices_report == "historical"){
          map(c(FALSE, TRUE),
              ~ final_design_df(df = copy(df), sparkline = .x)) %>% 
            set_names(c("Not_Sparkline", "Sparkline"))
        }
      })
      
      # Observer for generate df ------------------------------------------------
      observeEvent(input$generate_reportt,{
        
        req(design_df())
        
        df <- df() 
        design_df <- design_df()
        
        factor_variables <- classes_vector(data_type = "factor", 
                                           df = df)
        numeric_int_variables <- classes_vector(data_type = c("integer", "numeric"), 
                                                df = df) 
        #date variable not necessary to verify because it could be the historical data
        
        if(length(factor_variables) == 0 | length(numeric_int_variables) == 0 |
           (length(factor_variables) == 0 & length(numeric_int_variables) == 0)){
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "You need to have at least one factor variable and numeric or Integer variables",
            type = "error")
        }else if(is.character(design_df[["Not_Sparkline"]]) | is.character(design_df[["Sparkline"]]) |
                 (is.character(design_df[["Not_Sparkline"]]) & is.character(design_df[["Sparkline"]]))){
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "You do not have any information in that period",
            type = "error")
        }else{
          output$show_report <- DT::renderDT({design_df[["Not_Sparkline"]]})
          output$show_sparklines <- DT::renderDT({design_df[["Sparkline"]]})
          sendSweetAlert(
            session = session,
            title = "Success !!",
            text = "Corrected pull report",
            type = "success"
          )
        }
      })
    }else{
      observeEvent(input$generateReport,{
        
        ns <- session$ns
        
        showModal(
          modalDialog(
            title = "Generate the Report",
            "Please pull a Dataset from the SQL server",
            easyClose = TRUE,
            footer = modalButton(label = "Close",
                                 icon = icon(name = "eject", lib = "glyphicon"))
          ))
      })
    }
  
}
