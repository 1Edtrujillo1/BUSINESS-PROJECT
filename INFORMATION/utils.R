
# General Functions -------------------------------------------------------

#' @description create a list which we replicated *no_sublists* times to get
#' a list of lists where each element contains the element *element_sublists*
#' @param no_sublists number of sublists
#' @param element_sublists element to instert in each sublist
#' @return list, which each sublist contains the same element, this is useful
#' to apply with purrr, for example walk
list_of_lists <- function(no_sublists, element_sublists){
  
  lists_creator <- map(1:no_sublists,function(i){
    rep(list(), i)})
  
  insert_elements_lists <- map(1:length(lists_creator), function(i){
    lists_creator[[i]] <- element_sublists
  })
  insert_elements_lists
}
#list_of_lists(no_sublists = 2, element_sublists = list(c("a","b","c")))

#' @description create a close interval
#' @param x vector to analyze
#' @param rng range of the interval
#' @return logical vector of index in a vector that are in an interval
`%between%` <- function(x,rng) x >= rng[1] & x <= rng[2]
# c(4,6,7,9,10,13,15) %between% 6:9

#' @description
#' @param
#' @return
general_sequence <- function(from, to){
  tryCatch({
    length.out <- (to-from) %>% round(digits = 1)
    
    if(length.out == 1){
      numb_ceros <- str_extract(string = min(from, to), 
                                pattern = "(?<=[.])(.*?)(?=(?![0])[:digit:]+)") %>% #all after . and before the digit different from 0
        str_length()
      
      by_argument <- str_c(".", str_pad(string = 1, 
                                        width = numb_ceros, 
                                        side = "left", pad = "0")) %>% as.numeric()
      
      result <- seq(from = from, to = to, by = by_argument)
    }else result <- seq(from = from, to = to)
    result
  }, error = function(e){"need to define the correct parameters"})
}
# general_sequence(from = qbeta(0.99999,
#                               shape1 = scale,
#                               shape2 = shape,
#                               lower.tail = FALSE),
#                  to = qbeta(0.99999,
#                             shape1 = scale,
#                             shape2 = shape,
#                             lower.tail = TRUE))

#' @description to a previous plot we add specific ggplot 2 functions to create
#' a final design to the plot
#' @param plot defined plot
#' @return the previous plot with a specific design
design_plot <- function(plot){
  
  final_plot <- plot + theme_minimal() + 
    theme(
      plot.title = element_text(face = "italic", hjust = 0.5),
      axis.title = element_text(face = "italic"),
      axis.ticks = element_line(color = "red"),
      axis.line = element_line(color = "red"),
      axis.text = element_text(colour = "white"),
      text = element_text(colour = "white")
    )
  
  final_plot <- plotly::ggplotly(final_plot) %>% 
    layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
  final_plot
}

#' @description to a previous datatable we add specific DT arguments to create
#' a final DT dataset
#' @param df defined datatable
#' @param ... add additional arguments in DT format
#' @return the previous table in DT format
desing_DT <- function(df,...){
  datatable(data = df, 
            style = 'bootstrap', #theme of the datatable
            filter = list(position = 'top', clear = FALSE),
            options = list(
              autoWidth = TRUE,
              ...
            ),
            ...) %>% 
    return()
}

# Shiny Modules -----------------------------------------------------------

#' @description we read the dataset with the function *read_data* in a reactive
#' way to use in our shiny project
#' @param id general id of the module working in
#' @param path path of the file
#' @return reactive *read_data* 
reaDataset_reactive <- function(id, path){
  moduleServer(id, function(input, output, session){
    reactiveFileReader(
      intervalMillis = 1000,
      session = session,
      filePath = path,
      readFunc = read_data)
  })
}

#' @description 
#' @param id general id of the module working in
#' @param id_reference clickable input to visualize the box
#' @param title of the box
#' @param uioutput depends on this to bring other UI or only a message inside the
#' box
#' @param uId input of UI, if uioutput = TRUE we create a UI
#' @param actionId input of action button, if uioutput = TRUE we use this parameter 
#' in the footer for an action
#' @param message_string string, if uioutput = FALSE we show a string message
#' @return box to visualize inputs, when we click *id_reference*
#' if uioutput = TRUE we create a UI inside the box
#' if uioutput = FALSE we show a string message
generate_box_info_reactive <- function(id, id_reference, 
                                       title, uId = NULL, actionId = NULL,
                                       uioutput = FALSE, message_string = NULL){
  moduleServer(id, function(input, output, session){
    
    content_f <- function(uioutput, message_string){
      if(uioutput) uiOutput(NS(id, uId))
      else message_string
    }
    
    footer_f <- function(uioutput){
      if(uioutput){
        tagList(
          actionButton(inputId = NS(id, actionId),
                       label = "Generate",
                       icon = icon(name = "fas fa-file-signature",
                                   lib = "font-awesome")),
          modalButton(label = "Close",
                      icon = icon(name = "eject", lib = "glyphicon")))
      }else{
        modalButton(label = "Close",
                    icon = icon(name = "eject", lib = "glyphicon"))
      }
    }
    #We can´t add a renderUI for the defined uiOutput in the same module
    observeEvent(id_reference(),{
      showModal(
        modalDialog(
          title = title,
          content_f(uioutput = uioutput, message_string = message_string),
          easyClose = FALSE,
          footer = footer_f(uioutput = uioutput)
        ))
    })
  })
}

#' @description 
#' @param id general id of the module working in
#' @param df reactive df
#' @param add_elements TRUE if we want to add more inputs, FALSE if we want to 
#' show only dates information
#' @param uId_add_element input of the new UI if add_elements = TRUE we create 
#' a UI
#' @return the renderUI of the input UI from the function
#' generate_box_info_reactive of the parameter uId
date_selection_reactive <- function(id, df, add_elements = FALSE, 
                                    uId_add_element){
  moduleServer(id, function(input, output, session){
    
    renderUI({
      date_variable <- classes_vector(data_type = "Date", df = df)
      
      if(length(date_variable) == 0){
        list(
          "You do not have a date variable",
          hr(),
          radioButtons(inputId = NS(id, "choices_report"),
                       label = "Choice periodicity",
                       choices = list("Historical" = "historical"),
                       selected = NULL)) %>% 
          do.call(what = tagList, args = .)
        
      }else{
        possible_years <- year(df[,get(date_variable)]) %>% unique() %>% sort()
        
        list_dates <- list(
          radioButtons(
            inputId = NS(id, "choices_report"),
            label = "Choice periodicity",
            choices = list(
              "Yearly" = "year",
              "Monthly" = "month",
              "Semester" = "semester",
              "Quarter" = "quarter",
              "Historical" = "historical"),
            selected = NULL),
          hr(),
          conditionalPanel(ns = NS(id),
                           condition = "input.choices_report == 'year'",
                           selectInput(inputId = NS(id, "year_year"),
                                       label = "Select a year",
                                       choices = possible_years)),
          conditionalPanel(ns = NS(id),
                           condition = "input.choices_report == 'month'",
                           selectInput(inputId = NS(id, "month_year"),
                                       label = "Select a year",
                                       choices = possible_years),
                           selectInput(inputId = NS(id, "month_month"),
                                       label = "Select a month",
                                       choices = specific_month)),
          conditionalPanel(ns = NS(id),
                           condition = "input.choices_report == 'semester'",
                           selectInput(inputId = NS(id, "semester_year"),
                                       label = "Select a year",
                                       choices = possible_years),
                           selectInput(inputId = NS(id, "semester_semester"),
                                       label = "Select a semester",
                                       choices = 1:2)),
          conditionalPanel(ns = NS(id),
                           condition = "input.choices_report == 'quarter'",
                           selectInput(inputId = NS(id, "quarter_year"),
                                       label = "Select a year",
                                       choices = possible_years),
                           selectInput(inputId = NS(id, "quarter_quarter"),
                                       label = "Select a quarter",
                                       choices = 1:4))
        ) 
        if(add_elements){
          list_dates %>% 
            append(
              list(
                hr(),
                uiOutput(NS(id, uId_add_element))
              )) %>% do.call(what = tagList, args = .)
          
        }else{list_dates %>% do.call(what = tagList, args = .)}
      }
    })
  })
}

#' @description combinations of the  dataset based from the function
#' *report_creator* of the selection of periodicity 
#' @param id general id of the module working in
#' @param df reactive df
#' @param choices_report input from the function *report_creator*, based on this
#' we define our final_df
#' @param rest of the parameters all the inputs from the function *report_creator*, 
#' of the periodicity.
#' @return reactive dataset of the output from the function *report_creator*  
define_dataset_reactive <- function(id, df, choices_report, year_year,
                                    month_year, month_month,
                                    semester_year, semester_semester,
                                    quarter_year, quarter_quarter){
  
  moduleServer(id, function(input, output, session){
    reactive({ #reactive because of the df
      if(length(choices_report()) == 0) final_df <- NULL
      
      else if(choices_report() == "year"){ #inputs can´t be inside a submodule
        final_df <- report_creator(df = copy(df),
                                   year = year_year(),
                                   summary = FALSE)
      }else if(choices_report() == "month"){
        final_df <- report_creator(df = copy(df),
                                   year = month_year(),
                                   month = month_month(),
                                   summary = FALSE)
      }else if(choices_report() == "semester"){
        final_df <- report_creator(df = copy(df),
                                   year = semester_year(),
                                   semester = semester_semester(),
                                   summary = FALSE)
      }else if(choices_report() == "quarter"){
        final_df <- report_creator(df = copy(df),
                                   year = quarter_year(),
                                   quarter = quarter_quarter(),
                                   summary = FALSE)
      }else if(choices_report() == "historical"){
        final_df <- report_creator(df = copy(df),
                                   summary = FALSE)
      }
      final_df
    })
  })
}

#' @description this is very useful to get the names of the marginal distributions, 
#' to select specific  combination of levels
#' @param id general id of the module working in
#' @param df reactive df
#' @return list of marginals distributions of the dataset 
distribution_reactive <- function(id, df){
  
  moduleServer(id, function(input, output, session){
    
    factor_variables <- classes_vector(data_type = "factor", 
                                       df = df)
    
    num_int_var <- classes_vector(data_type = c("integer", "numeric"),
                                  df = df)
    help_distribution(df = df, 
                      num_int_var = num_int_var,
                      factor_variables = factor_variables,
                      func = NULL,
                      distribution = TRUE) %>% 
      return()
  })
}

#' @description create two types of alerts, one if in the shiny worked fine, and
#' the other if it didn´t work well.
#' @param id general id of the module working in
#' @param text text to show in the alert
#' @param type of alert
#' @return Alert
message_reactive <- function(id, text, type = c("success", "fail")){
  moduleServer(id, function(input, output, session){
    
    if(type == "success"){
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = text,
        type = "success")
      
    }else if(type == "fail"){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = text,
        type = "error")
    }
  })
}