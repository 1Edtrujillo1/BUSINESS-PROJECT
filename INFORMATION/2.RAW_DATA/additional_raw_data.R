#' @description Create a side-by-side textInput control for entry of unstructured text values
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param width The width of the input in pixel
#' @param bg The color of text
#' @param ... arguments to be passed to textInput
#' @return input adecuated for the box within the editable parts 
textInput2 <- function (inputId, label, value = "",width=100,bg=NULL,...)
{
  style=paste0("width: ",width,"px;")
  if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
  div(style="display:inline-block;",
      if(label!="") tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", class="form-control",value = value,
                 style=style,...))
}

#' @description Create a side-by-side selectInput
#' @param ... arguments to be passed to selectInput
#' @param width The width of the input in pixel
#' @return input adecuated for the box within the editable parts 
selectInput2 <- function(...,width=100){
  mywidth=paste(width,"px",sep="")
  div(style="display:inline-block;",selectInput(...,width=mywidth))
}

#' @description Create a side-by-side numericInput
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#' @param width The width of the input in pixel
#' @param ... arguments to be passed to numericInput
#' @return input adecuated for the box within the editable parts 
numericInput2 <- function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...)
{
  div(style="display:inline-block;",
      tags$label(label, `for` = inputId,class="control-label"),
      tags$input(id = inputId, type = "number", class="form-control",
                 value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
  )
}

#' @description Create a side-by-side dateInput
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param width The width of the input in pixel
#' @param ... arguments to be passed to dateInput
#' @return input adecuated for the box within the editable parts 
dateInput2 <- function(inputId,label,width=100,...){
  div(style="display:inline-block;",
      dateInput(inputId,label,width=paste0(width,"px"),...)
  )
}

#' @description Create a side-by-side checkboxInput
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param width The width of the input in pixel
#' @return input adecuated for the box within the editable parts 
checkboxInput2 <- function(inputId,label,value=FALSE,width=100){
  if(value)
    div(style="display:inline-block;",
        
        tags$input(id = inputId, type = "checkbox",checked = "checked"),
        tags$label(label, `for` = inputId,
                   style=paste("width: ",width-15,"px;",sep=""))
    )
  else
    div(style="display:inline-block;",
        tags$input(id = inputId, type = "checkbox"),
        tags$label(label, `for` = inputId, style=paste("width: ",width-15,"px;",sep=""))
    )
}

