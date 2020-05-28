# NOTIFICATIONS -----------------------------------------------------------
#' @description Obtain a dataset of notifications for the header shiny
#' @param path path of the json file
#' @return Data.table obtained from a JSON file
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

#' @description This input represent notifications in the header
#' @param id to connect each inputs and the output in the same session 
#' @return input for notification in the header
NotificationmenuInput_header <- function(id){
  ns = NS(id)
  dropdownMenuOutput(outputId = ns("notificationMenu"))
}

#' @description Create an interactive notification ouput
#' @param parameters of a shiny module 
#' @return notifications output
NotificationmenuOutput <- function(input, output, session){
  ns <- session$ns
  output$notificationMenu <- renderMenu({
    # Create a Dynamic notifications for the header
    ntfcation <- apply(notifications_list(path = "INFORMATION/notifications.json"), 1,
                       function(row){
                         notificationItem(text = row[["TEXT"]],
                                          href = row[["HREF"]],
                                          status = row[["STATUS"]],
                                          icon = icon(name = row[["ICON"]]))
                       })
    dropdownMenu(type = "notifications", .list = ntfcation)
  })
}

# MENU --------------------------------------------------------------------
#' @description Create different kind of interactive boxes for the home menu 
#' @param id to connect each inputs and the output in the same session 
#' @return boxes for the menu
HomemenuInput_body <- function(id){
  ns = NS(id)
  fluidRow(
    # Add boxes for the home header options -----------------------------------
    conditionalPanel(
      condition = "input.basemenu == 'Home'",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          boxPlus(
            title = "Home",
            closable = FALSE,
            collapsible = TRUE,
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            enable_dropdown = TRUE,
            dropdown_icon = "info",
            dropdown_menu = dropdownItemList(
              dropdownItem(url = "https://trello.com/jorgeeduardotrujillovelazquez/boards", name = "trello"),
              dropdownDivider(), #create a line space
              dropdownItem(url = "#", name = "item1"),
              dropdownItem(url = "#", name = "tem2"),
            ),
            box(
              width = NULL,
              title = "Pages Information",
              status = NULL,
              socialButton(
                url = "https://trello.com/",
                type = "trello"
              ),
              socialButton(
                url = "http://dropbox.com",
                type = "dropbox"
              ),
              socialButton(
                url = "http://github.com",
                type = "github"
              )
            ))
        ))),
    conditionalPanel(
      condition = "input.basemenu == 'Personal Information'",
      fluidRow(
        column(
          width = 6,
          offset = 3,# to aign to center
          shinydashboardPlus::flipBox(
            id = 3,
            main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
            header_img = "https://image.flaticon.com/icons/svg/119/119598.svg",
            front_title = "Eduardo Trujillo",
            back_title = "About Eduardo",
            hr(), #Line space
            "More than 2 years of experience in Data Science Areas",
            back_content = tagList(
              column(
                width = 6,
                offset = 3,# to aign to center
                "SUPER"))
          )
        ))),
    conditionalPanel(
      condition = "input.basemenu == 'Additional Information'",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          box(
            title = "Additional Information",
            status = "primary"
          )
        )))
  )
}

#' @description this function is needed to visualize the interactive boxes of the menu
#' @param parameters of a shiny module 
#' @return allow the interactive boxes for the menu
HomemenuOutput <- function(input, output, session){
  ns <- session$ns
}




