
library(purrr)

map(c("shiny", "shinydashboard", "dashboardthemes", "shinydashboardPlus", "shinyWidgets", "mixdist",
      "jsonlite", "data.table", "dplyr"), 
    require, character.only = TRUE)

map(c("header.R", "sidebar.R", "body.R", "rightsidebar.R",
      "INFORMATION/utils.R"), 
    source)

# Define the UI -----------------------------------------------------------

ui <- dashboardPagePlus(header = header,  sidebar = sidebar, 
                        body = body, rightsidebar = rightsidebar)

# Define server -----------------------------------------------------------

server <- function(input, output) {
  
  # Create a Dynamic notifications for the header
  
  output$notificationMenu <- renderMenu({
    
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


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
