# Loading Required Information --------------------------------------------
library(purrr)

map(c("shiny", "shinydashboard", "dashboardthemes", "shinydashboardPlus", "shinyWidgets", "mixdist","DT",
      "jsonlite", "data.table", "dplyr", "bit64", "stringr", "scales", "ggplot2", "plotly",
      "haven", "readxl", "lubridate"), 
    require, character.only = TRUE)

source("INFORMATION/1.MENU/reactive_menu.R")

map(
  str_c("INFORMATION/2.RAW_DATA", 
        c("raw_data.R", "reactive_raw_data.R", "additional_raw_data.R"), sep = "/"),
  source)

map(
  str_c("INFORMATION/4.STATISTICAL_DISTRIBUTIONS",
        c("statistical_distributions.R", "reactive_discretedistr.R",
          "reactive_statisical_distributions.R"), sep = "/"),
  source)

map(c("header.R", "body.R", "sidebar.R", "rightsidebar.R"), 
    source)

# Define the UI -----------------------------------------------------------
ui <- dashboardPagePlus(header = header,  sidebar = sidebar, 
                        body = body, rightsidebar = rightsidebar)

# Define server -----------------------------------------------------------
server <- function(input, output) {
  
  options(shiny.maxRequestSize=100*1024^2) #accept files of MB bigger
  
  # Sever of the header -----------------------------------------------------
  notifications <- callModule(module = NotificationmenuOutput, id = "notifications")
  menu <- callModule(module = HomemenuOutput, id = "menu")
  # Server of the Raw Data --------------------------------------------------
  raw_data <- callModule(module = raw_dataOutput, id = "raw_data")
  
  # Statistical Distributions -----------------------------------------------
  discrete_distributions <- callModule(module = discrete_distOutput, id = "discrete_distributions")
  
 
  
}


# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
