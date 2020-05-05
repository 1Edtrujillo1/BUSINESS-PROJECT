header <- dashboardHeaderPlus(
  
  
  
  # Add a title -------------------------------------------------------------
  
  title = span(tagList(icon("building"), "Eduardo Company")),
  
  # Add a Dynamic Notifications ---------------------------------------------
  
  dropdownMenuOutput(outputId = "notificationMenu"),
  
  # Enable the right bar with an icon ---------------------------------------
  
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "align-justify",
  
  # add the home menu  ------------------------------------------------------
  
  left_menu = tagList(
    dropdownBlock(
      id = "dropdownplus",
      icon = "home",
      radioButtons(
        inputId = "basemenu",
        label = "Menu",
        choices = c("Home", "Personal Information", "Additional Information"))
    ))
)

