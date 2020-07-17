header <- dashboardHeaderPlus(
  
  # Add a title -------------------------------------------------------------
  title = span(tagList(icon("building"), "Demo")),
  
  # Add a Dynamic Notifications ---------------------------------------------
  NotificationmenuInput_header("notifications"),
  
  # Enable the right bar with an icon ---------------------------------------
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "align-justify",
  
  # Add the home menu in the HomemenuInput_body function of the reac --------
  left_menu = tagList(
    dropdownBlock(
      id = "dropdownplus",
      icon = "home",
      radioButtons(
        inputId = "basemenu",
        label = "Menu",
        choices = c("Home", "Personal Information", "Additional Information"),
        selected = "Home")
    ))
)

