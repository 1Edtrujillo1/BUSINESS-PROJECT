#dashboardthemes: 
# https://github.com/nik01010/dashboardthemes
# install_github("nik01010/dashboardthemes") "usethis", "devtools"

# . -----------------------------------------------------------------------

# mixdist package para usar expresiones latex en shiny

# . -----------------------------------------------------------------------

#Shinydashboardplus: create additional options in header 
# left_menu = tagList(
#   dropdownBlock()

# . -----------------------------------------------------------------------

#id, allow us to connect with other inputs (for example conditionalPanel) 

# sidebarMenu(
#   
#   id = "leftbar",...)

# . -----------------------------------------------------------------------

#Create the tabs options in the right bar, the id is with a number always

# rightSidebarTabContent(
#   id = 1,..)

# . -----------------------------------------------------------------------

#library(pryr) to save base plot objects

# . -----------------------------------------------------------------------

# library(chartjs) donout chart for comparing numeric variable based on levels
# http://tutuchan.github.io/chartjs/index.html
#htmlwidgets
#https://github.com/ramnathv/htmlwidgets

# . -----------------------------------------------------------------------

#STANDARD ERROR
#https://www.datanovia.com/en/lessons/ggplot-error-bars/
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/


# str_extract(string = deparse(substitute(df_discrete)), #deparse(substitute()) nonestandar evaluation to bring the name of the dataset as a character vector
#             pattern = "(?<=my_)\\w*(\\w*)(?=\\d\\_)|(?<=my_)\\w*(\\w*)(?=\\_)"))  #all after my_ and before digit_ or _

#"[^\\d]*" everything except a 0 or more digit
#"(?<=my_)\\w*(\\w*)(?=\\d\\_)|(?<=my_)\\w*(\\w*)(?=\\_)" all after my_ and before digit_ or _
# str_extract(string = deparse(substitute(df_discrete)), #deparse(substitute()) nonestandar evaluation to bring the name of the dataset as a character vector
#             pattern = "(?<=my_)\\w*(\\w*)(?=\\d\\_)|(?<=my_)\\w*(\\w*)(?=\\_)")

# help --------------------------------------------------------------------
# k <- function(id, distribution, tale){
#   
#   moduleServer(id, function(input, output, session){
#     
#     conditionalPanel(
#       ns = NS(id),
#       condition = glue("input.cont_args == {distribution()}"),
#       
#       conditionalPanel(
#         ns = NS(id),
#         condition = glue("input.cont_tail == {tale()}"),
#         
#         distribution(),
#         tale()
#       )
#     )
#     
#   })
# }

# inumericInput_reactive <- function(id){
#   
#   moduleServer(id, function(input, output, session){
#     
#     conditionalPanel(
#       ns = NS(id),
#       condition = "input.continous == 'exponential'", #| input.continous == 'chi_square' | input.continous == 'student'",
#       
#       div("\\(n\\)", style = "font-size:1%", color = "transparent"), #color to hide because is not a parameter (trick) 
#       
#       numericInput(inputId = NS(id, "rate_exponential"),
#                    label = withMathJax("Rate \\(\\lambda\\):"),
#                    value = 1,
#                    min = 0,
#                    step = 0.5)
#     )
#     
#   })
# }