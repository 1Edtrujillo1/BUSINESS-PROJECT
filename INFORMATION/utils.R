#' @description 
#' @param
#' @return
list_of_lists <- function(no_sublists, element_sublists){
  
  lists_creator <- map(1:no_sublists,function(i){
    rep(list(), i)})
  
  insert_elements_lists <- map(1:length(lists_creator), function(i){
    lists_creator[[i]] <- element_sublists
  })
  insert_elements_lists
}
#list_of_lists(no_sublists = 2, element_sublists = list(c("a","b","c")))

#' @description 
#' @param
#' @return
`%between%` <- function(x,rng) x >= rng[1] & x <= rng[2]

#' @description 
#' @param
#' @return
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
