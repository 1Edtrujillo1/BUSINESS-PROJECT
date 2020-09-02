# df <- read_data(path = "INFORMATION/FILES/saved.rds")
# df <- report_creator(df = df, summary = FALSE)
# 
# factor_variables <- c("SEX", "PCLASS")
# num_int_var <- "PASSENGERID"
# df <- df[,c(factor_variables, num_int_var),with = FALSE]

# UNIVARIABLE ANALYSIS ----------------------------------------------------

# GENERAL FUNCTION --------------------------------------------------------
# level_string <- names(distribution)[[3]]

#' @description 
#' @param
#' @return
general_Descript_Stats <- function(df, 
                                   level = c("all", 
                                             level_string),
                                   table = c("frequency",
                                             "central",
                                             "dispersion",
                                             "coef_var",
                                             "standard_error",
                                             "skeweness",
                                             "kurtosis",
                                             "conditional frequency", 
                                             "conditional expectation"),
                                   plot = c("table",
                                            "acumulative_1", 
                                            "acumulative_2", 
                                            "histogram",
                                            "comparing_means",
                                            "standard_error",
                                            "level_general_boxplot",
                                            "level_bins_boxplot"),
                                   description = FALSE){
  
  factor_variables <- classes_vector(data_type = "factor", 
                                     df = df)
  num_int_var <- classes_vector(data_type = c("integer", "numeric"),
                                df = df)
  if(length(num_int_var) != 1) 
    result <- 
    "You need ONE numeric or integer variable to make an UNIVARIABLE ANALYSIS"
  else{
    distribution <- help_distribution(df = df, 
                                      num_int_var = num_int_var,
                                      factor_variables = factor_variables,
                                      func = NULL,
                                      distribution = TRUE)
    if(level == "all") df <- df
    else if(level == level_string) df <- distribution[[level_string]]
    
    frequency <- map(c("table", "acumulative_1", 
                       "acumulative_2", "histogram"),
                     ~descript_frequency(df = df, 
                                         num_int_var = num_int_var,
                                         plot = .x)) %>% 
      set_names(c("table", "acumulative_1", 
                  "acumulative_2", "histogram"))
    
    calculations <- map(list(~central_tendency(df = .x, 
                                               num_int_var = num_int_var,
                                               plot = "table"),
                             ~dispersion_measures(df = .x,
                                                  num_int_var = num_int_var)),
                        ~help_distribution(df = df, 
                                           num_int_var = num_int_var,
                                           factor_variables = factor_variables,
                                           func = .x,
                                           distribution = FALSE)) %>% 
      set_names("CENTRAL", "DISPERSION")
    
    coefficients <- map(c(TRUE, FALSE), function(description){
      map(c("coef_var", "standard_error","skeweness", "kurtosis"), 
          ~statistical_coefficients(df = df, 
                                    num_int_var = num_int_var, 
                                    unit = .x,
                                    description = description)) %>% 
        set_names(c("coef_var", "standard_error","skeweness", "kurtosis"))
    }) %>% set_names("Description", "Not Description")
    
    if(table == "frequency"){
      if(level == "all") 
        result <- "Is better to make the analysis of Frequencies in a specific combination of levels"
      else if(level == level_string){
        if(plot == "table") result <- frequency[["table"]]
        else if(plot == "acumulative_1") result <- frequency[["acumulative_1"]]
        else if(plot == "acumulative_2") result <- frequency[["acumulative_2"]]
        else if(plot == "histogram") result <- frequency[["histogram"]]
        else result <- NULL
      }else result <- NULL
      
    }else if(table == "central"){
      df_central_tendency <- calculations[["CENTRAL"]]
      if(level == "all"){
        names_factors_df_central_tendency <- 
          df_central_tendency[,factor_variables, with = FALSE] %>% 
          split(x = ., f = seq(.[,.N])) %>% 
          map_chr(~str_c(.x %>% unlist(), collapse = ",")) %>% unname()
        if(plot == "table") result <- df_central_tendency
        else if(plot == "comparing_means"){
          if(df_central_tendency[,.N] <= 6){ #to show correctly with chartjs
            result <- chartjs(height = "500px", palette = "Paired") %>% 
              cjsOptions(animation = list(animateScale = TRUE, 
                                          animateRotate = TRUE)) %>% 
              cjsPolar(labels = names_factors_df_central_tendency) %>% 
              cjsSeries(data = round(x = df_central_tendency[,MEAN], digits = 2)) 
          }else result <- "You need to have 6 or less possible combination of levels" 
        }else if(plot == "standard_error"){
          plot_se <- copy(df_central_tendency)[, X:=1:.N]
          SE <- coefficients[["Not Description"]][["standard_error"]]
          plot_se <- plot_se %>% ggplot(aes(x = X, y = MEAN, fill = as.factor(X))) + 
            geom_col(alpha = 0.6, color = "black") +
            geom_errorbar(aes(ymin = MEAN-SE, ymax = MEAN+SE)) + 
            labs(title = paste("Comparing means from each level based on", 
                               num_int_var),
                 x = "LEVELS") + 
            scale_x_discrete(limits = names_factors_df_central_tendency) + 
            theme(legend.position = "none")
          result <- design_plot(plot = plot_se)
        }else result <- NULL
      }else if(level == level_string){
        plots_central_levels <- map(c("general_boxplot","bins_boxplot"),
                                    ~central_tendency(df = df, 
                                                      num_int_var = num_int_var, 
                                                      plot = .x)) %>% 
          set_names("general_boxplot","bins_boxplot")
        if(plot == "table") result <- df_central_tendency
        else if(plot == "level_general_boxplot")
          result <- plots_central_levels[["general_boxplot"]]
        else if(plot == "level_bins_boxplot") 
          result <- plots_central_levels[["bins_boxplot"]]
        else result <- NULL
      } else result <- NULL
      
    }else if(table == "dispersion"){
      df_dispersion <- calculations[["DISPERSION"]]
      result <- df_dispersion
      
    }else if(table == "coef_var"){
      if(description) result <- coefficients[["Description"]][["coef_var"]]
      else result <- coefficients[["Not Description"]][["coef_var"]]
      
    }else if(table == "standard_error"){
      if(description) result <- coefficients[["Description"]][["standard_error"]]
      else result <- coefficients[["Not Description"]][["standard_error"]]
      
    }else if(table == "skeweness"){
      if(description) result <- coefficients[["Description"]][["skeweness"]]
      else result <- coefficients[["Not Description"]][["skeweness"]]
      
    }else if(table == "kurtosis"){
      if(description) result <- coefficients[["Description"]][["kurtosis"]]
      else result <- coefficients[["Not Description"]][["kurtosis"]]
      
    }else if(table %in% c("conditional frequency", "conditional expectation")){
      if(level == "all"){
        conditional_choice <- map(c("conditional frequency", 
                                    "conditional expectation"), 
                                  ~conditional_stats(df = df, 
                                                     num_int_var = num_int_var, 
                                                     distribution = distribution,
                                                     table = .x)) %>% 
          set_names("conditional frequency", "conditional expectation")
        if(table == "conditional frequency") 
          result <- conditional_choice[["conditional frequency"]]
        else if(table == "conditional expectation")
          result <- conditional_choice[["conditional expectation"]]
      } else 
        result <- glue("Conditional values of {num_int_var} based on all levels of dataset") 
      
    }else result <- NULL
  }
  result
}
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "frequency")
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "frequency",
#                        plot = "acumulative_1")
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "frequency",
#                        plot = "histogram")
# 
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "central")
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "central",
#                        plot = "comparing_means")
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "central",
#                        plot = "standard_error")
# 
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "central",
#                        plot = "table")
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "central",
#                        plot = "level_general_boxplot")
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "central",
#                        plot = "level_bins_boxplot")
# 
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "dispersion")
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "dispersion")
# 
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "coef_var",
#                        description = TRUE)
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "standard_error",
#                        description = TRUE)
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "skeweness",
#                        description = TRUE)
# general_Descript_Stats(df = df,
#                        level = level_string,
#                        table = "kurtosis",
#                        description = TRUE)
# 
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "conditional frequency")
# general_Descript_Stats(df = df,
#                        level = "all",
#                        table = "conditional expectation")

#' @SUBFUNCTION
#' @description 
#' @param
#' @return
help_distribution <- function(df, num_int_var, factor_variables, 
                              func = NULL,
                              distribution = FALSE){
  
  distributions <- marginal_distribution(df = df, 
                                         num_int_var = num_int_var,
                                         factor_variables = factor_variables)
  if(distribution) result <- distributions
  else{
    dfs_fators <- map(distributions, 
                      ~.x[,factor_variables, with = FALSE] %>% unique())
    
    each <- map(distributions, func)
    
    df_help <- map2_df(dfs_fators, each,
                       ~cbind(.x, .y)) %>% as.data.table() 
    result <- df_help
  }
  result
}
# dfs <- help_distribution(df = df,
#                   num_int_var = num_int_var,
#                   factor_variables = factor_variables,
#                   distribution = TRUE)

# PARTICULAR FUNCTIONS ----------------------------------------------------
# df2 <- dfs$`FEMALE,1`

# Frequency Distribution --------------------------------------------------
#' @description 
#' @param
#' @return
descript_frequency <- function(df, num_int_var, 
                               plot = c("table", "acumulative_1", 
                                        "acumulative_2", "histogram")){
  
  boundaries <- boundaries(df = df, num_int_var = num_int_var)
  
  bounds <- boundaries[["bounds"]] %>% 
    split(x = ., f = seq(.[,.N])) %>% 
    map(~do.call(what = "c", args = .x))
  
  freq <- map(bounds, ~df[get(num_int_var) %between% .x] %>% .[,.N]) %>% 
    flatten_int()
  
  if(sum(freq) == df[,.N]){
    percent_frequency <- round(x = (freq/df[,.N])*100, digits = 2)
    #total <- sum(freq)
    #total_percent <- sum(percent_frequency)
    cum_frequency <- cumsum(freq)
    percent_cum_frequency <- round(x = (cum_frequency/df[,.N])*100, digits = 2)
    
    frequency_df <- round(x = copy(boundaries[["bounds"]]), digits = 2) %>% 
      .[,':='(
        FREQUENCY = freq,
        CUM_FREQUENCY = cum_frequency,
        PERCENT = percent_frequency,
        CUM_PERCENT = percent_cum_frequency
      )]
    
    if(plot == "table"){
      result <- frequency_df 
      
    }else if(plot == "acumulative_1"){
      ogive_plot <- ggplot(frequency_df, aes(x = LS, y = CUM_PERCENT)) + 
        geom_line(color = "red") + 
        geom_point(color = "blue", shape = 15, size = 2) + 
        labs(title = "OGIVE",
             x = num_int_var,
             y = "ACUMULATIVE") 
      result <- design_plot(plot = ogive_plot)
      
    }else if(plot == "acumulative_2"){
      plot(ecdf(x = frequency_df$LS), 
           main = "",
           xlab = "",
           ylab = "",
           col = "blue",
           bg = 'transparent') 
      title("ACUMULATIVE DISTRIBUTION FUNCTION", col.main = "white")
      axis(1, col = "red", col.ticks = "red")
      axis(2, col = "red", col.ticks = "red")
      title(col.main = "white")
      mtext(num_int_var, side = 1, line = 3, col = "white", cex = 1)
      mtext("F(x)", side = 2, line = 3, col = "white", cex = 1)
      
      result <- recordPlot()
      
    }else if(plot == "histogram"){
      histogram <- copy(frequency_df)[, Xi:=LI + (boundaries[["bin_size"]]/2)]
      
      mode <- descriptive_mode(df = df, 
                               num_int_var = num_int_var, 
                               description = FALSE)
      mode_hist <- map(bounds, ~mode %between% .x) %>% 
        map(~any(.x))
      mode_hist <- mode_hist[mode_hist == TRUE] %>% 
        names()
      mode_hist <- map(mode_hist, ~pluck(bounds, .x)) %>% 
        map(~round(.x, digits = 2))
      mode_hist <- map_dbl(mode_hist, ~
                             histogram[LI == .x[["LI"]] & LS == .x[["LS"]], Xi])
      
      histogram <- ggplot(data = histogram, aes(x = Xi, y = FREQUENCY), 
                          width = boundaries[["bin_size"]]) + 
        geom_col(alpha = 0.4, fill = ifelse(histogram[,Xi] %in% mode_hist, 
                                            "pink", "red"), 
                 color = "black") + 
        geom_line(color = "blue") + 
        geom_point(color = "blue") +
        geom_text(aes(label = scales::percent(x = PERCENT, scale = 1)),
                  size = 3, vjust = -1) +
        labs(title = paste("Histogram of", num_int_var),
             x = num_int_var) 
      
      cent_tend_vals <- central_tendency(df = df, 
                                         num_int_var = num_int_var, 
                                         plot = "table")
      histogram <- histogram + 
        map(mode, ~geom_point(aes(x = .x, y = 0), color = "#d24cff")) + 
        geom_vline(aes(xintercept = cent_tend_vals[,MEAN]), 
                   col = "red", linetype = 1) +
        geom_vline(aes(xintercept = cent_tend_vals[,Q25]), 
                   col = "green", linetype = 6) +
        geom_vline(aes(xintercept = cent_tend_vals[,MEDIAN]), 
                   col = "green", linetype = 6) + 
        geom_vline(aes(xintercept = cent_tend_vals[,Q75]), 
                   col = "green", linetype = 6) + 
        
        geom_segment(aes(x = cent_tend_vals[,Q25], xend = cent_tend_vals[,Q75],
                         y = 0, yend = 0), color = "green") + 
        geom_point(aes(x = cent_tend_vals[,Q25], y = 0), color = "green") + 
        geom_point(aes(x = cent_tend_vals[,MEDIAN], y = 0), color = "green") + 
        geom_point(aes(x = cent_tend_vals[,Q75], y = 0), color = "green")
      
      dispersion_values <- dispersion_measures(df = df, 
                                               num_int_var = num_int_var)
      
      histogram <- histogram + geom_segment(
        aes(x = cent_tend_vals[,MEAN]-dispersion_values[,SD],
            xend = cent_tend_vals[,MEAN]+dispersion_values[,SD],
            y = max(FREQUENCY) + 1,
            yend = max(FREQUENCY) + 1), color = "orange") + 
        geom_point(aes(x = cent_tend_vals[,MEAN]-dispersion_values[,SD], 
                       y = max(FREQUENCY) + 1),
                   color = "orange") + 
        geom_point(aes(x = cent_tend_vals[,MEAN], 
                       y = max(FREQUENCY) + 1), 
                   color = "orange") + 
        geom_point(aes(x = cent_tend_vals[,MEAN]+dispersion_values[,SD],
                       y = max(FREQUENCY) + 1),
                   color = "orange")
      
      result <- design_plot(plot = histogram)
    }
  }else result <- glue("You need to define the correct frequency of {num_int_var}")
  result
}
# descript_frequency(df = df2, num_int_var = num_int_var,
#                                plot = "table")
# descript_frequency(df = df2, num_int_var = num_int_var,
#                              plot = "acumulative_1")
# descript_frequency(df = df2, num_int_var = num_int_var,
#                              plot = "acumulative_2")
# descript_frequency(df = df2, num_int_var = num_int_var,
#                              plot = "histogram")

#' @description 
#' @param
#' @return
boundaries <- function(df, num_int_var){
  
  df <- copy(df) %>% drop_na()
  
  bounds <- invoke_map(list(max, min),
                       list_of_lists(no_sublists = 2,
                                     element_sublists = 
                                       list(df[,get(num_int_var)])),
                       na.rm = TRUE) %>% 
    set_names(c("VALMAX","VALMIN")) 
  
  bins_number <- length(df[,get(num_int_var)]) %>% sqrt() %>% round()
  
  bin_size <- (bounds[["VALMAX"]]-bounds[["VALMIN"]])/bins_number
  
  First <- bounds[["VALMIN"]]
  for(i in seq_len(bins_number)){ #seq_len modify First vector
    interval_values <- First[i] + bin_size
    First <- c(First, interval_values)
  }
  interval_values <- First
  
  list(
    bins_number = bins_number,
    bin_size = bin_size,
    interval_values = interval_values,
    bounds = data.table(
      LI = interval_values[1:(length(interval_values)-1)],
      LS = interval_values[2:length(interval_values)])
  ) %>% 
    return()
}
# boundaries(df = df2, num_int_var = "PASSENGERID") 

# Central Tendency --------------------------------------------------------
#' @description 
#' @param
#' @return
central_tendency <- function(df, num_int_var, plot = c("table", "general_boxplot", 
                                                       "bins_boxplot")){
  
  probs_percentile <- list(
    Q25 = 0.25,
    Q75 = 0.75,
    P90 = 0.90,
    P95 = 0.95,
    P99 = 0.99)
  
  elements_sublists <- list(
    central = list_of_lists(no_sublists = 4, 
                            element_sublists = list(df[,get(num_int_var)])),
    percentil = list_of_lists(no_sublists = 5, 
                              element_sublists = list(df[,get(num_int_var)])))
  
  central_tendency_values_and_percentiles <- invoke_map(
    
    list(max, min, mean, median) %>% 
      append(list_of_lists(no_sublists = 5, 
                           element_sublists = quantile)),
    
    elements_sublists[["central"]] %>% 
      append(
        map2(elements_sublists[["percentil"]], probs_percentile, 
             ~ .x %>% append(.y))
      ),
    na.rm = TRUE
  ) %>% set_names("MAX", "MIN", "MEAN", 
                  "MEDIAN", "Q25", "Q75", 
                  "P90", "P95", "P99")
  
  df_central_tendency <- as.data.table(central_tendency_values_and_percentiles) %>% 
    .[,':='(MODE = descriptive_mode(df = df, 
                                    num_int_var = num_int_var, 
                                    description = TRUE),
            MIDRANGE = (MAX+MIN)/2
    )] %>% setcolorder(c("MODE", "MEAN", "MIN",
                         "Q25","MEDIAN", "Q75",
                         "P90", "P95", "P99",
                         "MAX", "MIDRANGE"))
  if(plot == "table"){
    result <- df_central_tendency
    
  }else if(plot == "general_boxplot"){
    general_boxPlot <- df[,num_int_var, with = FALSE] %>% 
      ggplot(aes(x = 1, y = get(num_int_var))) +
      geom_boxplot(fill = "red", col = "blue", alpha = 0.4) +
      geom_jitter(alpha = 0.2) + 
      labs(title = paste("Distribution Boxplot of", num_int_var),
           y = num_int_var)
    general_boxPlot <- design_plot(plot = general_boxPlot)
    result <- general_boxPlot
    
  }else if(plot == "bins_boxplot"){
    boundaries <- boundaries(df = df, num_int_var = num_int_var)
    
    bins_boxPlot <- df[,num_int_var, with = FALSE] %>% 
      ggplot(aes(x = cut(x = df[,get(num_int_var)], 
                         breaks = boundaries[["bins_number"]],
                         labels = str_c(
                           boundaries[["bounds"]][,LI] %>% round(digits = 2), 
                           boundaries[["bounds"]][,LS] %>% round(digits = 2),
                           sep = "-")), 
                 y = get(num_int_var))) +
      geom_boxplot(fill = "red", col = "blue", alpha = 0.4) +
      labs(title = paste("Boxplot in each Bin of", num_int_var),
           x = "bins",
           y = num_int_var)
    bins_boxPlot <- design_plot(plot = bins_boxPlot)
    result <- bins_boxPlot
  }
  result
}
# central_tendency(df = df2, num_int_var = num_int_var, plot = "table")
# central_tendency(df = df2, num_int_var = num_int_var, plot = "general_boxplot")
# central_tendency(df = df2, num_int_var = num_int_var, plot = "bins_boxplot")

#' @description 
#' @param
#' @return
descriptive_mode <- function(df, num_int_var,description = FALSE){
  
  mode <- df[,.N, by = num_int_var] %>% 
    .[N == max(N), get(num_int_var)] %>% 
    sort()
  
  if(description){
    if(length(mode) == 0)
      "YOU DO NOT HAVE MODE IN YOUR DISTRIBUTION"
    else{
      mode_text <- str_c(mode, collapse = ",")
      
      if(length(mode) == 1) glue("UNIMODAL:{mode_text}")
      else if(length(mode) == 2) glue("BIMODAL:{mode_text}")
      else if(length(mode) == 3) glue("TRIMODAL:{mode_text}")
      else{
        if(length(mode) %in% 4:5) glue("MULTI-MODAL:{mode_text}")
        else "MULTI-MODAL: more than 5 modes"
      } 
    }
  }else mode
}
#descriptive_mode(df = df2, num_int_var = num_int_var, description = FALSE)

# Measures of Dispersion --------------------------------------------------
#' @description 
#' @param
#' @return
#' variance
#' standard deviation
#' range
#' interquantil range
dispersion_measures <- function(df, num_int_var){
  
  dispersion_values <- invoke_map(list(var, sd),
                                  list_of_lists(no_sublists = 2,
                                                element_sublists = 
                                                  list(df[,get(num_int_var)])),
                                  na.rm = TRUE) %>% 
    set_names("VAR", "SD") %>% 
    as.data.table() 
  
  cent_tend_vals <- central_tendency(df = df, 
                                     num_int_var = num_int_var, 
                                     plot = "table")
  
  dispersion_values[,':='(IQR = cent_tend_vals[,c(Q75-Q25)],
                          RANGE = cent_tend_vals[,c(MAX-MIN)])]
  dispersion_values  
}
#dispersion_measures(df = df2, num_int_var = num_int_var)

# Coefficient of Variations -----------------------------------------------
#' @description 
#' @param
#' @return
#' coeffiicient of variation
#' ratio of the standard deviation to the sample mean
#' rules:
#' mean dif from 0
#' - coef<1 low variance
#' - coef>1 low variance

#' standard error #error bar
#' measure of the dispersion of sample means around the population mean. 
#' (mean in each bin (each bin is X1,X2,..,Xn iid) respect the total mean) 
#' this is how much sample means will vary from the standard deviation
#' it is generaly used in confidence intervals

#' coefficient of asimetry skewness: analyze if the distribution of the data is
#' simetric around the mean 
#' rules:
#' -sk>0 in the frequency plot, the data presents a tale goes to the right from the mean 
#' (Data is in the right hand away from the mean)positive skewness
#' -sk=0 in the frequency plot, the data is simetric, for each value on the left is 
#' another on the right in the same distance to the mean but with contrary signs
#' -sk<0 in the frequency plot, the data presents a tale goes to the left from the mean 
#' (Data is in the left hand away from the mean)negative skewness

#' kurtosis: degree of relative flattening of the data distribution
#' this unit of medicion of the form of the tails of the distribution of the data, 
#' where this unit wide or reduce the tailes of distribution of the sample with the
#' intention to check peak of the plot 
#' rules: 
#' k3>0 LEPTOCURTICA: fast decay, light tails
#' k3=0 MESOCURTICA: normal curve
#' k3<0 PLATICURTICA: slow decay, wide tails
statistical_coefficients <- function(df, num_int_var,
                                     unit = c("coef_var", "standard_error",
                                              "skeweness", "kurtosis"),
                                     description = FALSE){
  
  cent_tend_vals <- central_tendency(df = df, 
                                     num_int_var = num_int_var, 
                                     plot = "table")
  
  dispersion_values <- dispersion_measures(df = df, 
                                           num_int_var = num_int_var)
  
  boundaries <- boundaries(df = df, num_int_var = num_int_var)
  
  descriptions <- jsonlite::fromJSON("INFORMATION/3.1.DESCRIPTIVE_STATISTICS/descriptive_info.json")
  
  if(unit == "coef_var"){
    COEF_VAR <- ifelse(cent_tend_vals[,MEAN] != 0,
                       (dispersion_values[,SD]/cent_tend_vals[,MEAN]) %>% 
                         round(digits = 2),
                       descriptions[["COEFICIENT OF VARIATION"]]$`MEAN 0`)
    if(description){
      if(COEF_VAR < 1) 
        glue("{COEF_VAR}<1 {descriptions[['COEFICIENT OF VARIATION']]$`COEF_VAR<1`}")
      else if(COEF_VAR > 1) 
        glue("{COEF_VAR}>1 {descriptions[['COEFICIENT OF VARIATION']]$`COEF_VAR>1`}")
    }else COEF_VAR
    
  }else if(unit == "standard_error"){
    STANDAR_ERROR <- (dispersion_values[,SD]/boundaries[["bins_number"]]) %>% 
      round(digits = 2)
    if(description){
      glue("{STANDAR_ERROR} {descriptions[['STANDARD ERROR']]}")
    }else STANDAR_ERROR
    
  }else if(unit == "skeweness"){
    SK <- skewness(df[,get(num_int_var)]) %>% 
      round(digits = 2)
    if(description){
      if(SK < 0) glue("{SK}<0 {descriptions[['SKEWENESS']]$`SK < 0`}")
      else if(SK == 0) glue("{SK}=0 {descriptions[['SKEWENESS']]$`SK = 0`}")
      else if(SK > 0) glue("{SK}>0 {descriptions[['SKEWENESS']]$`SK > 0`}")
    }else SK
    
  }else if(unit == "kurtosis"){
    K <- kurtosis(df[,get(num_int_var)]) %>% 
      round(digits = 2)
    if(description){
      if(K < 0) glue("{K}<0 {descriptions[['KURTOSIS']]$`K < 0`}")
      else if(K == 0) glue("{K}=0 {descriptions[['KURTOSIS']]$`K = 0`}")
      else if(K > 0) glue("{K}>0 {descriptions[['KURTOSIS']]$`K > 0`}")
    }else K
  }
}
# statistical_coefficients(df = df2,
#                          num_int_var = num_int_var,
#                          unit = "standard_error", description = FALSE)

# Conditional Expectancy --------------------------------------------------
#' @description 
#' @param
#' @return
#MALE_3 <- distribution[[1]][,PASSENGERID]
#MALE_3_prop <- MALE_3/sum(MALE_3)
# MALE_3_df <- data.table(
#   MALE_3,
#   MALE_3_prop
# )
#FEMALE_1 <- distribution[[3]][,PASSENGERID]
#int <- intersect(MALE_3, FEMALE_1)
#MALE_3_df2 <- MALE_3_df[MALE_3%in%int]
#p <- MALE_3_df2[,sum(MALE_3_prop)]
#MALE_3_df2[,cond:=MALE_3_prop/p]
# cond_mean <- MALE_3_df2[,MALE_3*cond] %>% sum()
# cond_cuad_mean <- MALE_3_df2[,(MALE_3^2)*cond] %>% sum()
# var_mean <- cond_cuad_mean-(cond_mean^2)
# sd_cond <- sqrt(var_mean)
conditional_stats <- function(df, 
                              num_int_var, 
                              distribution, 
                              table = c("conditional frequency",
                                        "conditional expectation")){
  
  x_var <- map(distribution, ~.x[,get(num_int_var)])
  x_var_prop <- map(x_var, ~.x/sum(.x, na.rm = TRUE))
  
  x_var_df <- map2(x_var, x_var_prop, ~data.table() %>% 
                     .[,(num_int_var):= .x] %>% 
                     .[,PROB:=.y])
  
  # Intersection 
  possibilities_intesect <- map(1:length(x_var), function(i){
    map(1:length(x_var), function(j){
      if(i==j) NULL
      else{
        intersect(pluck(x_var, i), pluck(x_var, j))
      }
    }) 
  })
  
  names_possibilities <- map(1:length(x_var), function(i){
    map(1:length(x_var), function(j){
      str_c(names(x_var[i]),names(x_var[j]), sep = "-")
    }) 
  }) 
  
  for(i in 1:length(possibilities_intesect)){
    names(possibilities_intesect[[i]]) <- unlist(names_possibilities[[i]])
  }
  
  possibilities_intesect <- possibilities_intesect %>% map(~compact(.x)) 
  possibilities_intesect <- 
    possibilities_intesect[map(possibilities_intesect, length) > 0] %>% 
    unlist(.,recursive=FALSE)
  
  # dfs of the names intersection posibilities 
  split_names_possibilites <- str_split(string = names(possibilities_intesect),
                                        pattern = "-")
  x_var_df <- map(1:length(split_names_possibilites), function(i){
    each <- split_names_possibilites[[i]]
    
    map(each, ~pluck(x_var_df, .x)) %>% 
      set_names(each)
  }) 
  
  x_var_df <- map(1:length(x_var_df), function(i){
    map(x_var_df[[i]], 
        ~.x[get(num_int_var) %in% possibilities_intesect[[i]]])
  }) %>% set_names(names(possibilities_intesect))
  
  walk(x_var_df, function(i){
    walk(i, ~.x[,PROB_COND:=PROB/(.x[,sum(PROB, na.rm = TRUE)])])
  })
  
  if(table == "conditional frequency"){
    name_var_df <- reference_name_cond(df = x_var_df)
    walk2(x_var_df, name_var_df, function(x,y){
      walk2(x,y, ~.x[,COND:=.y])
    })
    x_var_df <- x_var_df %>% flatten_df()
    walk(c("PROB","PROB_COND"), 
         ~x_var_df[,(.x):=scales::percent(x = get(.x))])
    result <- x_var_df
  }
  
  else if(table == "conditional expectation"){
    cond_stats <- map(copy(x_var_df), function(i){
      map(i, ~data.table(COND_MEAN = .x[,get(num_int_var)*PROB_COND] %>% 
                           sum(na.rm = TRUE),
                         COND_MEAN_CUAD = .x[,(get(num_int_var)^2)*PROB_COND] %>% 
                           sum(na.rm = TRUE)) %>% 
            .[,COND_VAR:=COND_MEAN_CUAD-(COND_MEAN^2)] %>% 
            .[,COND_SD:=sqrt(COND_VAR)] %>% 
            .[,"COND_MEAN_CUAD":=NULL])
    })
    name_var_cond_stats <- reference_name_cond(df = cond_stats)
    walk2(cond_stats, name_var_cond_stats, function(x,y){
      walk2(x,y, ~.x[,(num_int_var):=.y])
    })
    cond_stats <- cond_stats %>% flatten_df()
    round_vars <- str_subset(string = names(cond_stats), pattern = "COND[_].*")
    walk(round_vars, ~cond_stats[,(.x):=round(get(.x), digits = 2)])
    result <- cond_stats
  }else NULL
  result
}
# result <- conditional_stats(df = df,
#                   num_int_var = num_int_var,
#                   distribution = distribution,
#                   table = "conditional frequency")
# result <- conditional_stats(df = df,
#                             num_int_var = num_int_var,
#                             distribution = distribution,
#                             table = "conditional expectation")
# map(x_var_df, ~names(.x))
# names(possibilities_intesect)

#' @SUBFUNCTION
#' @description 
#' @param
#' @return
reference_name_cond <- function(df){
  
  map(1:length(df), function(i){
    
    each_sub_df <- df[i]
    
    names_each_sublist <- map(each_sub_df, ~names(.x)) %>% 
      unlist(recursive = FALSE) %>% unname()
    
    name_sublist <- names(each_sub_df)
    
    map(names_each_sublist, 
        ~c(.x, str_split(name_sublist, pattern = "-") %>% flatten_chr()) %>% 
          unique() %>% str_c(collapse = " given ")) %>% 
      set_names(names_each_sublist)
  }) %>% set_names(names(df)) %>% 
    return()
}