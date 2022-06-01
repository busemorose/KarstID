## ---------------------------
##
## Purpose of script: define functions for part of the app: analysis of classified discharges
##
## Author: Guillaume Cinkus
##
## Date Created: 2021-08-10
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

erfinv <- function(x) qnorm((x + 1)/2) / sqrt(2)

log10_bounds <- function(x, method) {
  if (method == "ceiling") return(10 ^ (ceiling(log10(x))))
  if (method == "floor") return(10 ^ (floor(log10(x))))
}

fdc_mangin <- function(discharge) {
  discharge_ordered <- sort(discharge)
  non.NA <- sum(!is.na(discharge_ordered)) 
  # Compute & plot probability plot
  quant <- (1 : non.NA) / non.NA
  # get index of unique discharge value
  index_unique <- length(discharge_ordered) - match(unique(discharge_ordered), rev(discharge_ordered)) + 1
  # filter the initial discharge vector with only unique values
  discharge_ordered_unique <- discharge_ordered[index_unique]
  quant <- quant[index_unique]
  
  y_class <- sqrt(2) * erfinv(quant)
  discharge_duration_curve_df <- data.frame(discharge = discharge_ordered_unique, 
                                            prob_exceedance = y_class) %>% 
    dplyr::slice(1:(dplyr::n() - 1))
}

fdc_normal <- function(discharge) {
  discharge_ordered <- sort(discharge)
  non.NA <- sum(!is.na(discharge_ordered)) 
  # Compute & plot probability plot
  quant <- (1 : non.NA) / non.NA
  # get index of unique discharge value
  index_unique <- length(discharge_ordered) - match(unique(discharge_ordered), rev(discharge_ordered)) + 1
  # filter the initial discharge vector with only unique values
  discharge_ordered_unique <- discharge_ordered[index_unique]
  quant <- quant[index_unique]
  
  discharge_duration_curve_df <- data.frame(prob_exceedance = quant, 
                                            discharge = discharge_ordered_unique) %>% 
    dplyr::mutate(prob_exceedance = 1 - prob_exceedance)
}

plot_fdc <- function(fdc_df, method, xlog = FALSE) {
  
  # Define breaks when using logarithmic scale
  minor_breaks <- rep(1:9, 21) * (10 ^ rep(-10:10, each = 9))
  
  if (method == "mangin") {  
    yticks <- c(0.10, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95, 0.98, 0.99, 0.995, 0.998, 0.9999)
    ylabel <- as.character(yticks)
    yticks <- sqrt(2) * erfinv(yticks)
    
    ggplot(fdc_df, aes(x = discharge, y = prob_exceedance)) + 
      geom_line(size = 0.8) +
      scale_y_continuous(breaks = yticks, labels = ylabel) +
      {
        if (xlog == TRUE) {
          # List two combine two ggplot2 elements
          list(scale_x_log10(minor_breaks = minor_breaks, 
                             limits = c(
                               log10_bounds(min(fdc_df$discharge), "floor"), 
                               log10_bounds(max(fdc_df$discharge), "ceiling"))),
               annotation_logticks(sides = "b"))
        }
      } +
      theme_bw() +
      ggtitle("Mangin Method") +
      xlab(expression("Discharge" ~(m^3~.s^-1))) +
      ylab("Probability exceedance") +
      theme(title = element_text(size = 16, color = "#2d2d2d"),
            axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"))
    
  } else if (method == "normal") {
    ggplot(fdc_df, aes(x = prob_exceedance, y = discharge)) + 
      geom_line(size = 0.8) +
      theme_bw() +
      ggtitle("Classic Method") +
      xlab("Probability exceedance") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      theme(title = element_text(size = 16, color = "#2d2d2d"),
            axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"))
  }
}