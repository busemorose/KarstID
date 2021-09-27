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
  discharge_duration_curve_df <- data.frame(discharge_ordered_unique, y_class) %>% 
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
  
  discharge_duration_curve_df <- data.frame(quant, discharge_ordered_unique) %>% 
    dplyr::mutate(quant = 1 - quant)
}

plot_fdc <- function(fdc_df, method) {
  if (method == "mangin") {  
    yticks <- c(0.10, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95, 0.98, 0.99, 0.995, 0.998, 0.9999)
    ylabel <- as.character(yticks)
    yticks <- sqrt(2) * erfinv(yticks)
    
    ggplot(fdc_df, aes(x = discharge_ordered_unique, y = y_class)) + 
      geom_line() + 
      scale_y_continuous(breaks = yticks, labels = ylabel)
    
  } else if (method == "normal") {
    ggplot(fdc_df, aes(x = quant, y = discharge_ordered_unique)) + 
      geom_line()
  }
}