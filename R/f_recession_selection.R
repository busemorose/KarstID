## ---------------------------
##
## Purpose of script: define functions for 2nd part of the app: recession selection
##
## Author: Guillaume Cinkus
##
## Date Created: 2021-07-28
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes:
## # update_slider: avoid repetition in the code
##
## # plot_all_rc: plot function for manual recession curves selection
##   - display either (i) time series (if recession list empty) 
##                    (ii) recession curves over time series
##                    (iii) selected + recession curves over time series
##   - `type` and `ref` columns for color aes
## ---------------------------

update_slider <- function(session, inputId, dataset) {
  updateSliderInput(session,
                    inputId,
                    value = c(
                      min({{ dataset }}$date),
                      max({{ dataset }}$date)
                    ))
}

plot_all_rc <- function(dataset, recession_list, start, end, highlight) {
  dataset <- data.table(dataset)
  
  if (!length(recession_list)) {
    dataset[, type := "df"]
    
    ggplot(dataset, aes(date, discharge, color = type)) +
      geom_line(size = 0.8) +
      theme_bw() +
      xlab("Date") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      scale_color_manual("",
                         values = c("df" = "black",
                                    "rc" = "#3c96f0",
                                    "selected" = "orangered3"),
                         label = c("Observed discharge", "Retained recession curves", "Selected recession curves")) +
      theme(axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"),
            legend.text = element_text(size = 14),
            legend.position = "top") + 
      guides(color = guide_legend(override.aes = list(size = 2)))
  } else {
    r <- data.table::rbindlist(lapply(seq_along(recession_list),
                                      function(i) {
                                        dplyr::mutate(recession_list[[i]], ref = paste0("r", i))
                                      }))
    r[, type := "rc"]
    r$type[r$ref == paste0("r", highlight)] <- "selected" # filter if selected
    r[, t:= NULL] # remove t column for binding df
    dataset[, ref := "full"]
    dataset[, type := "df"]
    dplot <- rbindlist(list(dataset, r))
    dplot <- dplot[date > start & date < end] # filter date
    
    ggplot(dplot, aes(date, discharge, color = type, group = ref)) +
      geom_line(size = 0.8) +
      theme_bw() +
      xlab("Date") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      scale_color_manual("",
                         values = c("df" = "black",
                                    "rc" = "#3c96f0",
                                    "selected" = "orangered3"),
                         label = c("Observed discharge", "Retained recession curves", "Selected recession curves")) +
      theme(axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"),
            legend.text = element_text(size = 14),
            legend.position = "top") + 
      guides(color = guide_legend(override.aes = list(size = 2)))
  }
}
