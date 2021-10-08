## ---------------------------
##
## Purpose of script: define functions for 1st page of app: data_import
##
## Author: Guillaume Cinkus
##
## Date Created: 2020-12-09
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes: 
## # import_data: import several file formats : .txt, .xlsx, .csv
##                 - please import file as two columns (date/discharge), col names don't matter
##                 - Excel files does not need date_format input
##                 - mean argument is either "default", "hour" or "day", with "default" being the mean according to datetime
## # h_mean: perform hourly mean of a discharge time series
## # d_mean: perform daily mean of a discharge time series
## # fill_gap: perform a spline interpolation to fill gap in a discharge time series
##              - maxgap: define the number of max consecutive NA values to interpolate
##              - no_NA: if TRUE, return the longest observation without NA (after interpolation)
##
## ---------------------------

import_data <- function(filepath,
                        mean = "default", # c("day")
                        delim = ";", 
                        skip = 0,
                        header = TRUE, 
                        na = c("", "NA"), 
                        decimal_mark = ".", 
                        date_time = FALSE,
                        date_format = "%Y-%m-%d",
                        sheet = 1, 
                        maxgap = 5, 
                        no_NA = FALSE) {
  
  file_format <- stringr::str_extract(filepath, "\\w+$") # get file format
  
  if (header == TRUE) { # to prevent negative skip and to skip header as the col names are manually defined
    stopifnot(skip >= 0)
    skip <- skip + 1
  }
  
  if (date_time == FALSE && mean == "hour") stop("Can't perform a hourly mean on daily timestep.")
  
  # read_excel or read_delim
  
  if (file_format %in% c("xls", "xlsx", "xlsm", "xlsb")) {
    dataset <- readxl::read_excel(filepath,
                             sheet = sheet,
                             skip = skip,
                             na = na,
                             col_names = c("date", "discharge"))
    if (date_time == FALSE) {
      # in case of weird POSIXct cases or unusual date format
      if (class(dataset$date) != "Date") dataset$date <- as.Date(dataset$date, format = date_format)
      if (any(is.na(dataset$date))) return(data.table(dataset))
      if (mean == "default") mean <- "day"
      dataset <- padr::pad(dataset)
      dataset <- fill_gap(dataset, maxgap, no_NA)
      dataset <- mean_discharge(dataset, mean)
      return(dataset)
    } else {
      if (!("POSIXct" %in% class(dataset$date))) dataset$date <- as.POSIXct(dataset$date, format = date_format)
      if (mean == "default") mean <- "hour"
      if (any(is.na(dataset$date))) return(data.table(dataset))
      if (padr::get_interval(dataset$date) != "hour") dataset <- thicken_data(dataset) # if datetime interval is lower than <1hour
      dataset <- padr::pad(dataset)
      dataset <- fill_gap(dataset, maxgap, no_NA)
      dataset <- mean_discharge(dataset, mean)
      return(dataset)
    }
  } else if(file_format %in% c("txt", "csv")) {
    
    # col_date or col_datetime
    
    if (date_time == FALSE) {
      dataset <- readr::read_delim(filepath,
                                   delim = delim,
                                   skip = skip,
                                   na = na,
                                   locale = readr::locale(decimal_mark = decimal_mark),
                                   col_names = c("date", "discharge"),
                                   col_types = readr::cols(readr::col_date(date_format), readr::col_double()))
      if (any(is.na(dataset$date))) return(data.table(dataset))
      if (mean == "default") mean <- "day"
      dataset <- padr::pad(dataset)
      dataset <- fill_gap(dataset, maxgap, no_NA)
      dataset <- mean_discharge(dataset, mean)
      return(dataset)
    } else if (date_time == TRUE) {
      dataset <- readr::read_delim(filepath,
                                   delim = delim,
                                   skip = skip,
                                   na = na,
                                   locale = readr::locale(decimal_mark = decimal_mark),
                                   col_names = c("date", "discharge"),
                                   col_types = readr::cols(readr::col_datetime(date_format), readr::col_double()))
      if (any(is.na(dataset$date))) return(data.table(dataset))
      if (mean == "default") mean <- "hour"
      if (padr::get_interval(dataset$date) != "hour") dataset <- thicken_data(dataset) # if datetime interval is lower than <1hour
      dataset <- padr::pad(dataset)
      dataset <- fill_gap(dataset, maxgap, no_NA)
      dataset <- mean_discharge(dataset, mean)
      return(dataset)
    }
  } else {
      stop("Only the following file formats are supported: .xls, .xlsx, .xlsm, .xlsb, .txt, .csv.")
    }
}

mean_discharge <- function(dataset, timestep) {
  dataset <- data.table(dataset)
  dataset <- dataset[, date := lubridate::floor_date(date, timestep)]
  dataset <- dataset[, list(discharge = mean(discharge, na.rm = FALSE)), by = "date"]
  return(dataset)
}

fill_gap <- function(dataset, maxgap = 5, no_NA = FALSE) {
  dataset <- data.table(dataset)
  dataset <- dataset[, discharge := zoo::na.spline(discharge, method =  "monoH.FC", maxgap = maxgap)]
  dataset$discharge <- dplyr::if_else(dataset$discharge < 0, 0, dataset$discharge)
  
  if (no_NA == FALSE) {
    return(dataset)
  } else {
    dsg_na_test <- is.na(dataset$discharge)
    dt_na <- data.table(dsg_na_test, runid = rleid(dsg_na_test))
    dt_stats <- dt_na[, .(length = .N, position = .I[1], type = dsg_na_test[1]), by = runid]
    dt_stats <- dt_stats[type == FALSE]
    dt_stats <- dt_stats[which.max(length)]
    longest_not_na <- c(dt_stats$position, dt_stats$position + dt_stats$length - 1)
    no_na_dataset <- dataset[longest_not_na[1]:longest_not_na[2]]
    
    return(no_na_dataset)
  }
}

thicken_data <- function(dataset) {
  dataset <- padr::thicken(dataset, interval = "hour", drop = TRUE) %>% 
    dplyr::rename(date = date_hour) %>% 
    dplyr::select(date, discharge)
}

about_popup <- function() {
  msg = HTML(paste0("KarstID proposes the application of common analyses of karst spring hydrographs in R through a Shiny application. It includes recession curves, statistical, classified discharges and correlational and spectral analyses. The application also allows performing a classification of the hydrological functioning and comparing the results to a database of 78 karst systems.",
                    "<h3>Authors</h3>",
                    "Guillaume Cinkus, Naomi Mazzilli & Hervé Jourde",
                    "<h3>Contact</h3>",
                    "guillaume.cinkus@umontpellier.fr",
                    "<h3>References</h3>",
                    includeHTML(system.file("extdata/references.html", package = "KarstID")),
                    "<h3>License</h3>",
                    includeHTML(system.file("extdata/license.html", package = "KarstID"))))
  
  showModal(modalDialog(
    title = "About KarstID",
    msg,
    easyClose = TRUE,
    footer = NULL
  ))
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("extdata",
                         system.file("extdata",
                                     package = "KarstID"))
}
