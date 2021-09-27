test_that("data import function gives appropriate results", {
  options(warn = -1) # avoid warning message due to deprecated in padr package
  
  # Create sample data for daily timestep tests -------------------------------
  df <- data.table(date = seq(as.Date("2012-05-05"), as.Date("2012-07-05"), 1),
                   discharge = seq(62, 1, -1))
  
  # error format
  path_df <- tempfile(fileext = ".txrt")
  write.table(df, path_df, sep = "\t", row.names = FALSE)
  expect_error(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d"), 
               "Only the following file formats are supported: .xls, .xlsx, .xlsm, .xlsb, .txt, .csv.")
  
  # basic
  path_df <- tempfile(fileext = ".txt")
  write.table(df, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d"), 
               df)
  
  # with discharge missing and interp
  df2 <- df %>% dplyr::slice(-c(25, 52))
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d"), 
               df)
  
  # with date missing and interp
  df2 <- df
  df2$date[25] <- NA
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d"), 
               df2)
  
  # with discharge missing and no interp
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df
  df_verif$discharge[c(25, 26, 27, 28, 29, 30)] <- NA
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d"), 
               df_verif)
  
  # with discharge missing and interp
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df
  df_verif$discharge[c(25, 26, 27, 28, 29, 30)] <- NA
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d", maxgap = 10), 
               df)
  
  # with discharge missing and no_na = TRUE
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df %>% dplyr::slice(31:62)
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d", maxgap = 5, no_NA = TRUE), 
               df_verif)
  
  
  
  
  # Create sample data for hourly timestep tests -----------------------------
  df <- data.table(date = seq(as.POSIXct("2012-05-05 00:00", tz = "UTC"), as.POSIXct("2012-07-05 00:00", tz = "UTC"), 3600),
                   discharge = seq(1465, 1, -1))
  
  # basic
  path_df <- tempfile(fileext = ".txt")
  write.table(df, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE), 
               df)
  
  # with discharge missing and interp
  df2 <- df %>% dplyr::slice(-c(25, 52))
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE), 
               df)
  
  # with date missing and interp
  df2 <- df
  df2$date[25] <- NA
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE), 
               df2)
  
  # with discharge missing and no interp
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df
  df_verif$discharge[c(25, 26, 27, 28, 29, 30)] <- NA
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE), 
               df_verif)
  
  # with discharge missing and interp
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df
  df_verif$discharge[c(25, 26, 27, 28, 29, 30)] <- NA
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, maxgap = 10), 
               df)
  
  # with discharge missing and no_na = TRUE
  df2 <- df %>% dplyr::slice(-c(25, 26, 27, 28, 29, 30))
  df_verif <- df %>% dplyr::slice(31:1465)
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, no_NA = TRUE), 
               df_verif)
  
  # test daily mean on hourly data ----------------------------------------
  df_mean <- mean_discharge(df, "day")
  
  # normal
  write.table(df, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, mean = "day"), 
               df_mean)
  
  # with NA and interp
  df2 <- df %>% dplyr::slice(-c(29, 30))
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, mean = "day"), 
               df_mean)
  
  # with too much NA and no interp
  df2 <- df %>% dplyr::slice(-c(29, 30, 31, 32, 33, 34, 35, 36))
  df_mean2 <- df_mean
  df_mean2$discharge[2] <- NA
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, mean = "day"), 
               df_mean2)
  
  # with too much NA and interp
  df2 <- df %>% dplyr::slice(-c(29, 30, 31, 32, 33, 34, 35, 36))
  path_df <- tempfile(fileext = ".txt")
  write.table(df2, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, mean = "day", maxgap = 10), 
               df_mean)
  
  # test padding with weird timestep ----------------------------------------
  
  df <- data.table(date = seq(as.POSIXct("2012-05-05 00:00", tz = "UTC"), as.POSIXct("2012-07-05 00:00", tz = "UTC"), 2401),
                   discharge = seq(1465, 1, -1)) %>% dplyr::slice(-c(12,13,14,15,16,17))
  df2 <- padr::thicken(df, interval = "hour") %>% 
    dplyr::select(date_hour, discharge) %>% 
    dplyr::rename(date = date_hour) %>% 
    padr::pad(.) %>% 
    fill_gap(.) %>% 
    mean_discharge(., "hour")
  path_df <- tempfile(fileext = ".txt")
  write.table(df, path_df, sep = "\t", row.names = FALSE)
  expect_equal(import_data(path_df, delim =  "\t", date_format = "%Y-%m-%d %H:%M:%S", date_time = TRUE, maxgap = 5), 
               df2)
})





