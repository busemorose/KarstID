## ---------------------------
##
## Purpose of script: define functions for part of the app: correlational and spectral analyses
##
## Author: Guillaume Cinkus
##
## Date Created: 2021-08-09
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# timestep is either 1 for daily timestep or 24 for hourly timestep
acspf <- function(discharge, max_lag = 125, timestep = 1) {
  acspf <- list("k" = as.numeric(),
               "rk" = as.numeric(),
               "f" = as.numeric(),
               "sf" = as.numeric(),
               "mem_ef" = as.numeric(),
               "reg_time" = as.numeric())

  max_lag <- max_lag * timestep
  
  n <- length(discharge)
  acf <- stats::acf(discharge, max_lag)
  
  sf <- rep(NA, max_lag)
  k <- 1:max_lag
  f <- k / (2 * max_lag)
  for (j in 1:max_lag) {
    sum <- 0
    sum <- sum((1 + cos((pi * k) / max_lag)) / 2 * acf$acf[k+1] * cos(2 * pi * f[j] * k))
    sf[j] <- 2 * (1 + 2 * sum)
  }
  
  # slice to max f value = 0.5 for plotting
  f <- (f * timestep)[1:125]
  sf <- (sf / timestep)[1:125]
  
  memory_effect <- stats::approx(x = acf$acf, y = acf$lag, xout = (0.2))$y / timestep
  regulation_time <- (max(sf) / 2) 
  
  acspf$k = acf$lag
  acspf$rk = acf$acf
  acspf$f = f
  acspf$sf = sf
  acspf$mem_ef <- memory_effect
  acspf$reg_time <- regulation_time
  
  return(acspf)
}

plot_acf <- function(k, rk) {
  x <- data.frame(k = k, rk = rk)
  ggplot(x, aes(k, rk)) +
    geom_line(size = 0.8) +
    theme_bw() +
    ggtitle("Autocorrelation Function") +
    xlab("k (days)") +
    ylab(expression(r[k])) +
    theme(title = element_text(size = 16, color = "#2d2d2d"),
          axis.title = element_text(size = 16, color = "#2d2d2d"),
          axis.text = element_text(size = 14, color = "#2d2d2d"))
}

plot_spf <- function(f, sf) {
  x <- data.frame(f = f, sf = sf)
  ggplot(x, aes(f, sf)) +
    geom_line(size = 0.8) +
    theme_bw() +
    ggtitle("Variance Density Spectrum") +
    xlab(expression("f" ~(days^-1))) +
    ylab(expression(s[f])) +
    theme(title = element_text(size = 16, color = "#2d2d2d"),
          axis.title = element_text(size = 16, color = "#2d2d2d"),
          axis.text = element_text(size = 14, color = "#2d2d2d"))
}
