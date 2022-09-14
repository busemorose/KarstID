## ---------------------------
##
## Purpose of script: define functions for part of the app: simple correlational and spectral analyses
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
  f <- (f * timestep)[1:max_lag]
  sf <- (sf / timestep)[1:max_lag]
  
  # calculation of memory effect
  index_l0.2 <- min(which(acf$acf < 0.2))
  index_me <- which.min(c(abs(acf$acf[index_l0.2-1] - 0.2), abs(acf$acf[index_l0.2] - 0.2))) + index_l0.2 - 2
  memory_effect <- acf$lag[index_me] / timestep
  
  # calculation of regulation time
  regulation_time <- (max(sf) / 2) 
  
  # list for export
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
    geom_hline(yintercept = 0.2, linetype = "dashed", color = "#2d2d2d", alpha = 0.5) +
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
    coord_cartesian(xlim = c(0, 0.5)) +
    theme(title = element_text(size = 16, color = "#2d2d2d"),
          axis.title = element_text(size = 16, color = "#2d2d2d"),
          axis.text = element_text(size = 14, color = "#2d2d2d"))
}
