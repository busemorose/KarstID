## ---------------------------
##
## Purpose of script: define functions for 3rd part of the app: recession model 
##
## Author: Guillaume Cinkus
##
## Date Created: 2021-08-05
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes:
## # 
## ---------------------------

hom_model <- function(timestep, maximal_infiltration_flowrate, infiltration_speed, infiltration_heterogeneity_coef){
  maximal_infiltration_flowrate * ((1 - infiltration_speed * timestep) / (1 + infiltration_heterogeneity_coef * timestep))
}

exp_model <- function(timestep, baseflow_extrapolated_at_t0, depletion_coef){
  baseflow_extrapolated_at_t0 * exp(-depletion_coef * timestep)
}

# timestep is either 1 for daily timestep or 24 for hourly timestep
model_mangin <- function(recession_dataset, breakpoint, vtransit, timestep = 1) {
  
  mangin <- list("recession" = data.table(),
                 "k" = as.numeric(),
                 "i" = as.numeric(),
                 "alpha" = as.numeric())
  
  recession_dataset <- data.table::dcast(recession_dataset, t ~ variable) # wide format
  
  # non influenced regime
  ni_regime <- recession_dataset[t >= breakpoint]
  
  exp_model <- minpack.lm::nlsLM(discharge ~ exp_model(t, qr0, alpha),
                                 data = ni_regime,
                                 start = list(qr0 = 10, alpha = 0.01), 
                                 control = list(maxiter = 1000))
  
  qr0 <- summary(exp_model)$parameters[1]
  alpha <- summary(exp_model)$parameters[2]
  
  recession_dataset[, phi_t := exp_model(t, qr0, alpha)]
  
  # influenced regime
  q0 <- max(recession_dataset$discharge, na.rm = TRUE) - qr0
  eta <- 1 / breakpoint
  
  i_regime <- recession_dataset[t <= breakpoint]
  i_regime[, discharge := discharge - phi_t]
  
  hom_model <- minpack.lm::nlsLM(discharge ~ hom_model(t, q0, eta, epsilon),
                                 data = i_regime, 
                                 start = list(epsilon = 1), 
                                 control = list(maxiter = 1000))
  
  epsilon <- summary(hom_model)$parameters[1]
  recession_dataset[t <= breakpoint, psi_t := hom_model(t, q0, eta, epsilon)]
  
  # simulated discharge
  recession_dataset[, sim_discharge := rowSums(.SD, na.rm = TRUE), .SDcols = c("phi_t", "psi_t")]
  
  # indicators
  vdyn <- 86400 * (recession_dataset$sim_discharge[(breakpoint + 1)] / alpha) # +1 for row index
  
  k <- vdyn / vtransit
  i <- hom_model(2 * timestep, 1, eta, epsilon)
  
  mangin$recession <- recession_dataset
  mangin$k <- k / timestep
  mangin$i <- i
  mangin$alpha <- alpha * timestep
  
  return(mangin)
}

plot_rc_model <- function(recession, rc_model, breakpoint) {
  
  if (breakpoint < 2 | !is.numeric(breakpoint) | breakpoint >= max_bp_value(recession$value)) {
    ggplot(recession, aes(t, value, color = variable)) +
      geom_line(size = 0.8) +
      theme_bw() +
      xlab("Date") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      scale_color_manual("",
                         values = c("discharge" = "black",
                                    "sim_discharge" = "orangered3"),
                         label = c("Observed discharge", "Simulated discharge")) +
      theme(axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"),
            legend.text = element_text(size = 14),
            legend.position = "top") + 
      guides(color = guide_legend(override.aes = list(size = 2)))
  } else {
    model <- melt(rc_model, id.vars = "t", measure.vars = c("discharge", "sim_discharge"))
    ggplot(model, aes(t, value, color = variable)) +
      geom_line(size = 0.8) +
      geom_vline(xintercept = breakpoint) +
      theme_bw() +
      xlab("Date") +
      ylab(expression("Discharge" ~(m^3~.s^-1))) +
      scale_color_manual("",
                         values = c("discharge" = "black",
                                    "sim_discharge" = "orangered3"),
                         label = c("Observed discharge", "Simulated discharge")) +
      theme(axis.title = element_text(size = 16, color = "#2d2d2d"),
            axis.text = element_text(size = 14, color = "#2d2d2d"),
            legend.text = element_text(size = 14),
            legend.position = "top") + 
      guides(color = guide_legend(override.aes = list(size = 2)))
  }
}

rm_peak <- function(rc_df) {
  length <- length(rc_df$t)
  index <- NULL
  
  for (i in 0:length) {
    index <- c(index, which(rc_df$discharge[i:length] > rc_df$discharge[i]) + (i - 1))
  }
  
  na_index <- unique(index)
  rc_df$discharge[na_index] <- NA
  return(rc_df)
}

max_bp_value <- function(discharge) {
  if (any(is.na(discharge))) {
    x <- rle(is.na(discharge))
    x$lengths <- cumsum(x$lengths) - x$lengths
    x <- max(x$lengths) - 1
  } else {
    x <- length(discharge) - 1
  }
}

rmse <- function(obs, sim) {
  sqrt(mean((obs - sim) ^ 2, na.rm = TRUE))
}
