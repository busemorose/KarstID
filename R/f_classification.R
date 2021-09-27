## ---------------------------
##
## Purpose of script: define functions for 3nd part of the app: classification
##
## Author: Guillaume Cinkus
##
## Date Created: 2021-08-07
##
## Email: guillaume.cinkus@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

import_classif_df <- function(datapath) {
  classif_bdd <- read_delim(paste0(datapath, ".txt")) %>% 
    dplyr::rename(ME = memory_effect,
           RT = regulation_time,
           System = system) %>% 
    dplyr::mutate(Distance = NA,
           Class = class_system(k_max, alpha_mean, IR)) %>% 
    dplyr::select(System, Class, Distance, k_max, alpha_mean, IR, mean, cv, svc, ME, RT)
  classif_bdd <- data.table(classif_bdd)
  
  saveRDS(classif_bdd, file = paste0(datapath, ".rds"))
}

class_system <- function(kmax, alphamean, IR) {
  ifelse(kmax <= 0.4, 
         ifelse(alphamean >= 0.03,
                ifelse(IR >= 0.25, "C1", "C2"),
                ifelse(IR >= 0.25, "C3", "C4")),
         ifelse(IR >= 0.25, "C5", "C6"))
}

carac_system <- function(name, class) {
  ifelse(class == "C1",
         paste0(name, " is classified C1, which characterize a system with poor capacity of dynamic storage, fast draining of the capacitive function and substantial variability of hydrological functioning."),
         ifelse(class == "C2",
                paste0(name, " is classified C2, which characterize a system with poor capacity of dynamic storage, fast draining of the capacitive function and low variability of hydrological functioning."),
                ifelse(class == "C3",
                       paste0(name, " is classified C3, which characterize a system with poor capacity of dynamic storage, moderate draining of the capacitive function and substantial variability of hydrological functioning."),
                       ifelse(class == "C4",
                              paste0(name, " is classified C4, which characterize a system with poor capacity of dynamic storage, moderate draining of the capacitive function and low variability of hydrological functioning."),
                              ifelse(class == "C5",
                                     paste0(name, " is classified C5, which characterize a system with noticeable capacity of dynamic storage, slow draining of the capacitive function and substantial variability of hydrological functioning."),
                                     paste0(name, " is classified C6, which characterize a system with noticeable capacity of dynamic storage, slow draining of the capacitive function and low variability of hydrological functioning.")
                                     )
                              )
                       )
                )
         )
}

adj_distance <- function(Ith, Icalc) {
  abs(Ith - Icalc) / Ith
}

diag_distance <- function(Ith_x, Icalc_x, Ith_y, Icalc_y) {
  sqrt(adj_distance(Ith_x, Icalc_x) ^ 2 + adj_distance(Ith_y, Icalc_y) ^ 2)
}

calc_class_distance <- function(class, kmax, alphamean, IR) {
  ifelse(class == "C1",
         return(C1_dist(alphamean, IR)),
         ifelse(class == "C2",
                return(C2_dist(alphamean, IR)),
                ifelse(class == "C3",
                       return(C3_dist(kmax, alphamean, IR)),
                       ifelse(class == "C4",
                              return(C4_dist(kmax, alphamean, IR)),
                              ifelse(class == "C5",
                                     return(C5_dist(kmax, IR)),
                                     return(C6_dist(kmax, IR))
                              )
                       )
                )
         )
  )
}

class_filename <- function(class) {
  system.file(paste0("class/", class, ".png"), package = "KarstID")
}

C1_dist <- function(alphamean, IR) {
  IR_th <- 0.25
  alpha_th <- 0.03
  C1 <- 0
  C2 <- adj_distance(IR_th, IR)
  C3 <- adj_distance(alpha_th, alphamean)
  C4 <- diag_distance(alpha_th, alphamean, IR_th, IR)
  C5 <- NA
  C6 <- NA
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

C2_dist <- function(alphamean, IR) {
  IR_th <- 0.25
  alpha_th <- 0.03
  C1 <- adj_distance(IR_th, IR)
  C2 <- 0
  C3 <- diag_distance(alpha_th, alphamean, IR_th, IR)
  C4 <- adj_distance(alpha_th, alphamean)
  C5 <- NA
  C6 <- NA
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

C3_dist <- function(kmax, alphamean, IR) {
  IR_th <- 0.25
  alpha_th <- 0.03
  k_th <- 0.4
  C1 <- adj_distance(alpha_th, alphamean)
  C2 <- diag_distance(alpha_th, alphamean, IR_th, IR)
  C3 <- 0
  C4 <- adj_distance(IR_th, IR)
  C5 <- adj_distance(k_th, kmax)
  C6 <- diag_distance(k_th, kmax, IR_th, IR)
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

C4_dist <- function(kmax, alphamean, IR) {
  IR_th <- 0.25
  alpha_th <- 0.03
  k_th <- 0.4
  C1 <- diag_distance(alpha_th, alphamean, IR_th, IR)
  C2 <- adj_distance(alpha_th, alphamean)
  C3 <- adj_distance(IR_th, IR)
  C4 <- 0
  C5 <- diag_distance(k_th, kmax, IR_th, IR)
  C6 <- adj_distance(k_th, kmax)
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

C5_dist <- function(kmax, IR) {
  IR_th <- 0.25
  k_th <- 0.4
  C1 <- NA
  C2 <- NA
  C3 <- adj_distance(k_th, kmax)
  C4 <- diag_distance(k_th, kmax, IR_th, IR)
  C5 <- 0
  C6 <- adj_distance(IR_th, IR)
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

C6_dist <- function(kmax, IR) {
  IR_th <- 0.25
  k_th <- 0.4
  C1 <- NA
  C2 <- NA
  C3 <- diag_distance(k_th, kmax, IR_th, IR)
  C4 <- adj_distance(k_th, kmax)
  C5 <- adj_distance(IR_th, IR)
  C6 <- 0
  round(c(C1, C2, C3, C4, C5, C6), 3) * 100
}

msg_dist <- function(x) {
  if (is.na(x)) "/" else paste0(x, "%")
}

show_popup <- function(msg) {
  showModal(modalDialog(
    title = "Classification may be biased",
    msg,
    easyClose = TRUE,
    footer = NULL
  ))
}

calc_syst_distance <- function(alpha_sys, alpha_calc, k_sys, k_calc, IR_sys, IR_calc) {
  alpha_dist <- adj_distance(alpha_sys, alpha_calc)
  k_dist <- adj_distance(k_sys, k_calc)
  IR_dist <- adj_distance(IR_sys, IR_calc)
  dist <- sqrt(alpha_dist^2 + k_dist^2 + IR_dist^2)
  round(dist, 2) * 100
}

