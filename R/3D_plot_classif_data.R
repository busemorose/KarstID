# default_dataset <- readr::read_delim("inst/extdata/default_dataset.txt",
#                                      delim = "\t")
# 
# classif_data_plot <- classif_bdd %>%
#   select(System, Class, k_max, alpha_mean, IR) %>%
#   mutate(id = seq_len(nrow(classif_bdd)),
#          color = recode(Class,`C1` = "#000000",
#                         `C2` = "#009E73",
#                         `C3` = "#e79f00",
#                         `C4` = "#9ad0f3",
#                         `C5` = "#0072B2",
#                         `C6` = "#D55E00")) %>%
#   arrange(System)
# 
# ms <- replicate(2, classif_data_plot, simplify = F)
# ms[[2]]$IR <- 0
# m <- group2NA(dplyr::bind_rows(ms), "id") %>%
#   mutate(System = ifelse(is.na(System), dplyr::lag(System), System))