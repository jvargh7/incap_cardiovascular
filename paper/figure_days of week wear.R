rm(list = ls());gc();source(".Rprofile")

library(naniar)

f4k_df <- readRDS(paste0(path_incap_rally_box,"/Activity trajectories/working/f4k_passometer data before filtering.RDS")) %>% 
  dplyr::select(iduni,contains("steps")) %>%   dplyr::select(-steps_mean) 
 


f4k_df %>% 
  pivot_longer(cols=contains("steps"),names_to="day",values_to="steps",names_prefix = "steps_")

f4k_df %>% 
  dplyr::select(-iduni) %>% 
  gg_miss_var(.,show_pct = TRUE)

library(visdat)
f4k_df %>% 
  dplyr::select(-iduni) %>% 
  vis_dat()
