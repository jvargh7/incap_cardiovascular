cardio_df <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_df.RDS")) %>% 
  dplyr::filter(!is.na(mvpa_tot_mean)) %>% 
  mutate(exposure_group = case_when(steps_mean >= steps_cutoff & mvpa_recommended == 1 ~ 1,
                           steps_mean < steps_cutoff & mvpa_recommended == 1 ~ 2,
                           steps_mean < steps_cutoff & mvpa_recommended == 0 ~ 3,
                           steps_mean >= steps_cutoff & mvpa_recommended == 0 ~ 4,
                           TRUE ~ NA_real_)) %>% 
  dplyr::filter(exposure_group !=2) %>%  # Excluded because only 11 were in group 2 at steps_cutoff == 7500 
  mutate(exposure_group = factor(exposure_group,levels=c(1,3,4),labels=c("Hsteps_Hmvpa","Lsteps_Lmvpa","Hsteps_Lmvpa")))


library(mice)
mi_iter = 10

mi_null <- mice(cardio_df,
                maxit = 0)

method = mi_null$method
# method["steps1000_mean"] = "~I(steps_mean/1000)"
method["mvpa_recommended"] = "~I(mvpa_tot_mean >= (150/7))"
method["fat_kg"] = "~I(adwt-ffm_kg)"
method["fat_pct"] = "~I(fat_kg/adwt)"

# Each row corresponds to a variable block, i.e., a set of variables to be imputed. 
# A value of 1 means that the column variable is used as a predictor for the target block (in the rows). 
pred = mi_null$predictorMatrix

pred["iduni",] = 0
pred[,"iduni"] = 0

cardio_mice <- mice(cardio_df,
                 method = method,
                 pred = pred,
                 m=mi_iter,maxit=50,seed=500)

saveRDS(cardio_mice,paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))
