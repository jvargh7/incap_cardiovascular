cardio_mice <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))
source("preprocessing/icpaux01_variable list.R")

summary_physical_activity <- map_dfr(outcome_vars,
                                     function(o){
                                       formula_o <- paste0(o, " ~ steps1000_mean + mvpa_recommended + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + adeduyr + adbmi + formal + wealth");
                                       model <- (lm.mids(as.formula(formula_o),data = cardio_mice) %>% pool(.) %>% summary(.)) %>% 
                                         mutate(variable = o);
                                       return(model)
                                       
                                     }) %>% 
  dplyr::filter(term %in% c("steps1000_mean","mvpa_recommended")) %>% 
  mutate(LCI = estimate - 1.96*std.error,
         UCI = estimate + 1.96*std.error)


summary_aerobic_fitness <- map_dfr(outcome_vars,
                                     function(o){
                                       formula_o <- paste0(o, " ~ residual_distance + baseline + freq_peak + drop6to7 + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + adeduyr + adbmi + formal + wealth");
                                       model <- (lm.mids(as.formula(formula_o),data = cardio_mice) %>% pool(.) %>% summary(.)) %>% 
                                         mutate(variable = o);
                                       return(model)
                                       
                                     }) %>% 
  dplyr::filter(term %in% c("residual_distance","baseline","freq_peak","drop6to7")) %>% 
  mutate(LCI = estimate - 1.96*std.error,
         UCI = estimate + 1.96*std.error)

summary_muscle_strength <- map_dfr(outcome_vars,
                                   function(o){
                                     formula_o <- paste0(o, " ~ I(handgrip*10) + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + adeduyr + adbmi + formal + wealth");
                                     model <- (lm.mids(as.formula(formula_o),data = cardio_mice) %>% pool(.) %>% summary(.)) %>% 
                                       mutate(variable = o);
                                     return(model)
                                     
                                   }) %>% 
  dplyr::filter(term %in% c("I(handgrip * 10)"))  %>% 
  mutate(LCI = estimate - 1.96*std.error,
         UCI = estimate + 1.96*std.error)


bind_rows(summary_physical_activity %>% 
            mutate(exposure = "Physical Activity"),
          summary_aerobic_fitness %>% 
            mutate(exposure = "Aerobic Fitness"),
          summary_muscle_strength %>% 
            mutate(exposure = "Muscle Strength")) %>% 
  write_csv(.,paste0(path_incap_cardio_folder,"/working/summary_linear regressions.csv"))
