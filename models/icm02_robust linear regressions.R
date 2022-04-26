cardio_mice <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))
source("preprocessing/icpaux01_variable list.R")
source("C:/code/external/functions/imputation/adjusted_ci.R")
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

summary_physical_activity <- map_dfr(outcome_vars,
                                     function(o){
                                       formula_o <- paste0(o, " ~ steps1000_mean + mvpa_recommended + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + age + adeduyr + adbmi + formal + wealth");
                                       
                                       model_list <- map(1:cardio_mice$m,
                                                         function(x){
                                                           df = complete(cardio_mice,x);
                                                           mod = lm(as.formula(formula_o),data = df);
                                                           return(mod)
                                                         }) %>% 
                                         clean_mi_conditionalregression(.,link="lm_robust") %>% 
                                         mutate(variable = o)
                                         return(model_list)
                                       
                                     }) %>% 
  rename(term = iv,
         std.error = sqrt_T_D,
         estimate = theta_D,
         LCI = lci,
         UCI = uci) %>% 
  dplyr::filter(term %in% c("steps1000_mean","mvpa_recommended"))


summary_aerobic_fitness <- map_dfr(outcome_vars,
                                   function(o){
                                     formula_o <- paste0(o, " ~ residual_distance + baseline + freq_peak + drop6to7 + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + age + adeduyr + adbmi + formal + wealth");
                                     model_list <- map(1:cardio_mice$m,
                                                       function(x){
                                                         df = complete(cardio_mice,x);
                                                         mod = lm(as.formula(formula_o),data = df);
                                                         return(mod)
                                                       }) %>% 
                                       clean_mi_conditionalregression(.,link="lm_robust") %>% 
                                       mutate(variable = o)
                                     return(model_list)
                                   }) %>% 
  rename(term = iv,
         std.error = sqrt_T_D,
         estimate = theta_D,
         LCI = lci,
         UCI = uci) %>% 
  dplyr::filter(term %in% c("residual_distance","baseline","freq_peak","drop6to7")) 

summary_muscle_strength <- map_dfr(outcome_vars,
                                   function(o){
                                     formula_o <- paste0(o, " ~ I(handgrip*10) + adht + moscho_imputed + 
                                       moage_imputed + gtatole + full + male + chbirtho + age + adeduyr + adbmi + formal + wealth");
                                     model_list <- map(1:cardio_mice$m,
                                                       function(x){
                                                         df = complete(cardio_mice,x);
                                                         mod = lm(as.formula(formula_o),data = df);
                                                         return(mod)
                                                       }) %>% 
                                       clean_mi_conditionalregression(.,link="lm_robust")  %>% 
                                       mutate(variable = o)
                                     return(model_list)
                                     
                                   }) %>% 
  rename(term = iv,
         std.error = sqrt_T_D,
         estimate = theta_D,
         LCI = lci,
         UCI = uci) %>% 
  dplyr::filter(term %in% c("I(handgrip * 10)"))


bind_rows(summary_physical_activity %>% 
            mutate(exposure = "Physical Activity"),
          summary_aerobic_fitness %>% 
            mutate(exposure = "Aerobic Fitness"),
          summary_muscle_strength %>% 
            mutate(exposure = "Muscle Strength")) %>% 
  write_csv(.,paste0(path_incap_cardio_folder,"/working/summary_robust linear regressions.csv"))
