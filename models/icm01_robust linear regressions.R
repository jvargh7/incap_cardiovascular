rm(list=ls());gc();source(".Rprofile")

cardio_mice <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))
source("preprocessing/icpaux01_variable list.R")
source("C:/code/external/functions/imputation/adjusted_ci.R")
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

summary_exposure_group <- map_dfr(outcome_vars,
                                     function(o){
                                       formula_o <- paste0(o, " ~ relevel(exposure_group,ref='Lsteps_Lmvpa') + adht + moscho_imputed + moage_imputed + gtatole + full + male + chbirtho + age + adeduyr + fat_kg + ffm_kg + wealth");
                                       
                                       if(o %in% c("Glucose","hba1c")){
                                         formula_o <- paste0(formula_o," + dmmed")
                                       } else if(o %in% c("adsbp","addbp","pwv")){
                                         formula_o <- paste0(formula_o," + htnmed")
                                       }
                                       
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
  dplyr::filter(str_detect(term,"exposure_group"))


summary_exposure_group %>% 
  write_csv(.,"models/icm01_robust linear regressions by exposure group.csv")
