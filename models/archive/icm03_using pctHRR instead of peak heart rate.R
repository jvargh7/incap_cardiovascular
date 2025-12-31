cardio_mice <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))
source("preprocessing/icpaux01_variable list.R")
source("C:/code/external/functions/imputation/adjusted_ci.R")
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

summary_aerobic_fitness_pctHRR <- map_dfr(outcome_vars,
                                   function(o){
                                     formula_o <- paste0(o, " ~ residual_distance + baseline + pctHRR + drop6to7 + adht + moscho_imputed + 
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
  dplyr::filter(term %in% c("residual_distance","baseline","pctHRR","drop6to7")) 



summary_aerobic_fitness_pctHRR %>% 
  mutate(term = factor(term,levels=c("steps1000_mean","mvpa_recommended",
                                     "residual_distance","baseline",
                                     "pctHRR","drop6to7",
                                     "I(handgrip * 10)"),
                       labels = c("Steps in 1000s",
                                  "MVPA of atleast 150min",
                                  "Excess distance (m)",
                                  "Baseline HR",
                                  "Percentage HRR","Recovery HR",
                                  "Grip strength in kg \nper 10kg body weight"))) %>% 
  dplyr::select(variable,term,Coefficient) %>% 
  pivot_wider(names_from=term,values_from=Coefficient) %>% 
  write_csv(.,"models/icm03_using pctHRR for robust linear regression.csv")



