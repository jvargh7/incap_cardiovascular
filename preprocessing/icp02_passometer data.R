# Originally: "C:/code/incap/2015-18/f4k01_passometer data.R"


f4k_df <- haven::read_dta(paste0(path_gtml_earlier_data, "/ELIO all 20 Jun 2017.dta")) %>% 
# f4k_df <- read_dta(paste0(path_gtml_earlier_data, "/META data as of 20 Jun 2017_stata12.dta")) %>% 
  dplyr::select(iduni,sexo,height,weight,starts_with("f4k")) %>% 
  mutate_at(vars(sexo), function(x) factor(x,levels=attr(x,"labels"),labels=attr(x,"labels") %>% attr(.,"names"))) %>% 
  rename(start_day = f4k1_1,
         start_date = f4k1_2,
         start_hh = f4k3_1,
         device_id = f4k2,
         start_mm = f4k3_2,
         steps_sun = f4k4,
         steps_mon = f4k5,
         steps_tue = f4k6,
         steps_wed = f4k7,
         steps_thu = f4k8,
         steps_fri = f4k9,
         steps_sat = f4k10,
         
         mvpa_mm_sun = f4k11_1,
         mvpa_mm_mon = f4k12_1,
         mvpa_mm_tue = f4k13_1,
         mvpa_mm_wed = f4k14_1,
         mvpa_mm_thu = f4k15_1,
         mvpa_mm_fri = f4k16_1,
         mvpa_mm_sat = f4k17_1,
         
         mvpa_ss_sun = f4k11_2,
         mvpa_ss_mon = f4k12_2,
         mvpa_ss_tue = f4k13_2,
         mvpa_ss_wed = f4k14_2,
         mvpa_ss_thu = f4k15_2,
         mvpa_ss_fri = f4k16_2,
         mvpa_ss_sat = f4k17_2,
         
         
         end_day = f4k18_1,
         end_date = f4k18_2,
         end_hh = f4k19_1,
         end_mm = f4k19_2,
         reason_nonresponse = f4k20_1
         ) %>% 
  dplyr::select(iduni,starts_with("start"),starts_with("end"),everything())  %>% 
  mutate_at(vars(starts_with("steps_")),function(x) case_when(x < 2000 ~ NA_real_,
                                                              x > 40000 ~ NA_real_,
                                                              TRUE ~ x)) %>% 
  mutate(mvpa_mm_sun = case_when(is.na(steps_sun) ~ NA_real_,
                                 TRUE ~ mvpa_mm_sun),
         mvpa_mm_mon = case_when(is.na(steps_mon) ~ NA_real_,
                                 TRUE ~ mvpa_mm_mon),
         mvpa_mm_tue = case_when(is.na(steps_tue) ~ NA_real_,
                                 TRUE ~ mvpa_mm_tue),
         mvpa_mm_tue = case_when(is.na(steps_tue) ~ NA_real_,
                                 TRUE ~ mvpa_mm_tue),
         mvpa_mm_wed = case_when(is.na(steps_wed) ~ NA_real_,
                                 TRUE ~ mvpa_mm_wed),
         mvpa_mm_thu = case_when(is.na(steps_thu) ~ NA_real_,
                                 TRUE ~ mvpa_mm_thu),
         mvpa_mm_fri = case_when(is.na(steps_fri) ~ NA_real_,
                                 TRUE ~ mvpa_mm_fri),
         mvpa_mm_sat = case_when(is.na(steps_sat) ~ NA_real_,
                                 TRUE ~ mvpa_mm_sat),
         mvpa_ss_sun = case_when(is.na(steps_sun) ~ NA_real_,
                                 TRUE ~ mvpa_ss_sun),
         mvpa_ss_mon = case_when(is.na(steps_mon) ~ NA_real_,
                                 TRUE ~ mvpa_ss_mon),
         mvpa_ss_tue = case_when(is.na(steps_tue) ~ NA_real_,
                                 TRUE ~ mvpa_ss_tue),
         mvpa_ss_tue = case_when(is.na(steps_tue) ~ NA_real_,
                                 TRUE ~ mvpa_ss_tue),
         mvpa_ss_wed = case_when(is.na(steps_wed) ~ NA_real_,
                                 TRUE ~ mvpa_ss_wed),
         mvpa_ss_thu = case_when(is.na(steps_thu) ~ NA_real_,
                                 TRUE ~ mvpa_ss_thu),
         mvpa_ss_fri = case_when(is.na(steps_fri) ~ NA_real_,
                                 TRUE ~ mvpa_ss_fri),
         mvpa_ss_sat = case_when(is.na(steps_sat) ~ NA_real_,
                                 TRUE ~ mvpa_ss_sat)
         
         ) %>% 
  
  mutate(mvpa_tot_sun = mvpa_mm_sun + mvpa_ss_sun/60,
         mvpa_tot_mon = mvpa_mm_mon + mvpa_ss_mon/60,
         mvpa_tot_tue = mvpa_mm_tue + mvpa_ss_tue/60,
         mvpa_tot_wed = mvpa_mm_wed + mvpa_ss_wed/60,
         mvpa_tot_thu = mvpa_mm_thu + mvpa_ss_thu/60,
         mvpa_tot_fri = mvpa_mm_fri + mvpa_ss_fri/60,
         mvpa_tot_sat = mvpa_mm_sat + mvpa_ss_sat/60
         ) %>% 
  
  dplyr::filter(is.na(reason_nonresponse),!is.na(device_id)) %>% 
  mutate(mvpa_mm_mean = rowMeans(.[,regexpr("mvpa_mm_",colnames(.))>0],na.rm=TRUE),
         mvpa_tot_mean = rowMeans(.[,regexpr("mvpa_tot_",colnames(.))>0],na.rm=TRUE),
         steps_mean = rowMeans(.[,regexpr("steps_",colnames(.))>0],na.rm=TRUE),
         na_any = apply(.[,paste0("steps_",c("mon","tue","wed","thu","fri",
                                             "sun","sat"))],1,function(x) sum(is.na(x))),
         na_weekdays = apply(.[,paste0("steps_",c("mon","tue","wed","thu","fri"))],1,function(x) sum(is.na(x))),
         na_weekend = apply(.[,paste0("steps_",c("sun","sat"))],1,function(x) sum(is.na(x))))


saveRDS(f4k_df,paste0(path_incap_rally_box,"/Activity trajectories/working/f4k_passometer data before filtering.RDS"))

# Checking observations ----------
with(f4k_df,
     table(na_weekdays,na_weekend))


# Assessing for intra-class correlation (at iduni level) --------
library(lme4)
f4k_df %>% 
  mutate(ndays = as.numeric(end_date - start_date)) %>% 
  dplyr::select(iduni,ndays,start_day,contains("steps")) %>% 
  pivot_longer(cols=contains("steps"),names_to="day",values_to="steps") %>% 
  dplyr::filter(!is.na(steps),!is.na(day)) %>% 
  mutate(weekend = case_when(day %in% c("sat","sun") ~ 1,
                             TRUE ~ 0)) %>% 
  
  mutate(day = str_replace(day,"steps_","") %>% factor(.,levels=c("mon","tue",
                                                                  "wed","thu","fri","sat","sun","mean"))) %>% 
  dplyr::filter(day != "mean") %>% 
  mutate(day = as.numeric(day)) %>% 
  lmer(steps ~ day + (1|iduni),data=.) %>%
  performance::icc(.)

with(f4k_df,table(na_weekdays<=2,na_weekend<=1))
with(f4k_df,table(na_any<=4))

  # SAVE ----------  
f4k_df %>%
  dplyr::filter(na_any <= 4) %>% 
saveRDS(.,paste0(path_incap_rally_box,"/Activity trajectories/working/f4k_passometer data.RDS"))

write_dta(f4k_df,paste0(path_incap_rally_box,"/Activity trajectories/shared/f4k_passometer data before filtering_stata12.dta"),version = 12)
write_dta(f4k_df,paste0(path_incap_rally_box,"/Activity trajectories/shared/f4k_passometer data before filtering_stata14.dta"),version = 14)
writexl::write_xlsx(f4k_df,paste0(path_incap_rally_box,"/Activity trajectories/shared/f4k_passometer data before filtering.xlsx"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))

dictionary_file(f4k_df,type="dta")



  
