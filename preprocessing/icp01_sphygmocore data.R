# Originally: "C:/code/incap/2015-18/f4d01_sphygmocore data.R"

# DFA ----------
gtml_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == 2) %>% 
  # dplyr::filter(!is.na(gtadladdercommunity2018)|!is.na(gtadladdereconomic2018)) %>% 
  dplyr::mutate(id_uni = pin - 20000000) 

hr_reserve <- readxl::read_excel(paste0(path_incap_rally_box,"/Heart rate response/working/HRreserve calculation.xlsx"),
                                 sheet = "Hoja1") %>% 
  rename(pctHRR = '%HRR',
         HRmax_minus_basal = 'HRmax-basal')

# f4d_df -------------
# Alive: ~ses_transitions/cohorts_replication/cwealth/cs_ipw for mi.R >> alive2016

ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs.RDS"))

# f4d_df <- haven::read_dta(paste0(path_gtml_earlier_data, "/META data as of 20 Jun 2017_stata12.dta")) %>% 
f4d_df <- haven::read_dta(paste0(path_gtml_earlier_data, "/ELIO all 20 Jun 2017.dta")) %>% 
  dplyr::select(iduni,starts_with("f4d"),sexo,height,weight) %>%
  mutate_at(vars(sexo), function(x) factor(x,levels=attr(x,"labels"),labels=attr(x,"labels") %>% attr(.,"names"))) %>% 
    left_join(hr_reserve %>% 
                dplyr::select(iduni,pwv_Heart_Rate,pctHRR),
              by="iduni") %>% 
  rename(visit_date = f4d,
         age = f4d1,
         fechan = f4d2,
         # Sphygmocor: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5290450/
         distance_carotid_mm = f4d3,
         distance_femoral_sleeve = f4d4_1,
         distance_femoral_mm = f4d4_2,
         sysbp = f4d5_1,
         diabp = f4d5_2,
         
         # 6MWT
         home_clock_hh = f4d6_1,
         home_clock_mm = f4d6_2,
         field_length = f4d7,
         field_turns = f4d8,
         field_extra_meters = f4d9,
         completed_distance_meters = f4d9_1,
         freq_basal = pwv_Heart_Rate,
         freq_minute1 = f4d10_1,
         freq_minute2 = f4d10_2,
         freq_minute3 = f4d10_3,
         freq_minute4 = f4d10_4,
         freq_minute5 = f4d10_5,
         freq_minute6 = f4d10_6,
         freq_minute7 = f4d10_7,
         pctHRR = pctHRR,
         completed_steps = f4d11,
         completed_distance = f4d12,
         activity_mm = f4d13_1,
         activity_ss = f4d13_2,
         person_completed_test = f4d14,
         final_test_status = f4d_efp
         ) %>% 
  mutate(activity_time = activity_mm + activity_ss/60,
         
         cadence = completed_steps/6,
         stride = (completed_distance_meters/6)/(0.5*cadence),
         
         # completed_distance_meters = field_calculated_meters*completed_distance
         ) %>% 
  dplyr::select(iduni,starts_with("field"),starts_with("completed"),starts_with("activity"),everything()) %>% 
  left_join(gtml_dfa %>% 
              dplyr::select(id_uni,adhtc3,adwtc3,adsbp,addbp,adbmi,adtgl,adhdl,gtadldlc2016,adglucose,adwc,
                            gtatole,gtchatoleexposurestatus) %>% 
              mutate(full = case_when(gtchatoleexposurestatus == "full" ~ 1,
                                      gtchatoleexposurestatus %in% c("partial","none") ~ 0,
                                      TRUE ~ NA_real_),
                     atole1000 = case_when(gtatole == 1 & full == 1 ~ 1,
                                           TRUE ~ 0)),
            by=c("iduni"="id_uni")) %>% 
  left_join(ses_masters %>% 
              dplyr::select(id_uni,pcall6775_1,pcall2016_1),
            by=c("iduni"="id_uni")) %>% 
  dplyr::filter(!is.na(pcall2016_1)) %>% 
  mutate(height_meta_hc = case_when(is.na(height) ~ adhtc3,
                            TRUE ~ height),
         stride_by_height = stride/(height_meta_hc/100),
         minute_by_km = 6/(completed_distance_meters/1000)
         ) %>% 
  
  # 0. Completed test
  dplyr::filter(person_completed_test == 1) %>% 
  
  # 1. Cadence \in [80,155]
  dplyr::filter(cadence >= 80, cadence <= 155) %>% 
  
  # 2. Valid step count and completed distance
  dplyr::filter(!is.na(completed_distance_meters),!is.na(completed_steps)) %>% 
  # 3. Stride/Height <= 1.20
  dplyr::filter(stride_by_height <= 1.20) %>% 
  
  # 4. Time per kilometer <= 17.14 min/km
  dplyr::filter(minute_by_km <= 17.14) %>% 
  
  # 5a. Values at min 5 and min 6 <= HRMax
  mutate(across(one_of(c("freq_minute5","freq_minute6")),
            function(x) case_when(x > 220 - age ~ NA_real_,
                                  TRUE ~ x))) %>% 
  dplyr::filter(!is.na(freq_minute5),!is.na(freq_minute6)
                ) %>% 
  
  # 5b. Difference between HR min6 - min 5 in [-15, 30]
  mutate(diff_5to6 = case_when(!is.na(freq_minute5) & !is.na(freq_minute6) ~ freq_minute6- freq_minute5,
                               TRUE ~ NA_real_)
  ) %>% 
  
  mutate_at(vars(one_of("diff_5to6")),function(x) case_when(x < -15 ~ NA_real_,

                                                          TRUE ~ x)) %>%
  dplyr::filter(!is.na(diff_5to6)) %>%

  mutate_at(vars(one_of("diff_5to6")),function(x) case_when(
    x > 30 ~ NA_real_,
    TRUE ~ x)) %>% 
  dplyr::filter(!is.na(diff_5to6)) %>% 
  
  # 5c. Heart rate at minute 6 minus minute 7 >= -5
  dplyr::filter(!is.na(freq_minute7))  %>% 
  dplyr::filter(freq_minute6 - freq_minute7 >= -5) %>% 
  
  # 5. % Heart rate reserve \in [25, 94.9] for minute 5 and 6
  dplyr::filter(pctHRR >= 25, pctHRR < 95) %>% 
  
  mutate(freq_peak = rowMeans(.[,c("freq_minute5","freq_minute6")],na.rm=TRUE)) %>% 
  

  

  
# table(!is.na(f4d_df$completed_steps)) Participated in 6MWT ---------
  mutate(residual_distance = lm(completed_distance_meters ~ height_meta_hc*sexo + adbmi*sexo + age*sexo) %>% 
           residuals(.),
         intensity = residual_distance %>%
           scale(.)) %>% 
  mutate(
         residual_distance_category = cut_number(residual_distance,n=3)
         ) %>% 
  mutate(baseline = freq_basal,
         change1to6 = (freq_minute6 - freq_minute1)/5,
         drop6to7 = freq_minute6 - freq_minute7)

saveRDS(f4d_df,paste0(path_incap_rally_box,"/Heart rate response/working/f4d_df.RDS"))
haven::write_dta(f4d_df,paste0(path_incap_rally_box,"/Heart rate response/shared/f4d_df_stata12.dta"),version = 12)
haven::write_dta(f4d_df,paste0(path_incap_rally_box,"/Heart rate response/shared/f4d_df_stata14.dta"),version = 14)
writexl::write_xlsx(f4d_df,paste0(path_incap_rally_box,"/Heart rate response/shared/f4d_df.xlsx"))

source(paste0(path_cohorts_repo,"/package/build_dict_custom.R"))

dictionary_file(f4d_df,type="dta")

# Summary characteristics ---------

library(compareGroups)
f4d_df %>% 
  compareGroups(sexo ~ gtatole + full + atole1000 + age + height_meta_hc + weight + adbmi + 
                  pctHRR + completed_steps + 
                  completed_distance_meters + baseline + change1to6 + 
                  freq_peak + drop6to7 + residual_distance +
                  adtgl + gtadldlc2016 + adglucose + 
                  adsbp + addbp
                  ,
                method = c(3,3,3,1,1,1,1,
                           2,1,
                           1,1,1,
                           1,1,1,
                           1,1,1,
                           1,1),data=.) %>% 
  createTable(digits =1,show.all=TRUE,show.n=TRUE,sd.type = 2,q.type=c(1,2)) %>% 
  export2xls(paste0(path_incap_rally_box,"/Heart rate response/working/table 1.xlsx"))
