
# Reading datasets ----------
gtml_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "guatemala") %>% 
  # dplyr::filter(!is.na(gtadladdercommunity2018)|!is.na(gtadladdereconomic2018)) %>% 
  dplyr::mutate(id_uni = pin - 20000000) 

# Getting the 'fat' (fat %) variable here since we aren't using META raw data anywhere else
f4g_df <- haven::read_dta(paste0(path_gtml_earlier_data, "/META data as of 20 Jun 2017_stata12.dta")) %>% 
  dplyr::select(iduni,f4g1:f4g6,weight,fat) %>% 
  mutate(handgrip = case_when(is.na(weight) ~ NA_real_,
                              TRUE ~ rowMeans(.[,c("f4g3","f4g4","f4g5","f4g6")],na.rm = TRUE)/weight))

meta_age <- haven::read_dta(paste0(path_gtml_earlier_data, "/META data as of 20 Jun 2017_stata12.dta")) %>% 
  dplyr::select(iduni,fechan,fecha_censo,f1v1f1,f1v2f2,f1v3f3,f1v4f4) %>% 
  mutate(interview_date = case_when(!is.na(f1v4f4) ~ f1v4f4,
                                    !is.na(f1v3f3) ~ f1v3f3,
                                    !is.na(f1v2f2) ~ f1v2f2,
                                    !is.na(f1v1f1) ~ f1v1f1,
                                    TRUE ~ fecha_censo)) %>% 
  mutate(age = round((interview_date-fechan)/365.25) %>% as.numeric(.))

ur <- readRDS(paste0(path_incap_ses_dfa,"/urbano_rural/urbano_rural.RDS"))
incap_early_life <- haven::read_dta(paste0(path_incap_rally_box,"/Body composition and fitness paper/incap_early_life_imputed.dta"))

f4d_df <- readRDS(paste0(path_incap_rally_box,"/Heart rate response/working/f4d_df.RDS"))
f4k_df <- readRDS(paste0(path_incap_rally_box,"/Activity trajectories/working/f4k_passometer data.RDS"))
lab_data <- read_csv(paste0(path_incap_cardio_folder,"/working/incap_lab complete_pca_fasting and D_v2.csv"))
pcscore <- read_csv(paste0(path_incap_cardio_folder,"/working/PCscore_for_Jithin_2022-04-19.csv"))
meta_srq <- readRDS(paste0(path_incap_rally_box,"/SRQ-20 changes/working/meta_srq.RDS"))
arterial_stiffness <- read_dta(paste0(path_incap_cardio_folder,"/working/Master dataset arterial stiffness META - clean NO MI Set up.dta"))


# Cardio initial -----------
cardio <- f4d_df  %>% 
  dplyr::select(iduni,
                  pctHRR, completed_steps, pctHRR,
                  completed_distance_meters, baseline, change1to6, 
                  freq_peak, drop6to7, residual_distance,contains("freq")) %>% 
  full_join(f4k_df %>% 
              dplyr::select(-sexo,-height,-weight) %>% 
              mutate(mvpa_recommended = case_when(mvpa_tot_mean >= (150/7) ~ 1,
                                                  mvpa_tot_mean < (150/7) ~ 0,
                                                  TRUE ~ NA_real_),
                     steps1000_mean = steps_mean/1000),
            by="iduni") %>% 
  full_join(lab_data %>% 
              dplyr::select(iduni,TC,TG,HDLc,LDLc,ApoA1,ApoB,hsCRP,Insulin,Glucose,NEFA,IL10,Adiponectin, Leptin,
                            TNFsR,Resistin),
            by="iduni") %>% 
  full_join(pcscore %>% 
              dplyr::select(iduni,contains("fast.PC")),
            by="iduni") %>% 
  left_join(gtml_dfa %>% 
              mutate(formal = case_when(ademployment == "formal" ~ 1,
                                        ademployment %in% c("informal","unemployed") ~ 0,
                                        TRUE ~ NA_real_),
                     male = case_when(chsex == "male" ~ 1,
                                      TRUE ~ 0),
                     wealth = scale(gtadwealthindex2016)) %>% 
              dplyr::select(id_uni,adagey,adbmi,adht,adwt,adsbp,addbp,adeduyr,adsrq,male,formal,wealth),
            by=c("iduni"="id_uni")) %>% 
  left_join(f4g_df %>% 
              dplyr::select(iduni,handgrip,fat),
            by="iduni") %>% 
  left_join(incap_early_life %>% 
              mutate(full = case_when(gtchatoleexposurestatus == "full" ~ 1,
                                      TRUE ~ 0)) %>% 
              dplyr::select(id_uni,gtatole,full,moscho_imputed,moage_imputed,moht_imputed,chbirtho),
            by=c("iduni"="id_uni")) %>% 
  left_join(meta_srq %>% 
              dplyr::select(iduni,d_srq20_twoway,d_srq20_total),
            by=c("iduni")) %>% 
  left_join(meta_age %>% 
              dplyr::select(iduni,age),
            by="iduni") %>% 
  left_join(arterial_stiffness %>% 
              dplyr::select(iduni, c_aix, c_aixhr75,pwv),
            by = "iduni")

source("preprocessing/icpaux01_variable list.R")

cardio_df <- cardio %>% 
  dplyr::select(covariate_vars,outcome_vars,
                exposure1_vars,exposure2_vars,
                exposure3_vars) %>% 
  mutate_all(~as.numeric(.))

saveRDS(cardio_df,paste0(path_incap_cardio_folder,"/working/cardio_df.RDS"))
