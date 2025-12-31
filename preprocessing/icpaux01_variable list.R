


outcome_vars <- c("adsbp","addbp","pwv","drop6to7",
                  "hsCRP","IL10","Glucose","hba1c"
                  )
main_exposure_vars <- c("mvpa_tot_mean","steps_mean")

covariate_vars <- c("iduni","moscho_imputed","moage_imputed","moht_imputed",
                    "gtatole","full","male","chbirtho","adeduyr","adbmi","fat_kg","ffm_kg",
                    "formal","age","adht","adwt","fat_pct","wealth","d_srq20_twoway","dmmed","htnmed")
other_lab_vars <- c("TC","TG","HDLc","LDLc","ApoA1","ApoB","Insulin","NEFA","Adiponectin", "Leptin",
                    "TNFsR","Resistin","fast.PC1","fast.PC2","fast.PC3",
                    "c_aix","c_aixhr75")

exposure1_vars <- c("mvpa_recommended")
exposure2_vars <- c("residual_distance","baseline","freq_peak","pctHRR",
                    "completed_steps","completed_distance_meters")
exposure3_vars <- c("handgrip")