
outcome_vars <- c("TC","TG","HDLc","LDLc","ApoA1","ApoB","hsCRP",
                  "Insulin","Glucose","NEFA","IL10","Adiponectin", "Leptin",
                  "TNFsR","Resistin","fast.PC1","fast.PC2","fast.PC3","adsbp","addbp",
                  "c_aix","c_aixhr75","pwv")
covariate_vars <- c("iduni","moscho_imputed","moage_imputed","moht_imputed",
                    "gtatole","full","male","chbirtho","adeduyr","adbmi",
                    "formal","age","adht","adwt","fat","wealth","d_srq20_twoway")

exposure1_vars <- c("steps_mean","mvpa_tot_mean","steps1000_mean","mvpa_recommended")
exposure2_vars <- c("residual_distance","baseline","freq_peak","drop6to7",
                    "pctHRR","completed_steps","completed_distance_meters")
exposure3_vars <- c("handgrip")