summary_lm <- read_csv("models/icm02_robust linear regressions.csv") %>% 
  mutate(term = factor(term,levels=c("steps1000_mean","mvpa_recommended",
                                     "residual_distance","baseline","freq_peak","drop6to7",
                                     "I(handgrip * 10)"),
                       labels = c("Steps in 1000s","MVPA of atleast 150min",
                                  "Excess distance (m)","Baseline HR","Peak HR","Recovery HR",
                                  "Grip strength in kg \nper 10kg body weight")))

summary_lm %>% 
  dplyr::select(variable,term,Coefficient) %>% 
  pivot_wider(names_from=term,values_from=Coefficient) %>% 
  write_csv(.,"paper/table_coefficients from robust linear regression.csv")


