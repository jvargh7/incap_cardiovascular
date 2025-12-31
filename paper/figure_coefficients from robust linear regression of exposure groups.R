rm(list=ls());gc();source(".Rprofile")


summary_lm <- read_csv("models/icm01_robust linear regressions by exposure group.csv") %>% 
  mutate(term = case_when(term == 'relevel(exposure_group, ref = "Lsteps_Lmvpa")Hsteps_Hmvpa' ~ "High Steps High MVPA",
                          term == 'relevel(exposure_group, ref = "Lsteps_Lmvpa")Hsteps_Lmvpa' ~ "High Steps Low MVPA",
                          term == 'relevel(exposure_group, ref = "Lsteps_Lmvpa")Lsteps_Lmvpa' ~ "Low Steps Low MVPA",
                          term == 'relevel(exposure_group, ref = "Lsteps_Lmvpa")Lsteps_Hmvpa' ~ "Low Steps High MVPA"),
         variable = factor(variable,c("addbp","adsbp","Glucose",
                                      "hba1c","hsCRP","IL10",
                                      "drop6to7","pwv"),
                           labels=c("Diastolic BP (mmHg)","Systolic BP (mmHg)","Glucose (mg/dL)",
                                    "HbA1c (%)","hsCRP (unknown)","IL10 (unknown)",
                                    "Absolute Recovery min 6 to 7 (beats)","Pulse Wave Velocity (unknown)"))) %>% 
  mutate(term = factor(term,levels=c("Low Steps Low MVPA","Low Steps High MVPA","High Steps Low MVPA","High Steps High MVPA")))

(ggplot(data=summary_lm,aes(x=estimate,xmin=LCI,xmax=UCI,y=term,col=term)) +
  geom_point() +
  geom_errorbarh() +
  facet_wrap(variable ~.,scales="free_x") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept=0,col="red",linetype=2)) %>% 
  ggsave(.,filename=paste0(path_incap_cardio_folder,"/figures/coefficients from robust linear regression for exposure groups.png"),width=8,height=6)
  
