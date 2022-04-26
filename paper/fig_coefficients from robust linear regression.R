summary_lm <- read_csv(paste0(path_incap_cardio_folder,"/working/summary_robust linear regressions.csv"))

source("preprocessing/icpaux01_variable list.R")

pdf(file = paste0(path_incap_cardio_folder,"/figures/fig_coefficients from robust linear regression.pdf"))
for(o in outcome_vars){
  fig <- summary_lm %>% 
    dplyr::filter(variable == o) %>% 
    mutate(term = factor(term,levels=c("steps1000_mean","mvpa_recommended",
                                       "residual_distance","baseline","freq_peak","drop6to7",
                                       "I(handgrip * 10)"),
                         labels = c("Steps in 1000s","MVPA of atleast 150min",
                                    "Excess distance (m)","Baseline HR","Peak HR","Recovery HR",
                                    "Grip strength in kg \nper 10kg body weight"))) %>% 
    ggplot(data=.,aes(xmin = LCI,xmax = UCI,x = estimate,y=term)) +
    geom_point() +
    geom_errorbar() +
    facet_grid(exposure~.,scales="free_y") +
    theme_bw() +
    xlab("Estimate") +
    ylab("") +
    ggtitle(o) +
    geom_vline(xintercept = 0,col="red",linetype = 2)
  print(fig)
  
  
}
dev.off()