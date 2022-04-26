cardio_df <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_df.RDS"))


library(mice)
mi_iter = 10

mi_null <- mice(cardio_df,
                maxit = 0)

method = mi_null$method
method["steps1000_mean"] = "~I(steps_mean/1000)"
method["mvpa_recommended"] = "~I(mvpa_tot_mean >= (150/7))"

# Each row corresponds to a variable block, i.e., a set of variables to be imputed. 
# A value of 1 means that the column variable is used as a predictor for the target block (in the rows). 
pred = mi_null$predictorMatrix

pred["iduni",] = 0
pred[,"iduni"] = 0

cardio_mice <- mice(cardio_df,
                 method = method,
                 pred = pred,
                 m=mi_iter,maxit=50,seed=500)

saveRDS(cardio_mice,paste0(path_incap_cardio_folder,"/working/cardio_mice.RDS"))