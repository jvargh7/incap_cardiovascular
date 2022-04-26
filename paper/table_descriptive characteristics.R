cardio_df <- readRDS(paste0(path_incap_cardio_folder,"/working/cardio_df.RDS"))
source("preprocessing/icpaux01_variable list.R")

require(compareGroups)

compareGroups(data=cardio_df,
              formula = male ~ steps_mean + mvpa_tot_mean + mvpa_recommended + 
                residual_distance + baseline + freq_peak + drop6to7 + 
                handgrip +
                adht + moscho_imputed + 
                moage_imputed + gtatole + full + chbirtho + 
                age + adeduyr + adbmi + formal + wealth +
                TC + TG + HDLc + LDLc +
                ApoA1 + ApoB + hsCRP + Insulin + Glucose +
                NEFA + IL10 + Adiponectin + Leptin + TNFsR +
                Resistin + fast.PC1 + fast.PC2 + fast.PC3 +
                adsbp + addbp,
              method = c(1,1,3,
                         1,1,1,1,
                         1,
                         1,2,
                         2,3,3,2,
                         1,2,1,3,1,
                         1,1,1,1,
                         1,1,1,1,1,
                         1,1,1,1,1,
                         1,1,1,1,
                         1,1),include.miss = TRUE) %>% 
  createTable(.,digits=1,show.all=TRUE,q.type = c(2,2),sd.type = 2,show.n = TRUE) %>% 
  export2xls(.,file=paste0(path_incap_cardio_folder,"/working/table_descriptive characteristics.xlsx"))
