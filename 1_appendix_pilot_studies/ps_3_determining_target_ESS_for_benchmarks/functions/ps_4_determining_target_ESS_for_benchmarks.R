

# -- | ------------------------- Target-ESS pilot study stuff ---------------------------------------------------------------------------------------------------------------------




{
  
  
  DGP = 5
  M_dense = TRUE
  metric_type = "Hessian"
  
  n_burnin_vec = c(250)
  learning_rate_main_vec = c(0.10)
  adapt_interval_width_vec = c(25)
  u_Euclidean_metric_const_vec =  FALSE # c(TRUE, FALSE)
  adapt_M_us_vec = TRUE #  c(TRUE, FALSE)
  adapt_delta_vec =  c(0.80) # c(0.65, 0.80)
  
  N_vec = c(500, 1000, 2500, 5000, 12500, 25000)
  n_chains_vec =  c(32) 
  n_chains_burnin_vec =  c(16) 
  device_vec <- c("HPC")
  
  Phi_type_vec <- c(1)
  
  outs_list_to_plot <- list()
  
  start_runs <- 1
  n_runs <- 10
  
  
  
  
  counter <- 1
  
}







{
  
  stuff_rds_list_of_lists <- list()
  
  counter <- 1
  
  for (r in 1:length(device_vec)) { 
    for (o in 1:length(N_vec)) {
      for (n in 1:length(n_chains_vec)) {
        for (i in 1:length(n_burnin_vec)) {
          for (m in 1:length(adapt_delta_vec)) {
            for (j in 1:length(learning_rate_main_vec)) {
              for (p in 1:length(adapt_interval_width_vec)) {
                for (s in 1:length(Phi_type_vec)) {
                  for (k in 1:length(u_Euclidean_metric_const_vec)) {
                    for (l in 1:length(adapt_M_us_vec)) {
                      #  for (q in 1:length(n_iter_vec)) {
                      for (t in 1:length(n_chains_burnin_vec)) {
                        
                        
                        n_chains = n_chains_vec[n]
                        
                        
                        N = N_vec[o]
                        
                        if (n_chains < 17) { 
                          n_iter =  4000
                        } else if (n_chains %in% c(18:32)) { 
                          if (N < 5001) n_iter =  8000  
                          else          n_iter =  2000  
                        } else { 
                          n_iter =  1000  
                        }
                        
                        
                        {
                          print(paste0("N = ",  N_vec[o]))
                          print(paste0("adapt_interval_width_vec = ",  adapt_interval_width_vec[p]))
                          print(paste0("n_iter_vec = ",  n_iter))
                          print(paste0("device_vec = ", device_vec[r]))
                          print(paste0("Phi_type = ",  Phi_type_vec[s]))
                        }
                        
                        try({  
                          
                          Phi_type =  Phi_type_vec[s]
                          N = N_vec[o]
                          n_chains_burnin = n_chains_burnin_vec[t] 
                          
                          ###
                          {
                            
                            file_name <- paste0(#"seed", df_i,"_", 
                              "DGP", DGP,"_", 
                              "N", N_vec[o],"_", 
                              "n_chains", n_chains_vec[n],"_", 
                              "burn", n_burnin_vec[i],"_", 
                              "iter", n_iter,"_", 
                              "AD", adapt_delta_vec[m],"_", 
                              "LR", learning_rate_main_vec[j],"_", 
                              "width", adapt_interval_width_vec[p],"_",
                              # "Dense", M_dense,"_",
                              "U_const",  u_Euclidean_metric_const_vec[k], "_",
                              "U_M", adapt_M_us_vec[l], "_",  
                              "M_type_", metric_type,"_",
                              "Phi_type_", Phi_type_vec[s]
                            )
                            
                            if (parallel::detectCores() < 17) { 
                              file_name <- paste0("Laptop_", file_name) 
                            } 
                            
                            file_name <- paste0( file_name, "_", "n_ch_burn", n_chains_burnin_vec[t] )
                            file_name <- paste0( file_name, "_", "reps", n_runs - start_runs + 1 )
                            
                            file_name = paste0("Info_", file_name)
                            
                            
                            
                            if (parallel::detectCores() < 17) { 
                              stuff_rds <- readRDS(    file_name)  
                            } else { 
                              stuff_rds <- readRDS(   file_name)
                            }
                            
                            # run_index <- 1
                            # str(
                            #   stuff_rds[[1]][[run_index]]$trace_individual_log_lik_array 
                            #   )
                            
                            for (run_index in 1:n_runs) {
                              stuff_rds[[1]][[run_index]]$trace_individual_log_lik_array <- 0 
                            }
                            
                            
                            stuff_rds_list_of_lists[[counter]] <- stuff_rds 
                            
                            
                            ###  model_outs_list_of_lists[[counter]] <- stuff_rds[[1]]
                            
                          }
                        })
                        
                        counter = counter + 1
                        
                        gc(reset = TRUE)
                        
                      }
                    }
                  }
                  #     }
                }
              }
            }
          }
        }
      }
    }
    
  }
}











{
  
  
  
  DGP = 5
  M_dense = TRUE
  metric_type = "Hessian"
  
  # n_iter_vec <- c(1000)
  n_burnin_vec = c(250)
  learning_rate_main_vec = c(0.10)
  adapt_interval_width_vec = c(25)
  u_Euclidean_metric_const_vec =  FALSE # c(TRUE, FALSE)
  adapt_M_us_vec = TRUE #  c(TRUE, FALSE)
  adapt_delta_vec =  c(0.80) # c(0.65, 0.80)
  
  N_vec = c(500, 1000, 2500, 5000, 12500, 25000)
  n_chains_vec =  c(32) 
  n_chains_burnin_vec =  c(16) 
  device_vec <- c("HPC")
  
  Phi_type_vec <- c(1)
  
  outs_list_to_plot <- list()
  
  start_runs <- 1
  n_runs <- 10
  
  Se_ests <- array(dim = c(n_runs, n_tests))
  Sp_ests <- array(dim = c(n_runs, n_tests))
  prev_ests <- c()
  
  
  df_target_ESS_pilot_study <- tibble(DGP = DGP, 
                                      N =  N ,    
                                      algorithm = "HI_HMC",  #
                                      n_runs = 5,
                                      device =   "HPC",
                                      n_chains = 64, 
                                      n_chains_burnin  =  NA,
                                      n_iter = 50,
                                      metric_type = "Hessian", 
                                      M_dense = M_dense, 
                                      n_burn = 500, LR = 0.05, adapt_int_width = 25, AD = 0.80 , U_M_const = FALSE, adapt_M_us = TRUE,
                                      ##
                                      max_Rhat = NA_real_,
                                      max_nested_Rhat = NA_real_,
                                      ##
                                      ESS = NA_real_ ,
                                      ##
                                      ESS_per_sec_samp_mean =   NA_real_,    ESS_per_sec_samp_SD =   NA_real_, 
                                      ESS_per_sec_mean =    NA_real_,   ESS_per_sec_SD =   NA_real_,
                                      ESS_per_grad_samp_mean =   NA_real_, ESS_per_grad_samp_SD =  NA_real_,
                                      ESS_per_grad_mean =    NA_real_,     ESS_per_grad_SD =      NA_real_,
                                      ##
                                      time_100_ESS_mean = NA_real_,    time_100_ESS_SD =     NA_real_,
                                      time_1000_ESS_mean = NA_real_,   time_1000_ESS_SD =     NA_real_,
                                      ##
                                      grad_evals_1000_ESS_mean = NA_real_,  grad_evals_1000_ESS_SD = NA_real_, 
                                      ##
                                      L_burnin = NA_real_, 
                                      L_sampling = NA_real_ , 
                                      ##
                                      Phi_type = NA_real_,
                                      n_iter_test = NA_real_, 
                                      n_divs_pct = NA_real_,
                                      ##
                                      min_ESS_test =  NA_real_,
                                      min_ESS_target_test = NA_real_,
                                      ##
                                      max_IQR = NA_real_,
                                      max_IQR_index =NA_real_,
                                      max_Range = NA_real_,
                                      max_Range_index = NA_real_, 
                  
  
  
  
  
  counter_2 <- 1
  
}













for (t in 1:n_tests) {
  iqr_vec[counter] <-   iqr(   Sp_ests[,t]  ) 
  iqr_vec_lower[counter] <- c(summary(Sp_ests[,t])[2])
  iqr_vec_upper[counter] <- c(summary(Sp_ests[,t])[5])
  print(paste0("IQR of Sp", t, " = ", iqr(   Sp_ests[,t]  )  ))  
  counter <- counter + 1


iqr_vec[counter] <-   iqr(  prev_ests  ) 
iqr_vec_lower[counter] <- c(summary(prev_ests)[2])
iqr_vec_upper[counter] <- c(summary(prev_ests)[2])
print(paste0("IQR of prev", t, " = ",  iqr(  prev_ests  )  ))  

print(paste0("max IQR = ", max(iqr_vec) ))
}

counter <- 1
{
  for (t in 1:n_tests) {
    range_vec[counter] <-   range(   Se_ests[,t]  )[2] - range(   Se_ests[,t]  )[1]
    range_vec_lower[counter] <- c(range(Se_ests[,t])[1])
    range_vec_upper[counter] <- c(range(Se_ests[,t])[2])
    print(paste0("Range of Se", t, " = ",     range_vec[counter] ) )
    counter <- counter + 1
  }
  for (t in 1:n_tests) {
    range_vec[counter] <-   range(   Sp_ests[,t]  )[2] - range(   Sp_ests[,t]  )[1]
    range_vec_lower[counter] <- c(range(Sp_ests[,t])[1])
    range_vec_upper[counter] <- c(range(Sp_ests[,t])[2])
    print(paste0("Range of Sp", t, " = ",    range_vec[counter]  ))
    counter <- counter + 1
  }
  
  range_vec[counter] <-   range(   prev_ests  )[2] - range(   prev_ests   )[1]
  range_vec_lower[counter] <- c(range(prev_ests)[1])
  range_vec_upper[counter] <- c(range(prev_ests)[2])
  print(paste0("Range of prev", t, " = ",    range_vec[counter]   ))
  
  print(paste0("max Range = ", max(range_vec) ))
}



print(paste("|----------------------------------------|"))

print(paste0("max IQR (lower value) = ", round(           iqr_vec_lower[which(iqr_vec == max(iqr_vec))]  , 3)))
print(paste0("max IQR (upper value) = ", round(           iqr_vec_upper[which(iqr_vec == max(iqr_vec))]  , 3)))

print(paste0("max Range (lower value) = ",  round(           iqr_vec_lower[which(range_vec == max(range_vec))]  , 3)))
print(paste0("max Range (upper value) = ",  round(           iqr_vec_upper[which(range_vec == max(range_vec))]  , 3)))

print(paste("|----------------------------------------|"))


print(paste("|----------------------------------------|"))
print(paste0("max Range = ", round(max(range_vec), 3)))
print(paste0("max Range index = ", which(range_vec == max(range_vec))))
print(paste0("max IQR = ",  round(max(iqr_vec), 3)))
print(paste0("max IQR index = ",  which(iqr_vec == max(iqr_vec))))  
print(paste("|----------------------------------------|"))


min_ESS_val <- (round(mean(min_ess_vec), 0))
ESS_diff_from_target <- abs(ESS_target - min_ESS_val)
ESS_diff_from_target_pct <- ( ESS_diff_from_target / ESS_target ) * 100


print(paste("|----------------------------------------|"))
# print(paste("ESS_diff_from_target_pct = ",   ESS_diff_from_target_pct ))

if (n_iter_test != n_iter) {
  if (ESS_diff_from_target_pct %in% c(5:10)) { 
    cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "yellow"), "\n")
  } else  if (ESS_diff_from_target_pct %in% c(0:4.999)) { 
    cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "darkgreen"), "\n")
  } else { 
    cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "red"), "\n")
  }
  
}

print(paste("vec_min_ESS = ",   min_ESS_val ))
print(paste("N = ", mean(N)))
print(paste("n_iter = ", mean(n_iter)))
print(paste("n_iter_test = ", mean(n_iter_test)))
print(paste("Phi_type = ", Phi_type))
print(paste("n_divs (%) = ", round( 100 * mean(vec_divs / (n_iter_test * n_chains) ), 4) ))
print(paste("|----------------------------------------|"))
}
})


try({   
  {
    df_row <-  tibble(DGP = DGP, 
                      N = N_vec[o] ,    
                      algorithm = "HI_HMC",  
                      n_runs =      as.numeric(strsplit( stuff_rds[[39]], "[=]")[[1]][2]),
                      device = device_vec[r],
                      n_chains = n_chains_vec[n], 
                      n_chains_burnin  =  n_chains_burnin_vec[t],
                      n_iter = n_iter,
                      metric_type = metric_type, 
                      M_dense = M_dense, 
                      n_burn = n_burnin_vec[i], 
                      LR = learning_rate_main_vec[j], 
                      adapt_int_width = adapt_interval_width_vec[p], 
                      AD =  adapt_delta_vec[m] , 
                      U_M_const = u_Euclidean_metric_const_vec[k], 
                      adapt_M_us = adapt_M_us_vec[l], 
                      ESS_per_sec_samp_mean =   round( as.numeric(    strsplit(  stuff_rds[[22]], "[ing =]")[[1]][10]   ) , 3),   
                      ESS_per_sec_samp_SD =   round( as.numeric(    strsplit(stuff_rds[[22]], "[ing =]")[[1]][11]  ) , 3),    
                      ESS_per_sec_mean =     round( as.numeric(    strsplit(stuff_rds[[21]], "[all =]")[[1]] [8] ) , 3),    
                      ESS_per_sec_SD =  round( as.numeric(    strsplit(stuff_rds[[21]], "[all =]")[[1]] [9] ) , 3), 
                      ESS_per_grad_samp_mean = round( min_ESS_per_grad_samp_test_mean, 3),   
                      ESS_per_grad_samp_SD =  round( min_ESS_per_grad_samp_test_SD , 3),  
                      ESS_per_grad_mean =    round( as.numeric(    strsplit(stuff_rds[[23]], "[all =]")[[1]][9] ) , 3),    
                      ESS_per_grad_SD =  round( as.numeric(    strsplit(stuff_rds[[23]], "[all =]")[[1]][10]  ) , 3), 
                      time_100_ESS_mean = round( as.numeric(   strsplit(stuff_rds[[25]], "[_ESS = ]")[[1]][[11]] ) , 3),      
                      time_100_ESS_SD = round( as.numeric(   strsplit(stuff_rds[[25]], "[_ESS = ]")[[1]][[12]] ) , 3), 
                      time_1000_ESS_mean =round( as.numeric(    strsplit(stuff_rds[[26]], "[_ESS = ]")[[1]][[11]] ) , 3),       
                      time_1000_ESS_SD = round( as.numeric(   strsplit(stuff_rds[[26]], "[_ESS = ]")[[1]][[12]] ) , 3), 
                      grad_evals_1000_ESS_mean =  round( as.numeric(    strsplit( stuff_rds[[39]], split = "\\s+" )[[1]][1]   ) , 3), 
                      grad_evals_1000_ESS_SD = round( as.numeric(   strsplit( stuff_rds[[39]], split = "\\s+" )[[1]][2]  ) , 3), 
                      L_burnin =  round( as.numeric(    strsplit(stuff_rds[[14]], "[=]")[[1]][2]  ) , 3), 
                      L_sampling =  round( as.numeric(        strsplit(stuff_rds[[15]], "[=]")[[1]][2]  ) , 3), 
                      ESS = round( as.numeric(    strsplit(stuff_rds[[16]], "[=]")[[1]][2]  ) , 0), 
                      Phi_type = Phi_type_vec[s] ,
                      n_iter_test = n_iter_test, 
                      n_divs_pct = 100 *  round(mean(vec_divs / (n_iter_test * n_chains) ), 4) ,
                      min_ESS_test =  (round(mean(min_ess_vec), 0)) ,
                      min_ESS_target_test = ESS_target,
                      max_IQR = round(max(iqr_vec), 3) ,
                      max_IQR_index =  which(iqr_vec == max(iqr_vec)) ,
                      max_Range = round(max(range_vec), 3) ,
                      max_Range_index =  which(range_vec == max(range_vec)) , 
                      max_Rhat =  round(max(rhat_vec), 4) ,
                      max_nested_Rhat =  round(max(rhat_nested_vec), 4) 
    )
    
    
    
    
    
    print(df_row)
    try({  
      df_target_ESS_pilot_study <- rbind(df_target_ESS_pilot_study, df_row)
    })
    
    print(df_target_ESS_pilot_study)
  }
})






}  ### end of "for (ii in 1:length(ESS_target_vec))"




})


counter_2 = counter_2 + 1



}
}
}
#   }
}
}
}
}
}
}
}
}




}      










try({ 
  {
    
    
    
    
    df_target_ESS_pilot_study %>% dplyr::select(N, 
                                                LR,
                                                n_burn, 
                                                Phi_type,
                                                min_ESS_target_test, 
                                                min_ESS_test, 
                                                n_iter_test, 
                                                n_divs_pct, 
                                                max_IQR,
                                                max_IQR_index,
                                                max_Range,
                                                max_Range_index)   
    
    #                                                      #  dplyr::filter(ESS_target_within_10_pct  == 1) %>%  ####  
    
    
    df_target_ESS_pilot_study_simplified_subset <- df_target_ESS_pilot_study %>% dplyr::select(N, LR, n_burn, 
                                                                                               Phi_type,
                                                                                               min_ESS_target_test, 
                                                                                               min_ESS_test, 
                                                                                               n_iter_test, 
                                                                                               n_divs_pct, 
                                                                                               max_IQR,
                                                                                               max_IQR_index,
                                                                                               max_Range,
                                                                                               max_Range_index,
                                                                                               ESS_per_grad_samp_mean,
                                                                                               ESS_per_grad_samp_SD,
                                                                                               ESS_per_sec_samp_mean,
                                                                                               ESS_per_sec_samp_SD,
                                                                                               max_Rhat,
                                                                                               max_nested_Rhat
    ) %>%
      dplyr::mutate(ESS_target_m_actual = abs(min_ESS_target_test - min_ESS_test) ) %>%
      dplyr::mutate(ESS_target_within_5_pct  =  ifelse(  abs(ESS_target_m_actual / min_ESS_test ) * 100 < 5, 1, 0)) %>%
      dplyr::mutate(ESS_target_within_10_pct =  ifelse(  abs(ESS_target_m_actual / min_ESS_test ) * 100 < 10 , 1, 0)) %>%
      dplyr::mutate(n_iter_test_2 =  round( (min_ESS_target_test / min_ESS_test ) * n_iter_test, 0) ) %>% 
      dplyr::arrange(Phi_type, N) %>% dplyr::select(N, 
                                                    # LR, 
                                                    # n_burn, 
                                                    #   n_chains,
                                                    #  Phi_type,
                                                    min_ESS_target_test, 
                                                    min_ESS_test, 
                                                    n_iter_test, 
                                                    n_iter_test_2,
                                                    n_divs_pct, 
                                                    max_IQR,
                                                    max_IQR_index,
                                                    max_Range,
                                                    max_Range_index,
                                                    ESS_target_within_5_pct,
                                                    ESS_target_within_10_pct,
                                                    ESS_per_grad_samp_mean,
                                                    ESS_per_grad_samp_SD,
                                                    ESS_per_sec_samp_mean,
                                                    ESS_per_sec_samp_SD,
                                                    max_Rhat,
                                                    max_nested_Rhat
      ) # %>%
    #dplyr::filter(n_iter_test < n_iter) 
    
    
    df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 3) %>%    print(n = 1000) 
    df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 1) %>%    print(n = 1000)
    # 
    # df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 3) %>%  
    #   select(N, n_iter_test, n_iter_test_2)  %>%  
    # #  arrange(N, -min_ESS_target_test) %>% 
    #   print(n = 1000) 
    
    df_target_ESS_pilot_study_simplified_subset %>%    
      filter(Phi_type == 1) %>% 
      #  select(N, n_iter_test, n_iter_test_2)  %>% 
      arrange(N, -min_ESS_target_test) %>%  
      print(n = 1000)
    
    
    
  }
})














{
  
  
  
  df_target_ESS_pilot_study_2  <-  df_target_ESS_pilot_study %>%    dplyr::mutate(ESS_target_m_actual = abs(min_ESS_target_test - min_ESS_test) ) %>%
    dplyr::mutate(ESS_target_within_5_pct =  ifelse( (ESS_target_m_actual / min_ESS_test ) * 100 < 5, 1, 0)) %>%
    dplyr::mutate(n_iter_test_2 =  round( (min_ESS_target_test / min_ESS_test ) * n_iter_test, 0) ) 
  
  
  df_target_ESS_pilot_study_3 <- df_target_ESS_pilot_study_2 %>%   dplyr::filter(ESS_target_within_5_pct  == 1) 
  
  
  
  df_target_ESS_pilot_study_3$adapt_int_width <- factor(df_target_ESS_pilot_study_3$adapt_int_width)
  df_target_ESS_pilot_study_3$Phi_type <- factor(df_target_ESS_pilot_study_3$Phi_type)
  
  df_target_ESS_pilot_study_3 <- dplyr::mutate(df_target_ESS_pilot_study_3, N_label =  paste0("N = ",  df_target_ESS_pilot_study_3$N))
  df_target_ESS_pilot_study_3 <- dplyr::mutate(df_target_ESS_pilot_study_3, N_chains_label =  paste0("N_chains = ",  df_target_ESS_pilot_study_3$n_chains))
  
  df_target_ESS_pilot_study_3$N_label <- factor( df_target_ESS_pilot_study_3$N_label )
  
  df_target_ESS_pilot_study_3$N_label <- factor(df_target_ESS_pilot_study_3$N_label,  levels = c("N = 500", "N = 1000", "N = 2500", "N = 5000", "N = 12500", "N = 25000"))
  # df_target_ESS_pilot_study_3$N_label 
  
  df_target_ESS_pilot_study_3$N_chains_label <- factor(df_target_ESS_pilot_study_3$N_chains_label, levels = c( "N_chains = 4", "N_chains = 8", "N_chains = 16", "N_chains = 32", "N_chains = 64" ))
  # df_target_ESS_pilot_study_3$N_chains_label 
  
  
  
  df_target_ESS_pilot_study_3$min_ESS_target_test <- factor(df_target_ESS_pilot_study_3$min_ESS_target_test )
  
  
  df_target_ESS_pilot_study_3 <- df_target_ESS_pilot_study_3 %>% dplyr::mutate(
    # target_ESS = case_when(
    #   N == 500   ~ 1000, 
    #   N == 1000  ~ 1000, 
    #   N == 2500  ~ 1000,
    #   N == 5000  ~ 1000,
    #   N == 12500 ~ 500, 
    #   N == 25000 ~ 500),
    # N_iter_target_ESS_64_ch =   round((min_ESS_target_test/ESS) * n_iter *    (n_chains / 64)  , 0), 
    # N_iter_target_ESS_8_ch =   round((min_ESS_target_test/ESS) * n_iter *    (n_chains / 8)  , 0), 
    N_iter_target_ESS_for_chosen_N_ch = round( ( as.numeric(as.character(min_ESS_target_test))/ESS) * n_iter , 0), 
    time_sampling = ESS * as.numeric(as.character(1 / as.numeric(as.character(ESS_per_sec_samp_mean)) ) ), 
    time_total   =  ESS * as.numeric(as.character(1 / as.numeric(as.character(ESS_per_sec_mean)) ) ),
    time_burnin = (time_total - time_sampling) / 60,
    time_sampling =  ( time_sampling * (min_ESS_test / ESS) ) / 60,
    time_total   =  (time_burnin + time_sampling) ,
    time_target_ESS_mean =  time_burnin +   time_sampling * ( as.numeric(as.character(min_ESS_target_test)) / min_ESS_test )   ,
    time_target_ESS_1000_ESS_ratio = time_target_ESS_mean / as.numeric(as.character(time_1000_ESS_mean)),
    time_target_ESS_SD = as.numeric(as.character(time_1000_ESS_SD)) * time_target_ESS_1000_ESS_ratio
  )
  
  
  
  
}



# plots


{
  
  plot_pilot_study_target_ESS_plot_IQR <-  ggplot(data = df_target_ESS_pilot_study_3, 
                                                  aes(x = min_ESS_target_test, 
                                                      y =  max_IQR, 
                                                      colour = Phi_type, 
                                                      shape =  adapt_int_width) ) +
    geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_IQR), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    theme_bw(base_size = 16) + 
    facet_wrap( ~   N_label  ,
                scales = "free", 
                ncol = 2)  + 
    ylab("Max IQR") + 
    xlab("Min ESS") + 
    theme(legend.position = "bottom")  + 
    #scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    geom_hline(yintercept = 0.25,  linetype="dashed", color = "red", size = 1)#  + 
  #  geom_hline(yintercept = 0.40,  linetype="dashed", color = "red", size = 1) 
  
  plot_pilot_study_target_ESS_plot_IQR
  
}



{
  
  plot_pilot_study_target_ESS_plot_Range <-  ggplot(data = df_target_ESS_pilot_study_3, 
                                                    aes(x = min_ESS_target_test, 
                                                        y =  max_Range, 
                                                        colour = Phi_type, 
                                                        shape =  adapt_int_width) ) +
    geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_Range), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    theme_bw(base_size = 16) + 
    facet_wrap( ~   N_label  ,
                scales = "free", 
                ncol = 2)  + 
    ylab("Max Range") + 
    xlab("Min ESS") + 
    theme(legend.position = "bottom")  + 
    # scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
    geom_hline(yintercept = 0.75,  linetype="dashed", color = "red", size = 1) # + 
  #  geom_hline(yintercept = 0.80,  linetype="dashed", color = "red", size = 1) 
  
  plot_pilot_study_target_ESS_plot_Range 
  
  
}





plot_pilot_study_target_ESS_plot_IQR +
  plot_pilot_study_target_ESS_plot_Range 


{
  png("Figure_target_ESS_pilot_study_1.png" ,units = "in", width = 16, height=9, res=800)
  plot_pilot_study_target_ESS_plot_IQR +
    plot_pilot_study_target_ESS_plot_Range 
  dev.off()
}




{
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_1 <- mutate(df_target_ESS_pilot_study_3, value = max_Range, Type = "max(Range)")
  df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_2 <- mutate(df_target_ESS_pilot_study_3, value = max_IQR, Type = "max(IQR)")
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot <-  rbind(df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_1, 
                                                           df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_2)
  
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot  %>% select(              N,        min_ESS_test, 
                                                                           n_iter_test, 
                                                                           n_iter_test_2,
                                                                           n_divs_pct, 
                                                                           max_IQR,
                                                                           max_IQR_index,
                                                                           max_Range,
                                                                           max_Range_index,
                                                                           ESS_target_within_5_pct,
                                                                           #  ESS_target_within_10_pct,
                                                                           #  ESS_per_grad_samp_mean,
                                                                           #  ESS_per_grad_samp_SD,
                                                                           #   ESS_per_sec_samp_mean,
                                                                           #   ESS_per_sec_samp_SD,
                                                                           # max_Rhat,
                                                                           max_nested_Rhat) %>% print(n  = 1000)
  
  
  plot_pilot_study_target_ESS_plot_Range_and_IQR <-  ggplot(data = df_target_ESS_pilot_study_3_stacked_for_ggplot, 
                                                            aes(x = min_ESS_target_test, 
                                                                y =  value, 
                                                                # colour = Phi_type, 
                                                                colour =  Type) ) +
    geom_point(size = 3, aes(x =  min_ESS_target_test, y = value), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    #geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_IQR),   position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    theme_bw(base_size = 16) + 
    facet_wrap( ~   N_label  ,
                scales = "free", 
                ncol = 2)  + 
    ylab("Max Range (blue) and IQR (red)") + 
    xlab("Min ESS") + 
    theme(legend.position = "bottom")  + 
    # scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
    geom_hline(yintercept = 0.50,  linetype="dashed", color = "red", size = 0.5) + 
    geom_hline(yintercept = 0.75,  linetype="dashed", color = "skyblue", size = 0.5) 
  
  plot_pilot_study_target_ESS_plot_Range_and_IQR 
  
  
}


{
  png("Figure_target_ESS_pilot_study_2.png" ,units = "in", width = 4*3, height=3*3, res=800)
  plot_pilot_study_target_ESS_plot_Range_and_IQR
  dev.off()
}








{
  
  # get "final" dataset which has Phi_type and target_ESS to use for each N. 
  {
    
    
    
    
    print(df_target_ESS_pilot_study_3, n = 1000)
    
    df_target_ESS_pilot_study_4 <- df_target_ESS_pilot_study_3 %>%
      dplyr::mutate(Accept_target_ESS = ifelse((max_IQR < max_IQR_limit) & (max_Range < max_range_limit),   ##############################
                                               1, 0)) %>% 
      dplyr::filter(Accept_target_ESS == 1) %>%
      dplyr::arrange(N, Phi_type, min_ESS_target_test)
    
    print(df_target_ESS_pilot_study_4, n = 1000)
    
    
    
    df_target_ESS_pilot_study_5 <- df_target_ESS_pilot_study_4[1,]
    
    
    N_vec <- unique(df_target_ESS_pilot_study_4$N)
    
    try({  
      for (i in 1:length(N_vec)) {
        for (j in 1:length(Phi_type_vec)) {
          
          
          try({  
            df_target_ESS_pilot_study_subset <-   df_target_ESS_pilot_study_4 %>% 
              dplyr::filter(N == N_vec[i])  %>% 
              filter(Phi_type == Phi_type_vec[j])  
          })
          
          try({  
            accept_indicators_reverse <- rev(df_target_ESS_pilot_study_subset$Accept_target_ESS)
          })
          
          try({  
            counter = 0
            for (ii in 1:nrow(df_target_ESS_pilot_study_subset)) { 
              if (accept_indicators_reverse[ii] == 1) { 
                counter = counter + 1
              } else { 
                break
              }
              index_of_chosen_ESS = nrow(df_target_ESS_pilot_study_subset) - counter + 1
            }
          })
          
          
          
          try({ 
            df_target_ESS_pilot_study_5 <-   rbind(df_target_ESS_pilot_study_5,   
                                                   df_target_ESS_pilot_study_subset[index_of_chosen_ESS,])
          })
          
        }
      }
    })
    
    df_target_ESS_pilot_study_6 <- df_target_ESS_pilot_study_5[-1, ]
    
    
    
    
    df_target_ESS_pilot_study_6_less_variables <-      df_target_ESS_pilot_study_6  %>% dplyr::select(N, LR, n_burn, 
                                                                                                      Phi_type,
                                                                                                      min_ESS_target_test, 
                                                                                                      min_ESS_test, 
                                                                                                      n_iter_test, 
                                                                                                      n_divs_pct, 
                                                                                                      max_IQR,
                                                                                                      max_IQR_index,
                                                                                                      max_Range,
                                                                                                      max_Range_index, 
                                                                                                      time_total,
                                                                                                      time_sampling,
                                                                                                      time_target_ESS_mean,
                                                                                                      ESS_per_sec_samp_mean, 
                                                                                                      ESS_per_grad_samp_mean,
                                                                                                      ESS_per_grad_samp_SD,
                                                                                                      ESS_per_sec_samp_SD, 
                                                                                                      max_nested_Rhat,
                                                                                                      max_Rhat)
    
    
    
    
    
    df_target_ESS_pilot_study_6_less_variables  %>%    dplyr::filter(Phi_type == 1)  
    df_target_ESS_pilot_study_6_less_variables  %>%    dplyr::filter(Phi_type == 3)  
    
    
    
    
    df_target_ESS_pilot_study_4_subset_Phi_type_3 <- df_target_ESS_pilot_study_6_less_variables %>%     
      dplyr::filter(Phi_type == 3)    %>% print()
    
    
    df_target_ESS_pilot_study_4_subset_Phi_type_1 <- df_target_ESS_pilot_study_6_less_variables %>%     
      dplyr::filter(Phi_type == 1)    %>% print()
    
    
    
    df_target_ESS_pilot_study_4_subset_Phi_type_1_selected_N <- df_target_ESS_pilot_study_4_subset_Phi_type_3 %>%    
      dplyr::filter(N %in% c(500, 
                             1000,
                             2500,
                             5000
                             #  12500,
                             #25000
      ))  
    
    
    df_target_ESS_pilot_study_4_subset_Phi_type_3_selected_N <- df_target_ESS_pilot_study_4_subset_Phi_type_1 %>%     
      dplyr::filter(N %in% c(0, 
                             12500,
                             25000
      ))   
    
    
    
    
    
    df_target_ESS_pilot_study_4_subset_FINAL <- rbind(df_target_ESS_pilot_study_4_subset_Phi_type_1_selected_N, 
                                                      df_target_ESS_pilot_study_4_subset_Phi_type_3_selected_N)
    
    
    
  }
  
  
  
  
  
  
  {
    df_target_ESS_pilot_study_4_subset_Phi_type_3   %>% dplyr::select(N, 
                                                                      #LR,
                                                                      #n_burn, 
                                                                      Phi_type,
                                                                      min_ESS_target_test, 
                                                                      min_ESS_test, 
                                                                      n_iter_test, 
                                                                      n_divs_pct, 
                                                                      max_IQR,
                                                                      #  max_IQR_index,
                                                                      max_Range,
                                                                      # max_Range_index, 
                                                                      #  time_total,
                                                                      #  time_sampling,
                                                                      ESS_per_grad_samp_mean,
                                                                      ESS_per_grad_samp_SD,
                                                                      max_nested_Rhat,
                                                                      max_Rhat) %>% print()
    
    df_target_ESS_pilot_study_4_subset_Phi_type_1  %>% dplyr::select(N, 
                                                                     #LR,
                                                                     #n_burn, 
                                                                     Phi_type,
                                                                     min_ESS_target_test, 
                                                                     min_ESS_test, 
                                                                     n_iter_test, 
                                                                     n_divs_pct, 
                                                                     max_IQR,
                                                                     # max_IQR_index,
                                                                     max_Range,
                                                                     #  max_Range_index, 
                                                                     #   time_total,
                                                                     #   time_sampling,
                                                                     ESS_per_grad_samp_mean,
                                                                     ESS_per_grad_samp_SD,
                                                                     max_nested_Rhat,
                                                                     max_Rhat) %>% print()
    # 
    # df_target_ESS_pilot_study_4_subset_FINAL      %>% dplyr::select(N,
    #                                                                 #LR,
    #                                                                 #n_burn, 
    #                                                                 Phi_type,
    #                                                                 min_ESS_target_test, 
    #                                                                 min_ESS_test, 
    #                                                                 n_iter_test, 
    #                                                                 n_divs_pct, 
    #                                                                 max_IQR,
    #                                                               #  max_IQR_index,
    #                                                                 max_Range,
    #                                                               #  max_Range_index, 
    #                                                              #   time_total,
    #                                                              #   time_sampling,
    #                                                               ESS_per_grad_samp_mean,
    #                                                               ESS_per_grad_samp_SD,
    #                                                                 max_nested_Rhat,
    #                                                                 max_Rhat)  %>% print()
    
    print(paste("max_IQR_limit = ", max_IQR_limit ))
    print(paste("max_range_limit = ", max_range_limit ))
    
    
    
  }
  
  
}







# 
#   max_range_limit <-  1.00 ;     max_IQR_limit   <-  0.50
# # max_range_limit <-  1.00 ;     max_IQR_limit   <-  0.45
# # max_range_limit <-  0.80 ;     max_IQR_limit   <-  0.50
# #      max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.50
# #  max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
#   max_range_limit <-  0.50 ;     max_IQR_limit   <-  0.25
# 
# #  max_range_limit <-  0.70 ;     max_IQR_limit   <-  0.35
#    max_range_limit <-  0.60 ;     max_IQR_limit   <-  0.35
# 
# 
# #  max_range_limit <-  0.70 ;     max_IQR_limit   <-  0.25
max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.5

# 
# 
#  max_range_limit <-  0.80 ;     max_IQR_limit   <-  0.40


#  max_range_limit <-  0.70  ;     max_IQR_limit   <-  0.25
#   max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
#  #  max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.20


#   max_range_limit <-  0.60  ;     max_IQR_limit   <-  0.30



{ 
  
  df_target_ESS_pilot_study_simplified_subset %>%   
    filter(Phi_type == 1) %>%   
    mutate(cond_satisfied = ifelse( (max_IQR < max_IQR_limit) & (max_Range < max_range_limit), 1, 0)) %>%
    select(- ESS_target_within_5_pct, - n_iter_test_2, - Phi_type)        %>%  
    print(n = 1)  
  
  df_target_ESS_pilot_study_simplified_subset_2 <-  df_target_ESS_pilot_study_simplified_subset   %>%   
    mutate(cond_satisfied = ifelse( (max_IQR < max_IQR_limit) & (max_Range < max_range_limit), 1, 0)) %>% 
    group_by(N)  %>%
    arrange(N, - min_ESS_target_test) %>%
    mutate(cond_2 = cumsum(cond_satisfied)) %>% 
    slice(seq_len(min(which(cond_satisfied == 0)))) %>%
    select(- ESS_target_within_5_pct, - n_iter_test_2, - Phi_type) %>% 
    mutate(cond_satisfied_lag_1 = lag(cond_satisfied)) %>%
    # filter(cond_satisfied == 1, cond_satisfied_lag_1 == 1) %>% # optional
    filter(cond_satisfied == 1 ) %>%
    group_by(N)  %>%
    filter(min_ESS_target_test == min(min_ESS_target_test)) %>% 
    print(n = 100)
  
  
  
  {
    print(paste("max_IQR_limit = ", max_IQR_limit ))
    print(paste("max_range_limit = ", max_range_limit ))
    print(paste("inc = ", inc ))
    }
  
}




